{-|
Module      : Cascade.Data.Validation
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Cascade.Data.Validation
  ( Validatable(..)
  , GenericValidationErrors
  , FieldValidationError
  , Validity(Parsed, Raw)
  , Validate
  , ApiErrorFormat(..)
  , ToMessage(..)
  , module Validation
  ) where

import           Cascade.Polysemy               ( constraint )
import           Cascade.Type.Monoid
import qualified Data.Aeson                    as Aeson
import           Data.Aeson                     ( (.=) )
import qualified Data.Aeson.Types              as Aeson
import           Data.Data
import qualified Data.TMap                     as TMap
import           Data.TMap                      ( TMap )
import qualified Data.Text                     as Text
import           GHC.Generics
import           GHC.TypeLits
import           Polysemy                       ( EffectRow
                                                , Members
                                                , Sem
                                                )
import           Unsafe.Coerce
import           Validation

class Validatable (a :: Type) where
  type Raw a :: Type

  type Parsed a :: Type
  type Parsed a = a

  type Errors a :: Type

  type Effects a :: EffectRow
  type Effects a = '[]

  validate :: Raw a -> Sem (Effects a) (Validation (Errors a) (Parsed a))

newtype GenericValidationErrors (errors :: [Type]) = GenericValidationErrors TMap
  deriving stock Show
  deriving newtype (Semigroup, Monoid)

instance GenericValidationErrorsToJSON errors => Aeson.ToJSON (GenericValidationErrors errors) where
  toJSON errors = Aeson.object $ genericValidationErrorsToJSON errors

class GenericValidationErrorsToJSON (errors :: [Type]) where
  genericValidationErrorsToJSON :: GenericValidationErrors errors -> [Aeson.Pair]

instance GenericValidationErrorsToJSON '[] where
  genericValidationErrorsToJSON _ = []

instance ( Aeson.ToJSON error
         , KnownSymbol fieldName
         , GenericValidationErrorsToJSON errors
         , Typeable error
         ) => GenericValidationErrorsToJSON (FieldValidationError fieldName error ': errors) where
  genericValidationErrorsToJSON (GenericValidationErrors tmap) =
    case TMap.lookup @(FieldValidationError fieldName error) tmap of
      Nothing -> next
      Just (FieldValidationError e) ->
        (Text.pack $ symbolVal (Proxy @fieldName), Aeson.toJSON e) : next
   where
    next = genericValidationErrorsToJSON (GenericValidationErrors @errors tmap)

newtype FieldValidationError (fieldName :: Symbol) (error :: Type) = FieldValidationError error

instance GenericValidatableConstraints a => Validatable (Generically (a (v :: Validity))) where
  type Raw (Generically (a _)) = a 'Raw
  type Parsed (Generically (a _)) = a 'Parsed
  type Errors (Generically (a _))
    = GenericValidationErrors (GenericFieldValidationErrors (Rep (a 'Mark)))
  type Effects (Generically (a _)) = GenericEffects (Rep (a 'Mark))

  validate =
    (fmap . fmap) (unsafeCoerce @(a ( 'MarkR 'Parsed)) . to)
      . genericValidate
      . from
      . unsafeCoerce @_ @(a ( 'MarkR 'Raw))

class GenericValidatable (raw :: Type -> Type) (parsed :: Type -> Type) (errors :: [Type]) (effects :: EffectRow) where
  genericValidate :: raw p -> Sem effects (Validation (GenericValidationErrors errors) (parsed p))

instance
  ( KnownSymbol fieldName
  , Validatable a
  , Typeable (Errors a)
  , effects ~ Effects a
  ) => GenericValidatable (S1 ('MetaSel ('Just fieldName) _t1 _t2 _t3) (Rec0 (MarkedR a 'Raw)))
                    (S1 ('MetaSel ('Just fieldName) _t1 _t2 _t3) (Rec0 (MarkedR a 'Parsed)))
                    _errors
                    effects
  where
  genericValidate (M1 (K1 (MarkedR x))) = validate @a x <&> bimap
    (GenericValidationErrors . TMap.one . FieldValidationError @fieldName)
    (M1 . K1 . MarkedR)

instance
  ( GenericValidatable a c errors effects1
  , GenericValidatable b d errors effects2
  , Members effects1 effects3
  , Members effects2 effects3
  ) => GenericValidatable (a :*: b) (c :*: d) errors effects3 where
  genericValidate (l :*: r) = do
    lresult <- constraint $ genericValidate @a @c @errors @effects1 l
    rresult <- constraint $ genericValidate @b @d @errors @effects2 r
    pure $ (:*:) <$> lresult <*> rresult

instance (GenericValidatable a b errors effects) => GenericValidatable (D1 _m1 a) (D1 _m2 b) errors effects where
  genericValidate (M1 x) = fmap M1 <$> genericValidate x

instance (GenericValidatable a b errors effects) => GenericValidatable (C1 _m1 a) (C1 _m2 b) errors effects where
  genericValidate (M1 x) = fmap M1 <$> genericValidate x

instance {-# OVERLAPPABLE #-} (GenericValidatable a b errors effects) => GenericValidatable (S1 _m1 a) (S1 _m2 b) errors effects where
  genericValidate (M1 x) = fmap M1 <$> genericValidate x

instance {-# OVERLAPPABLE #-} GenericValidatable (Rec0 a) (Rec0 a) errors effects where
  genericValidate (K1 x) = pure $ pure (K1 x)

data Validity = Raw | Parsed | Mark | MarkR Validity

newtype Marked (a :: Type) = Marked a
newtype MarkedR (a :: Type) (v :: Validity) = MarkedR (Validate v a)

type family Validate (v :: Validity) (a :: Type) where
  Validate 'Raw a    = Raw a
  Validate 'Parsed a = Parsed a
  Validate 'Mark a = Marked a
  Validate ('MarkR v) a = MarkedR a v

type family ValidatableFieldsAList' (a :: Type -> Type) (r :: [(Symbol, Type)]) :: [(Symbol, Type)] where
  ValidatableFieldsAList' (S1 ('MetaSel ('Just fieldName) _ _ _) (Rec0 (Marked a))) r = '(fieldName, a) ': r
  ValidatableFieldsAList' (S1 _ _)   r = r
  ValidatableFieldsAList' (a :*: b)  r = ValidatableFieldsAList' a '[] <> ValidatableFieldsAList' b '[] <> r
  ValidatableFieldsAList' (D1 _ rep) r = ValidatableFieldsAList' rep r
  ValidatableFieldsAList' (C1 _ rep) r = ValidatableFieldsAList' rep r

type ValidatableFieldsAList (a :: Type -> Type) = ValidatableFieldsAList' a '[]

type family ToFieldValidationErrors (a :: (Symbol, Type)) :: Type where
  ToFieldValidationErrors '(fieldName, a) = FieldValidationError fieldName (Errors a)

type family GenericFieldValidationErrors' (as :: [(Symbol, Type)]) :: [Type] where
  GenericFieldValidationErrors' '[] = '[]
  GenericFieldValidationErrors' ('(fieldName, a) ': as) = FieldValidationError fieldName (Errors a) ': GenericFieldValidationErrors' as

type GenericFieldValidationErrors a
  = GenericFieldValidationErrors' (ValidatableFieldsAList a)

type family GenericEffects' (as :: [(Symbol, Type)]) :: EffectRow where
  GenericEffects' '[] = '[]
  GenericEffects' ('(_, a) ': as) = Effects a <> GenericEffects' as

type GenericEffects a = GenericEffects' (ValidatableFieldsAList a)

type GenericValidatableConstraints (a :: Validity -> Type)
  = ( Generic (a ( 'MarkR 'Raw))
    , Generic (a ( 'MarkR 'Parsed))
    , GenericValidatable
        (Rep (a ( 'MarkR 'Raw)))
        (Rep (a ( 'MarkR 'Parsed)))
        (GenericFieldValidationErrors (Rep (a 'Mark)))
        (GenericEffects (Rep (a 'Mark)))
    )

newtype ApiErrorFormat (error :: Type) = ApiErrorFormat error

instance (Data error, ToMessage error) => Aeson.ToJSON (ApiErrorFormat error) where
  toJSON (ApiErrorFormat e) = Aeson.object
    ["tag" .= (show @String $ toConstr e), "message" .= toMessage e]

class ToMessage (error :: Type) where
  toMessage :: error -> Text
