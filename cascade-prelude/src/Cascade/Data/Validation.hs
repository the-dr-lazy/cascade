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
  , validate
  , GenericValidationErrors
  , FieldValidationError
  , Phase(Parsed, Raw)
  , Validate
  , ApiErrorFormat(..)
  , ToMessage(..)
  , module Validation
  ) where

import           Cascade.Type.Monoid
import qualified Data.Aeson                         as Aeson
import           Data.Aeson                          ( (.=) )
import qualified Data.Aeson.Types                   as Aeson
import           Data.Data
import qualified Data.TMap                          as TMap
import           Data.TMap                           ( TMap )
import qualified Data.Text                          as Text
import           GHC.Generics
import           GHC.TypeLits
import           Polysemy                            ( EffectRow
                                                     , Sem
                                                     )
import           Unsafe.Coerce
import           Validation

type family Parsed (a :: Type) :: Type where
  Parsed (Generically a) = a
  Parsed a               = a

class Validatable (raw :: Type) (parsed :: Type) where
  type Errors raw parsed :: Type

  type Effects raw parsed :: EffectRow
  type Effects raw parsed = '[]

  parse :: raw -> Sem (Effects raw parsed) (Validation (Errors raw parsed) (Parsed parsed))

validate :: forall raw parsed
          . Validatable raw parsed
         => parsed ~ Parsed parsed => raw -> Sem (Effects raw parsed) (Validation (Errors raw parsed) parsed)
validate = parse @raw @parsed

type GenericValidatableConstraints (a :: Phase -> Type)
  = ( Generic (a ( 'MarkR 'Raw))
    , Generic (a ( 'MarkR 'Parsed))
    , GenericValidatable
        (Rep (a ( 'MarkR 'Raw)))
        (Rep (a ( 'MarkR 'Parsed)))
        (GenericFieldValidationErrors (Rep (a 'Mark)))
        (GenericEffects (Rep (a 'Mark)))
    )

instance GenericValidatableConstraints a => Validatable (a 'Raw) (Generically (a 'Parsed)) where
  type Errors (a 'Raw) (Generically (a 'Parsed)) = GenericValidationErrors (GenericFieldValidationErrors (Rep (a 'Mark)))
  type Effects (a 'Raw) (Generically (a 'Parsed)) = GenericEffects (Rep (a 'Mark))

  parse = (fmap . fmap) (unsafeCoerce @(a ( 'MarkR 'Parsed)) . to) . genericParse . from . unsafeCoerce @_ @(a ( 'MarkR 'Raw))

class GenericValidatable (raw :: Type -> Type) (parsed :: Type -> Type) (errors :: [Type]) (effects :: EffectRow) where
  genericParse :: raw p -> Sem effects (Validation (GenericValidationErrors errors) (parsed p))

instance
  ( KnownSymbol fieldName
  , Validatable raw parsed
  , Typeable (Errors raw parsed)
  , effects ~ Effects raw parsed
  , parsed ~ Parsed parsed
  ) => GenericValidatable (S1 ('MetaSel ('Just fieldName) _t1 _t2 _t3) (Rec0 (MarkedR raw parsed 'Raw)))
                    (S1 ('MetaSel ('Just fieldName) _t1 _t2 _t3) (Rec0 (MarkedR raw parsed 'Parsed)))
                    _errors
                    effects
  where
  genericParse (M1 (K1 (MarkedR x))) =
    parse @raw @parsed x <&> bimap (GenericValidationErrors . TMap.one . FieldValidationError @fieldName) (M1 . K1 . MarkedR)

instance
  ( GenericValidatable a c errors effects1
  , GenericValidatable b d errors effects2
  , effects4 ~ (effects1 <> effects2)
  ) => GenericValidatable (a :*: b) (c :*: d) errors effects3 where
  genericParse (l :*: r) = do
    lresult <- unsafeCoerce $ genericParse @a @c @errors @effects1 l
    rresult <- unsafeCoerce $ genericParse @b @d @errors @effects2 r
    pure $ (:*:) <$> lresult <*> rresult

instance (GenericValidatable a b errors effects) => GenericValidatable (D1 _m1 a) (D1 _m2 b) errors effects where
  genericParse (M1 x) = fmap M1 <$> genericParse x

instance (GenericValidatable a b errors effects) => GenericValidatable (C1 _m1 a) (C1 _m2 b) errors effects where
  genericParse (M1 x) = fmap M1 <$> genericParse x

instance {-# OVERLAPPABLE #-} (GenericValidatable a b errors effects) => GenericValidatable (S1 _m1 a) (S1 _m2 b) errors effects where
  genericParse (M1 x) = fmap M1 <$> genericParse x

instance {-# OVERLAPPABLE #-} GenericValidatable (Rec0 a) (Rec0 a) errors effects where
  genericParse (K1 x) = pure $ pure (K1 x)

data Phase = Raw | Parsed | Mark | MarkR Phase

newtype Marked (raw :: Type) (parsed :: Type) = Marked ()
newtype MarkedR (raw :: Type) (parsed :: Type) (v :: Phase) = MarkedR (Validate v raw parsed)

type family Validate (v :: Phase) (raw :: Type) (parsed :: Type) where
  Validate 'Raw       raw _      = raw
  Validate 'Parsed    _   parsed = parsed
  Validate 'Mark      raw parsed = Marked raw parsed
  Validate ('MarkR v) raw parsed = MarkedR raw parsed v

type family ValidatableFieldsAList' (a :: Type -> Type) (r :: [(Symbol, Type, Type)]) :: [(Symbol, Type, Type)] where
  ValidatableFieldsAList' (S1 ('MetaSel ('Just fieldName) _ _ _) (Rec0 (Marked raw parsed))) r = '(fieldName, raw,parsed) ': r
  ValidatableFieldsAList' (S1 _ _)   r = r
  ValidatableFieldsAList' (a :*: b)  r = ValidatableFieldsAList' a '[] <> ValidatableFieldsAList' b '[] <> r
  ValidatableFieldsAList' (D1 _ rep) r = ValidatableFieldsAList' rep r
  ValidatableFieldsAList' (C1 _ rep) r = ValidatableFieldsAList' rep r

type ValidatableFieldsAList (a :: Type -> Type) = ValidatableFieldsAList' a '[]

type family GenericFieldValidationErrors' (as :: [(Symbol, Type, Type)]) :: [Type] where
  GenericFieldValidationErrors' '[] = '[]
  GenericFieldValidationErrors' ('(fieldName, raw, parsed) ': as) = FieldValidationError fieldName (Errors raw parsed) ': GenericFieldValidationErrors' as

type GenericFieldValidationErrors a = GenericFieldValidationErrors' (ValidatableFieldsAList a)

type family GenericEffects' (as :: [(Symbol, Type, Type)]) :: EffectRow where
  GenericEffects' '[] = '[]
  GenericEffects' ('(_, raw, parsed) ': as) = Effects raw parsed <> GenericEffects' as

type GenericEffects a = GenericEffects' (ValidatableFieldsAList a)


newtype ApiErrorFormat (error :: Type) = ApiErrorFormat error

instance (Data error, ToMessage error) => Aeson.ToJSON (ApiErrorFormat error) where
  toJSON (ApiErrorFormat e) = Aeson.object ["tag" .= (show @String $ toConstr e), "message" .= toMessage e]

class ToMessage (error :: Type) where
  toMessage :: error -> Text

newtype GenericValidationErrors (errors :: [Type]) = GenericValidationErrors TMap
  deriving stock Show
  deriving newtype (Semigroup, Monoid)

instance GenericValidationErrorsToJSON errors => Aeson.ToJSON (GenericValidationErrors errors) where
  toJSON errors = Aeson.object $ genericValidationErrorsToJSON errors

-- | An arbitrary instance just for Servant client satisfication.
-- DO NOT RELY ON IT
instance Aeson.FromJSON (GenericValidationErrors errors) where
  parseJSON _ = pure . GenericValidationErrors $ TMap.empty

class GenericValidationErrorsToJSON (errors :: [Type]) where
  genericValidationErrorsToJSON :: GenericValidationErrors errors -> [Aeson.Pair]

instance GenericValidationErrorsToJSON '[] where
  genericValidationErrorsToJSON _ = []

instance ( Aeson.ToJSON error
         , KnownSymbol fieldName
         , GenericValidationErrorsToJSON errors
         , Typeable error
         ) => GenericValidationErrorsToJSON (FieldValidationError fieldName error ': errors) where
  genericValidationErrorsToJSON (GenericValidationErrors tmap) = case TMap.lookup @(FieldValidationError fieldName error) tmap of
    Nothing                       -> next
    Just (FieldValidationError e) -> (Text.pack $ symbolVal (Proxy @fieldName), Aeson.toJSON e) : next
    where next = genericValidationErrorsToJSON (GenericValidationErrors @errors tmap)

newtype FieldValidationError (fieldName :: Symbol) (error :: Type) = FieldValidationError error
