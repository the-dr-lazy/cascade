module Cascade.Data.Api.Prelude where

data family Readable (a :: Type)

data family Creatable (a :: Type)

data family Updatable (a :: Type)

data family Deletable (a :: Type)
