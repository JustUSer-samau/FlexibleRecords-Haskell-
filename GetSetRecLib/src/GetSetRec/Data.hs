{-#Language AllowAmbiguousTypes,FlexibleContexts,ScopedTypeVariables,TypeApplications,DeriveFunctor,TypeFamilies,DataKinds,TypeOperators,PolyKinds#-}
module GetSetRec.Data where
import GetSetRec.Base
import GHC.TypeLits
import Data.Proxy

data FVal a = None|Fail|Val a deriving (Show,Functor)
data k :=: v = Field {getField :: FVal v} deriving Functor

instance (KnownSymbol k,Show v)=>Show (k :=: v) where
  show k = (symbolVal @k Proxy)++" :=: "++v where
    v = case (getField k) of
      Val a -> show a
      None -> "None"
      Fail -> "Fail"


instance Semigroup (k :=: v) where
  (Field a) <> (Field b) = Field (a <> b)
instance Monoid (k :=: v) where
  mempty = Field $ None

instance Semigroup (FVal a) where
  (Val a) <> (Val b) = Fail
  Fail <> a = Fail
  a <> Fail = Fail
  None <> a = a
  a <> None = a

val :: v -> k :=: v
val v = Field $ Val v

unpack :: k :=: v -> FVal v
unpack = getField

{-get-set-}
set :: (v -> v) -> (k :=: v -> k :=: v)
set f = fmap f

type Get' k v t = GenKey Get (k :=: v) t
type Set' k v t = GenKey Set (k :=: v) t

get' :: forall k v t.Trans (Get' k v t) t (k :=: v) =>t -> FVal v
get' st = unpack $ trans @(Get' k v t) st

set' :: forall k v t.Trans (Set' k v t) (WithSet t (k :=: v)) t=>(v -> v) -> t -> t
set' f st = trans @(Set' k v t) (st :=> set f)
