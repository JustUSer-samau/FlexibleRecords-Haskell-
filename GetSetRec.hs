{-#Language FunctionalDependencies,TypeApplications,UndecidableInstances,ScopedTypeVariables,AllowAmbiguousTypes,FlexibleInstances,MultiParamTypeClasses,PolyKinds,TypeFamilies,TypeOperators,DataKinds,GADTs,KindSignatures#-}
module GetSetRec where
import GHC.TypeLits
import Data.Type.Equality hiding (trans)
import Data.Proxy
import Data.Kind
import Data.Monoid
{-:+-}
data a :+ b = a :+> b deriving Show
infixr 2 :+
infixr 2 :+>


data Skip a = Skip {getSkip :: a}
instance Semigroup (Skip a) where
  _ <> a = a

data k :=: v = SField {getSField :: Maybe (Skip v)}
instance (KnownSymbol k,Show v)=>Show (k :=: v) where
  show k = (symbolVal @k Proxy)++" :=: "++(maybe "EMPTY" show $ ( fmap getSkip  . getSField) k)


instance Semigroup (k :=: v) where
  (SField a) <> (SField b) = SField (a <> b)
instance Monoid (k :=: v) where
  mempty = SField Nothing
instance Functor ((:=:) k) where
  fmap f (SField k) = SField $ fmap Skip $ fmap f $ fmap getSkip k

sval :: v -> k :=: v
sval v = SField (Just (Skip v))

unpack :: k :=: v -> Maybe v
unpack = fmap getSkip . getSField

{-trans-}
class Trans k a b|k -> a b where
  trans :: a -> b
instance (Trans k a b,Trans g b c)=>Trans (k :+> g) a c where
  trans = (trans @g @b @c) . (trans @k @a @b)

{-mono-key-}
data MonoidKey :: Type -> Type ->  Type where
  MemptyKey :: m -> a -> v -> MonoidKey m v
  SemiKey :: m -> a -> v -> MonoidKey m v
  (:<>) :: MonoidKey m a -> MonoidKey m a -> m -> MonoidKey m  a

{-get-}
data Get = Get

instance Monoid v=>Trans (MemptyKey Get a v ) a v where
  trans _ = mempty
instance Trans (SemiKey Get v v) v v where
  trans v = v
instance (Semigroup v,Trans k a v,Trans g b v)=>Trans ((k :<> g) Get) (a :+ b) v where
  trans (a :+> b) = (trans @k a) <> (trans @g b)

type Get' k v t = GenKey Get (k :=: v) t
{-gen-keys-}
type family If (c :: Bool) (a :: k) (b :: k) :: k where
  If True a b = a
  If False a b = b


type family GenKey (k :: m) v t :: MonoidKey m Type where
  GenKey k v (a :+ b) = ((GenKey k v a) :<> (GenKey k v b)) k
  GenKey k v a = If (v == a) (SemiKey k a v) (MemptyKey k a v)


{-set-}
data WithSet a v = a :=> (v -> v)
data Set = Set

instance Trans (MemptyKey Set a v) (WithSet a v) a where
  trans (a :=> f) = a
instance Trans (SemiKey Set a a) (WithSet a a) a where
  trans (a :=> f) = f a
instance (Trans k (WithSet a v) a,Trans g (WithSet b v) b)=>Trans ((k :<> g) Set) (WithSet (a :+ b) v) (a :+ b) where
  trans ((a :+> b) :=> f) = (trans @k (a :=> f)) :+> (trans @g (b :=> f))

type Set' k v t = GenKey Set (k :=: v) t

set :: (v -> v) -> (k :=: v ->  k :=: v )
set f = fmap f
