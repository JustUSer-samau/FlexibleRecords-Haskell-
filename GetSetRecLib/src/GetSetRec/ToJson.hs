{-#Language TupleSections,ScopedTypeVariables,TypeApplications,TypeFamilies,TypeOperators,DataKinds#-}
module GetSetRec.ToJson where
import GetSetRec.Data
import GetSetRec.Base ((:+)(..))
import GHC.TypeLits
import Data.Proxy
import Data.Aeson
import Data.Aeson.Types
import Data.HashMap.Strict
import Data.Text hiding (unpack )
import Prelude hiding (lookup)




empty :: forall k v.(k :=: v)
empty =mempty

cutKeyJson :: Text -> Object -> Maybe (Value,Object)
cutKeyJson key hash = fmap (\x -> (x,delete key hash)) (lookup key hash)

parseObj :: Value -> Parser Object
parseObj (Object o) = return o
parseObj _ = fail "not object"
{-toJson-}

class ToJsonObj a where
  toJsonObj :: a -> Object

instance (KnownSymbol k,ToJSON v)=>ToJsonObj (k :=: v) where
  toJsonObj k= fromList [(pack $ symbolVal @k Proxy,toJSON $ unpack k)]
instance (ToJsonObj a,ToJsonObj b)=>ToJsonObj (a :+ b) where
  toJsonObj (a :+> b)= fromList $ (toList (toJsonObj a) ++ toList (toJsonObj b))


instance (ToJSON a)=>ToJSON (FVal a) where
  toJSON (Val a) = toJSON a
  toJSON None = Null
  toJSON Fail = Null
instance (ToJsonObj a,ToJsonObj b)=>ToJSON (a :+ b) where
  toJSON = Object . toJsonObj
instance (KnownSymbol k,ToJSON v)=>ToJSON (k :=: v) where
  toJSON = Object . toJsonObj


{-fromJson-}

class FromObjectPartially a where
  fromObjPart  :: Object -> Parser (a,Object)

instance (KnownSymbol k,FromJSON v)=>FromObjectPartially (k :=: v) where
  fromObjPart obj= do
    let key' = symbolVal @k Proxy
    case (cutKeyJson (pack key') obj) of
      Nothing -> fail $ "key "++key'++" is not in object"
      Just (v,o) -> do
        v' <- parseJSON v :: Parser v
        return (val v',o)

instance (FromObjectPartially a,FromObjectPartially b)=>FromObjectPartially (a :+ b) where
  fromObjPart obj= do
    (a,o) <- fromObjPart @a obj
    (b,o') <- fromObjPart @b o
    return (a :+> b,o')


data FromJsonObj a = FromJson {getFromJsonObj :: a}
instance (FromObjectPartially a)=>FromJSON (FromJsonObj a) where
  parseJSON value = do
    obj <- parseObj value
    (r,l) <- fromObjPart @a obj
    if (toList l) == [] then
      return (FromJson r) else
        fail "there are some keys left"

instance (FromObjectPartially a,FromObjectPartially b)=>FromJSON (a :+ b) where
  parseJSON val = fmap getFromJsonObj $ parseJSON val

instance  (KnownSymbol k,FromJSON v)=>FromJSON (k :=: v) where
  parseJSON val = fmap getFromJsonObj $ parseJSON val
