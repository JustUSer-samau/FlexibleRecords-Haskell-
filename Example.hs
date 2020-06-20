{-#Language OverloadedStrings,DeriveGeneric,TypeFamilies,TypeOperators,DataKinds,TypeApplications#-}
import GetSetRec_Base
import GHC.TypeLits
import GetSetRec_ToJson
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Data.Text hiding (unpack)
import GetSetRec_Data

data Gender = Male|Female deriving (Generic,Show)
instance ToJSON Gender
instance FromJSON Gender

type User =  "name" :=: String :+ "age" :=: Int  :+ "balance" :=: Float :+ "gender" :=: Gender
user :: User
user =  val "mike" :+> val 23 :+> val 100.25 :+> val Male



main :: IO()
main = do
  {-get-}
  print $  unpack $ trans @(Get' "age" Int User ) user
  print $ unpack $ trans @(Get' "name" String User) user
  print $ unpack $ trans @(Get' "balance" Float User) user
  print $ unpack $ trans @(Get' "gender" Gender User) user
  {-set-}
  print $ trans @(Set' "age" Int User) (user :=> set (+1))
  print $ trans @(Set' "name" String User) (user :=> set (++"-alexander"))
  print $ trans @(Set' "balance" Float User) (user :=> set (*2))
  print $ trans @(Set' "gender" Gender User) (user :=> set (const Female))
  {-encoding-decoding-}
  print $ encode user
  (eitherDecodeFileStrict "example.json" :: IO (Either String User)) >>= print
