{-#Language OverloadedStrings,DeriveGeneric,TypeFamilies,TypeOperators,DataKinds,TypeApplications#-}
module GetSetRec.Example where
import GetSetRec.Base
import GHC.TypeLits
import GetSetRec.ToJson
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Data.Text hiding (unpack)
import GetSetRec.Data

data Gender = Male|Female deriving (Generic,Show)
instance ToJSON Gender
instance FromJSON Gender

type User =  "name" :=: String :+ "age" :=: Int  :+ "balance" :=: Float :+ "gender" :=: Gender
user :: User
user =  val "mike" :+> val 23 :+> val 100.25 :+> val Male



test :: IO()
test = do
  {-get-}
  print $  get' @"age" @Int user--unpack $ trans @(Get' "age" Int User ) user
  print $ get' @"balance" @Float user--unpack $ trans @(Get' "name" String User) user
  print $ get' @"name" @String user --unpack $ trans @(Get' "balance" Float User) user
  print $ get' @"gender" @Gender user--unpack $ trans @(Get' "gender" Gender User) user
  {-set-}
  print $ set' @"age" @Int (+1) user --trans @(Set' "age" Int User) (user :=> set (+1))
  print $ set' @"name" @String (++"-kir") user--trans @(Set' "name" String User) (user :=> set (++"-alexander"))
  print $ set' @"balance" @Float (*3) user --trans @(Set' "balance" Float User) (user :=> set (*2))
  print $ set' @"gender" @Gender (const Female) user --trans @(Set' "gender" Gender User) (user :=> set (const Female))
  {-encoding-decoding-}
  print $ encode user
  --(eitherDecodeFileStrict "example.json" :: IO (Either String User)) >>= print
