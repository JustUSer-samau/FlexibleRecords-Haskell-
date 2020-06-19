{-#Language TypeFamilies,TypeOperators,DataKinds,TypeApplications#-}
import GetSetRec
import GHC.TypeLits

data Gender = Male|Female deriving Show

type AgeF = "age" :=: Int
type NameF = "name" :=: String
type BalanceF = "balance" :=: Float
type GenderF = "gender" :=: Gender

type User = NameF :+ AgeF :+ BalanceF :+ GenderF
user :: User
user = sval "mike" :+> sval 23 :+> sval 100.0 :+> sval Male



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
  
{-
 --result 
 
Just 23
Just "mike"
Just 100.0
Just Male
name :=: "mike" :+> (age :=: 24 :+> (balance :=: 100.0 :+> gender :=: Male))
name :=: "mike-alexander" :+> (age :=: 23 :+> (balance :=: 100.0 :+> gender :=: Male))
name :=: "mike" :+> (age :=: 23 :+> (balance :=: 200.0 :+> gender :=: Male))
name :=: "mike" :+> (age :=: 23 :+> (balance :=: 100.0 :+> gender :=: Female))

-} 
 
