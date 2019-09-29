module W5T2DataDefinitions
    ( PhoneType (..),
      CountryCode,
      PhoneNo,
      Phone
    ) where

data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving(Show, Read, Eq)

newtype CountryCode  = MakeCountryCode Integer deriving(Eq,Read)
newtype PhoneNo      = MakePhoneNo Integer deriving(Eq,Read)
data Phone        = MakePhone { phoneType   :: Maybe PhoneType
                               ,countryCode :: Maybe CountryCode
                               ,phoneNo     :: PhoneNo    
                                } deriving(Eq, Read)