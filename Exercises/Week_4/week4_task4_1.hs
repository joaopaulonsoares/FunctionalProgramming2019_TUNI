{--
Using the data keyword, define PhoneType type that has constructors for values WorkLandline, PrivateMobile, WorkMobile, and Other.
Derive instance for Show, Eq and Read for it.

Using the type keyword, define two type synonyms for Integer: CountryCode and PhoneNo.

Then, using the record syntax, define Phone type for phone numbers that has only one value constructor with fields for
phoneType :: PhoneType,
countryCode :: CountryCode, (just a type synonym for Integer)
and phoneNo :: PhoneNo. (just a type synonym for Integer)
Derive instances for Show, Eq and Read for it.

Make a function of type
:: PhoneType
-> CountryCode (just a type synonym for Integer)
-> PhoneNo (just a type synonym for Integer)
-> Phone
that throws an error if CountryCode or PhoneNo is a negative integer and otherwise creates a value of type Phone with the given values.

Note: It is not necessarily recommended to pollute the namespace with all kinds of type synonyms, but they can be useful for example in case you wanted to change the value of CountryCode into String in this case. (You would get errors telling you where you need to change some behavior)
Note: You can still make the mistake of using a value of type CountryCode as PhoneNo.
--}