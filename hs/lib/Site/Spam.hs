{-# LANGUAGE OverloadedStrings #-}
module Site.Spam
    ( spamEmails
    , spamIps
    ) where

import Data.List.NonEmpty (NonEmpty(..), nub, sort)
import Site.Core          (Email, VisitorIpAddr)

spamEmails :: NonEmpty Email
spamEmails =  sort $ nub
    $ "noreply@profunder247.com" :|
    [ "alexis0p45thill@yahoo.com"
    , "aubreyh@snacknationnow.com"
    , "bhelmer@ebdgroup.com"
    , "caron_mccarey37@rambler.ru"
    , "karendavis1972@yahoo.com"
    , "makaylaxs6m5lewis@yahoo.com"
    , "marydavis2nd@gmail.com"
    , "maryelizabeth2nd@gmail.com"
    , "noreply@business-funding-247.com"
    , "noreply@business-funding-365.com"
    , "noreply@businessloansfunded.com"
    , "noreply@getabusinessloan365.com"
    , "noreply@getbusinessfunded.com"
    , "noreply@get-my-business-funded.com"
    ]

spamIps :: NonEmpty VisitorIpAddr
spamIps =  sort $ nub
    $ "91.236.75.22" :|
    [ "193.169.252.17"
    , "193.169.252.46"
    ]
