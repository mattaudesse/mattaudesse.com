module Spec where

import Prelude
import Effect                  (Effect)
import Effect.Aff              (launchAff_)
import Test.Spec.Reporter.Spec (specReporter)
import Test.Spec.Runner        (runSpec)

import Site.Api.ContactSpec (contactValidation)

main :: Effect Unit
main =  launchAff_ $ runSpec [specReporter] do
  contactValidation
