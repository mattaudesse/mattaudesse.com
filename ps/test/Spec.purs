module Spec where

import Prelude
import Effect                  (Effect)
import Test.Spec.Reporter.Spec (specReporter)
import Test.Spec.Runner        (run)

import Site.Api.ContactSpec (contactValidation)

main :: Effect Unit
main =  run [specReporter] do
    contactValidation
