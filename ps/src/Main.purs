module Main where

import Prelude
import Data.Maybe          (maybe)
import Effect              (Effect)
import Effect.Aff          (launchAff_)
import Effect.Class        (liftEffect)
import Halogen.Aff         as HA
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode  (QuerySelector(..))

import Site.Api.Contact (contactForm)

main :: Effect Unit
main =  launchAff_ $ do
    rootContact <- HA.selectElement $ QuerySelector "#root-contact"
    maybe (pure unit)
          (liftEffect <<< HA.runHalogenAff <<< runUI contactForm unit)
          rootContact
