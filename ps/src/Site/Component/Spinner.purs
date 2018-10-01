module Site.Component.Spinner (spinner) where

import Prelude
import Halogen.HTML            as HH
import Halogen.HTML.Properties as HP

spinner :: forall a b. HH.HTML a b
spinner =
    HH.div [ HP.class_ (HH.ClassName "sk-folding-cube-container") ]
           [ mkCube 1, mkCube 2, mkCube 4, mkCube 3 ]

    where mkCube n =
            HH.div [ HP.classes [ HH.ClassName $ "sk-cube-" <> show n
                                , HH.ClassName   "sk-cube" ]]
                   []
