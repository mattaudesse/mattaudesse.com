module Site.Api.Contact
  ( Action
  , CommentBody
  , Email
  , FirstName
  , LastName
  , Phone
  , Request
  , VisitorMsgInvalid(..)
  , Website
  , contactForm
  , validRequest
  ) where

import Prelude

import Affjax.StatusCode         (StatusCode(..))
import Data.Array                (length, filter, (!!))
import Data.Array.NonEmpty       (NonEmptyArray)
import Data.Bifunctor            (bimap)
import Data.CodePoint.Unicode    (isSpace)
import Data.Const                (Const)
import Data.Either               (Either(..), isLeft, isRight)
import Data.Foldable             (all)
import Data.Generic.Rep          (class Generic)
import Data.Maybe                (Maybe(..), maybe)
import Data.Show.Generic         (genericShow)
import Data.String.CodePoints    (codePointFromChar)
import Data.String.CodeUnits     (toCharArray)
import Data.Validation.Semigroup (V, invalid, toEither)
import Effect.Aff                (Aff)
import Effect.Console            (info, error)

import Affjax                  as AX
import Data.String             as S
import Halogen                 as H
import Halogen.HTML            as HH
import Halogen.HTML.Events     as HE
import Halogen.HTML.Properties as HP

import Site.Component.Spinner (spinner)
import Site.HTTP              as HTTP


type FirstName   = String
type LastName    = String
type Email       = String
type Website     = String
type Phone       = String
type CommentBody = String

type Request =
  { firstName   :: FirstName
  , lastName    :: LastName
  , email       :: Email
  , website     :: Maybe Website
  , phone       :: Maybe Phone
  , commentBody :: CommentBody
  }

--------------------------------------------------------------------------------

data VisitorMsgInvalid
  = MissingFirstName
  | MissingLastName
  | InvalidEmailWhitespace
  | MissingEmailAccount
  | MissingEmailAt
  | InvalidEmailMultipleAtSigns
  | MissingOrInvalidEmailDomain
  | MissingCommentBody

derive instance genericVisitorMsgInvalid :: Generic VisitorMsgInvalid _
derive instance eqVisitorMsgInvalid      :: Eq      VisitorMsgInvalid

instance showVisitorMsgInvalid :: Show VisitorMsgInvalid where
  show = genericShow

type ValidatorOf a = a -> V (NonEmptyArray VisitorMsgInvalid) a

trimLen :: String -> Int
trimLen =  S.length <<< S.trim

missingFirstName :: ValidatorOf FirstName
missingFirstName n
  | trimLen n == 0 = invalid $ pure MissingFirstName
  | otherwise      = pure n

missingLastName :: ValidatorOf LastName
missingLastName n
  | trimLen n == 0 = invalid $ pure MissingLastName
  | otherwise      = pure n

invalidEmailWhitespace :: ValidatorOf Email
invalidEmailWhitespace e =
  let ws = filter (isSpace <<< codePointFromChar) $ toCharArray e
   in if length ws > 0 then invalid $ pure InvalidEmailWhitespace
                       else pure e

missingEmailAt :: ValidatorOf Email
missingEmailAt e | S.contains (S.Pattern "@") e = pure e
                 | otherwise                    = invalid $ pure MissingEmailAt

invalidEmailMultipleAtSigns :: ValidatorOf Email
invalidEmailMultipleAtSigns e =
  let ats = filter ((==) '@') $ toCharArray e
   in if length ats > 1 then invalid $ pure InvalidEmailMultipleAtSigns
                        else pure e

missingEmailAccount :: ValidatorOf Email
missingEmailAccount e =
  let parts = S.split (S.Pattern "@") e
      err   = invalid $ pure MissingEmailAccount
      nonEmpty a
        | trimLen a == 0 = err
        | otherwise      = pure a

   in case parts !! 0 of
        Nothing -> err
        Just a  -> nonEmpty a

missingOrInvalidEmailDomain :: ValidatorOf Email
missingOrInvalidEmailDomain e =
  let raw = (S.split (S.Pattern "@") e) !! 1
      err = invalid $ pure MissingOrInvalidEmailDomain

   in case raw of
      Nothing   -> err
      Just raw' ->
          let parts = S.split (S.Pattern ".") raw'
           in if length parts > 1 && all ((_ > 0) <<< trimLen) parts
                 then pure e
                 else err

validEmail :: ValidatorOf Email
validEmail e =
  bimap identity (const e) $ invalidEmailWhitespace      e
                          <* missingEmailAccount         e
                          <* missingEmailAt              e
                          <* invalidEmailMultipleAtSigns e
                          <* missingOrInvalidEmailDomain e

missingCommentBody :: ValidatorOf CommentBody
missingCommentBody b
  | trimLen b == 0 = invalid $ pure MissingCommentBody
  | otherwise      = pure b

validRequest :: ValidatorOf Request
validRequest { firstName, lastName, email, website, phone, commentBody } =
  { firstName:   _
  , lastName:    _
  , email:       _
  , website:     _
  , phone:       _
  , commentBody: _
  } <$> missingFirstName   firstName
    <*> missingLastName    lastName
    <*> validEmail         email
    <*> pure               website
    <*> pure               phone
    <*> missingCommentBody commentBody


--------------------------------------------------------------------------------

type State =
  { sent    :: Boolean
  , sending :: Boolean
  , req     :: Request
  }


data Action
  = UpdateFirstName      String
  | UpdateLastName       String
  | UpdateEmail          String
  | UpdateWebsite        String
  | UpdatePhone          String
  | UpdateCommentBody    String
  | SubmitVisitorMessage


handleAction
  :: forall slots
   . Action
  -> H.HalogenM State Action slots Void Aff Unit
handleAction = case _ of
  UpdateFirstName s -> do
    H.modify_ (_ { req { firstName = s }})

  UpdateLastName s -> do
    H.modify_ (_ { req { lastName = s }})

  UpdateEmail s -> do
    H.modify_ (_ { req { email = s }})

  UpdateWebsite s -> do
    let s' = if s == "" then Nothing else Just s
    H.modify_ (_ { req { website = s' }})

  UpdatePhone s -> do
    let s' = if s == "" then Nothing else Just s
    H.modify_ (_ { req { phone = s' }})

  UpdateCommentBody s -> do
    H.modify_ (_ { req { commentBody = s }})

  SubmitVisitorMessage -> do
    req <- H.gets _.req

    let valid     = isRight $ toEither $ validRequest req
        msgPrefix = "POST /api/contact: "

    when valid $ do
      H.modify_ (_ { sending = true })

      res <- H.liftAff $ HTTP.post_ "/api/contact" req

      case res of
        Left err ->
          H.liftEffect $ error (msgPrefix <> AX.printError err)

        Right { status: { code: StatusCode code, text }} -> do
          H.liftEffect $ info (msgPrefix <> "HTTP " <> show code <> " " <> text)
          H.modify_ (_ { sending = false, sent = true })


--------------------------------------------------------------------------------

offsetCol :: forall a b. Int -> HH.HTML a b
offsetCol n =
  let cls = "col-" <> show n
   in HH.div [ HP.class_ (HH.ClassName cls) ] []


renderForm :: forall m. State -> H.ComponentHTML Action () m
renderForm s = HH.div_
  [ HH.div [ HP.class_ (HH.ClassName "row") ]
           [ offsetCol 2
           , HH.div [ HP.class_ (HH.ClassName "col-10")          ]
                    [ HH.h1_    [ HH.text "Hey, how's it going?" ]]]

  , mkInput true  HP.InputText  "first-name" "First name:"   s.req.firstName UpdateFirstName [ HP.autofocus true ]
  , mkInput true  HP.InputText  "last-name"  "Last name:"    s.req.lastName  UpdateLastName  []
  , mkInput true  HP.InputEmail "email"      "Email:"        s.req.email     UpdateEmail     []
  , mkInput false HP.InputUrl   "website"    "Website:"      websiteVal      UpdateWebsite   []
  , mkInput false HP.InputTel   "phone"      "Phone number:" phoneVal        UpdatePhone     []

  , HH.div [ HP.class_ (HH.ClassName "row") ]
           [ offsetCol 2
           , HH.div [ HP.class_ (HH.ClassName "col-3")    ]
                    [ HH.label  [ HP.for  "comment-body"  ]
                                [ HH.text "Your message:" ]]]

  , HH.div [ HP.class_ (HH.ClassName "row") ]
           [ offsetCol 2
           , HH.div [ HP.class_   (HH.ClassName "col-8") ]
                    [ HH.textarea [ HP.name         "comment-body"
                                  , HP.id           "comment-body"
                                  , HP.required     true
                                  , HE.onValueInput UpdateCommentBody
                                  ]]]

  , HH.div [ HP.class_ (HH.ClassName "row") ]
           [ offsetCol 5
           , HH.div [ HP.class_ (HH.ClassName "col-2") ]
                    [ HH.a [ HE.onClick \_ -> SubmitVisitorMessage
                           , HP.id      "btn-send"
                           , HP.classes sendButtonClasses
                           ]
                           [ HH.text "Send" ]]]
  ]

  where

  validationResult = toEither $ validRequest s.req

  sendButtonClasses = [ HH.ClassName "btn-green" ]
                   <> if isLeft validationResult
                        then [ HH.ClassName "disabled" ]
                        else mempty

  websiteVal = maybe "" identity s.req.website
  phoneVal   = maybe "" identity s.req.phone

  mkInput required type_ elName label val trigger extraProps =
      let inputProps = [ HP.name     elName
                       , HP.id       elName
                       , HP.value    val
                       , HP.type_    type_
                       , HP.required required
                       , HE.onValueInput trigger
                       ] <> extraProps

          labelCol  = HH.div [ HP.classes [ HH.ClassName "col-3"
                                          , HH.ClassName "right-if-wide-enough" ]]
                             [ HH.label  [HP.for elName] [HH.text label] ]

          inputCol  = HH.div [ HP.class_ (HH.ClassName "col-5") ]
                             [ HH.input inputProps ]

      in HH.div [ HP.class_ (HH.ClassName "row") ]
                [ offsetCol 2, labelCol, inputCol  ]



renderSending :: forall m. State -> H.ComponentHTML Action () m
renderSending _ = HH.div_
  [ HH.div [ HP.class_ (HH.ClassName "row") ]
           [ offsetCol 2
           , HH.div [ HP.classes [ HH.ClassName "col-8"
                                 , HH.ClassName "center" ]]
                    [ HH.h1_     [ HH.text "Sending..."  ]]]

  , HH.div [ HP.class_ (HH.ClassName "row") ] []

  , HH.div [ HP.class_ (HH.ClassName "row") ]
           [ offsetCol 4, spinner           ]]


renderSent :: forall m. State -> H.ComponentHTML Action () m
renderSent _ = HH.div_
  [ HH.div [ HP.class_ (HH.ClassName "row") ]
           [ offsetCol 2
           , HH.div [ HP.classes [ HH.ClassName "col-8"
                                 , HH.ClassName "center" ]]
                    [ HH.h1_     [ HH.text "Thanks!"  ]]]

  , HH.div [ HP.class_ (HH.ClassName "row") ] []
  , HH.div [ HP.class_ (HH.ClassName "row") ]
           [ offsetCol 2
           , HH.div [ HP.classes [ HH.ClassName "col-8"
                                 , HH.ClassName "center" ]]
                    [ HH.h4_     [ HH.text "I'll respond shortly" ]]]

  , HH.div [ HP.class_ (HH.ClassName "row") ] []
  , HH.div [ HP.class_ (HH.ClassName "row") ]
           [ offsetCol 2
           , HH.div [ HP.classes [ HH.ClassName "col-8"
                                 , HH.ClassName "center" ]]
                    [ HH.a [ HP.href "/" ]
                           [ HH.text "Return to homepage" ]]]]


render :: forall m. State -> H.ComponentHTML Action () m
render s
  | s.sent    = renderSent    s
  | s.sending = renderSending s
  | otherwise = renderForm    s


--------------------------------------------------------------------------------

contactForm :: H.Component (Const Void) Unit Void Aff
contactForm =  H.mkComponent
  { render
  , eval:         H.mkEval $ H.defaultEval { handleAction = handleAction }
  , initialState: const { sent:    false
                        , sending: false
                        , req:     { firstName:   ""
                                   , lastName:    ""
                                   , email:       ""
                                   , website:     Nothing
                                   , phone:       Nothing
                                   , commentBody: ""
                                   }}}
