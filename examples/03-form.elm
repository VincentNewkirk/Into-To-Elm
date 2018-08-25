import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , age: String
  }


init : Model
init =
  Model "" "" "" ""



-- UPDATE


type Msg
  = Name String
  | Password String
  | PasswordAgain String
  | Age String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }

    Age age ->
      { model | age = age }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    , viewInput "text" "Age" model.age Age
    , viewValidation model
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
  if isInvalidAge model.age then
    div [style "color" "red" ] [ text "Invalid Age" ]
  else if isPasswordTooLong model.password then
    div [style "color" "red" ] [ text "Password Too Long" ]
  else if passwordsDontMatch model.password model.passwordAgain then
    div [ style "color" "red" ] [ text "Passwords do not match!" ]
  else
    div [ style "color" "green" ] [ text "OK" ]


-- FUNCTIONS

isInvalidAge a = String.toInt a == Nothing

isPasswordTooLong p = String.length p > 8

passwordsDontMatch one two = one /= two
