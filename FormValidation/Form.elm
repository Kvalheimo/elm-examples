import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)

import Http
import Json.Encode as Encode
import Json.Decode as Json

import Validation exposing (..)

type alias Model =
  { email : Field String String
  , password : Field String String
  , confirmPassword : Field String String
  , acceptPolicy : Field Bool Bool
  , status : SubmissionStatus
  }

type SubmissionStatus
    = NotSubmitted
    | InProcess
    | Succeded
    | Failed

initialModel : Model
initialModel =
  { email = field ""
  , password = field ""
  , confirmPassword = field ""
  , acceptPolicy = field False
  , status = NotSubmitted
  }

type Msg
  = InputEmail String
  | InputPassword String
  | InputConfirmPassword String
  | Submit
  | CheckAcceptPolicy Bool
  | SubmitResponse (Result Http.Error () )

main : Program Never Model Msg
main =
  program
    { init = (initialModel, Cmd.none)
    , update = update
    , subscriptions = \model -> Sub.none
    , view = view
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        InputEmail e ->
            ({model | email = model.email 
                    |> validate (OnChange e) emailValidation
                    }, Cmd.none)
        InputPassword p ->
            let 
                password = model.password 
                        |> validate (OnChange p) passwordValidation
            in 
            ({model | password = model.password 
                        |> validate (OnChange p) passwordValidation
                    , confirmPassword = model.confirmPassword
                            |> validate
                                OnRelatedChange
                                (confirmPasswordValidation password)
            }, Cmd.none)
        InputConfirmPassword p ->
            ({model | confirmPassword = model.confirmPassword
                    |> validate
                     (OnChange p) 
                     (confirmPasswordValidation model.password)
            }, Cmd.none)
        CheckAcceptPolicy a ->
            ({model | acceptPolicy = field a}, Cmd.none)
        Submit ->
            model |> validateModel |> submitIfValid
        SubmitResponse (Ok ()) ->
            ({initialModel | status = Succeded }, Cmd.none)
        SubmitResponse (Err _) ->
            ({model | status = Failed}, Cmd.none)
        
emailValidation = 
  isNotEmpty "An email is required"
  >=> isEmail "Please ensure this is a valid email"
passwordValidation = 
    isNotEmpty "Please enter a password"
    >=> isStrongPassword

confirmPasswordValidation password = 
    isNotEmpty "Please retype your password"
    >=> isEqualTo password"the passwords don't match" 

validateModel : Model -> Model 
validateModel model = 
  let

    email = model.email |> validate OnSubmit emailValidation



    password = model.password |> validate OnSubmit passwordValidation



    confirmPassword = model.confirmPassword 
        |> validate OnSubmit (confirmPasswordValidation password)

    acceptPolicy = model.acceptPolicy
      |> validate OnSubmit (isTrue " You must accept the policy")


  in
      {model | email = email
      , password = password
      , confirmPassword = confirmPassword
      , acceptPolicy = acceptPolicy
    }

isStrongPassword p = 
    if String.length p >= 6 then Ok p
    else Err "YOur password isnt strong enough"

submitIfValid : Model -> (Model, Cmd Msg)
submitIfValid model =
  let
    submissionResult = 
      Valid submit
        |: (validity model.email)
        |: (validity model.password)
        |: (validity model.confirmPassword)
        |: (validity model.acceptPolicy)
    
  in case submissionResult of
    Valid cmd ->
      ({model | status = InProcess}, cmd)
    _ ->
      (model, Cmd.none)
            
submit : String -> String -> String -> Bool ->  Cmd Msg
submit email password _ _ =
    let
        url = "http://localhost:3000/api/contact"

        json = Encode.object
            [ ("email", Encode.string email)
            , ("password", Encode.string password)
            ]

        decoder = Json.string |> Json.map (always () )


        request = Http.post url (Http.jsonBody json) decoder

    in request |> Http.send SubmitResponse

view : Model -> Html Msg
view model =
  Html.form 
    [ onSubmit Submit
    , novalidate True
    ]
    [ header model
    , body model
    , footer model
    , div [] [ model |> toString |> text ]
    ]

header  model = div []
  [ h1 [] [ text "Register" ] 
  , renderStatus model.status]

renderStatus status = 
    case status of
        NotSubmitted ->
            div [] [] 
        InProcess ->
            div [] [text "Your request is being sent"]
        Succeded ->
            div [] [text "Your request has been received"]
        Failed ->
            div [class "alert alert-danger"] 
            [ text "Ops! There was an error, please try again"]

errorLabel : Field raw a -> Html Msg
errorLabel field = label 
      [ class "label lable-error" ]
      [ field 
        |> extractError 
        |> Maybe.withDefault "" 
        |> text
      ]

body model = div []
  [ div []
    [ input
      [ placeholder "your email *"
      , type_ "email"
      , onInput InputEmail
      , value (model.email |> rawValue)
      , required True
      ] []
      , errorLabel model.email
    ]
  , div []
    [ input
      [ placeholder "your password *"
      , type_ "password"
      , onInput InputPassword
      , value (model.password |> rawValue )
      , required True
      ] []
      , errorLabel model.password 
      ]
  , div []
    [ input
      [ placeholder "confirm password *"
      , type_ "password"
      , onInput InputConfirmPassword
      , value (model.confirmPassword |> rawValue )
      , required True
      ] []
      , errorLabel model.confirmPassword 
      ]
  , div []
    [ input
      [ type_ "checkbox"
      , onCheck CheckAcceptPolicy
      , value (model.acceptPolicy |> rawValue |> toString)
      ] []
      , label [] [ text "I accept the privacy policy "]
      ]

    , div [] [errorLabel model.acceptPolicy ]
  ]

footer model = div []
    [ button
        [ type_ "submit"
        , disabled (model.status == InProcess) ] 
        [ text "Submit" ]
        , button 
        [ type_ "button" ]
        [ text "Cancel" ] 
    ]
