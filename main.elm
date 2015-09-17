import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import StartApp
import String
import Json.Decode as Json exposing ((:=))
import Http
import String
import Char
import Task exposing (..)
import Effects exposing (Never, Effects)

-- MODEL 

type alias Username = String
type alias Password = String
type alias ReturnValue = Maybe String

type alias Model = (Username, Password, Maybe Http.Response)

init : (Model, Effects Action)
init = (("", "", Nothing), Effects.none)

-- UPDATE

type Action
  = UpdateUsername String
  | UpdatePassword String 
  | Submit
  | Submitted (Maybe Http.Response)

update : Action -> Model -> (Model, Effects Action)
update action (username, password, returnValue) =
  case action of
    UpdateUsername name -> ((name, password, Nothing), Effects.none)
    UpdatePassword pswd -> ((username, pswd, Nothing), Effects.none)
    Submit -> ((username, password, Nothing), getRandomGif username password)
    Submitted (Just r) -> ((username, password, (Just r)), Effects.none)
    Submitted Nothing -> ((username, password, Nothing), Effects.none)

-- VIEW

view : Address Action -> Model -> Html
view address (username, pswd, response) =
  let r =
    case response of
      Nothing -> []
      Just r ->
        [
          div [ myStyle ] [ text (toString r.status) ]
        , div [ myStyle ] [ text (toString r.statusText) ]
        , div [ myStyle ] [ text (toString r.headers) ]
        -- , div [ myStyle ] [ text (toString r.value.Text) ]
        ]
  in div []
    ([ input
        [ placeholder "username"
        , value username 
        , on "input" (targetValue) (\u -> Signal.message address (UpdateUsername u))
        , myStyle
        ]
        []
    , input
        [ type' "password"
        , placeholder "Password"
        , value pswd 
        , on "input" (targetValue) (\p -> Signal.message address (UpdatePassword p))
        , myStyle
        ]
        []
    , button [ onClick address Submit] [ text "submit" ]
    ] ++ r)

myStyle : Attribute
myStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]

-- EFFECTS

getRandomGif : String -> String -> Effects Action
getRandomGif username password =
  -- send : Settings -> Request -> Task RawError Response
  Http.send Http.defaultSettings
    {
      verb = "POST"
    , headers = []
    , url = signupEndpoint
    , body = (Http.string
             ("{\"username\":\"" ++ username ++ "\", \"password\":\"" ++ password ++ "\"}"))
    }
    |> Task.toMaybe -- toMaybe : Task x a -> Task y (Maybe a)
    |> Task.map Submitted -- map :  (a -> b) -> Effects a -> Effects b
    |> Effects.task -- task : Task Never a -> Effects a

signupEndpoint : String
signupEndpoint =
  Http.url "http://localhost:1323/users" []

decodeUrl : Json.Decoder String
decodeUrl =
  Json.at ["data", "image_url"] Json.string

-- MAIN APP

app =
  StartApp.start
    { init = init,
      update = update,
      view = view,
      inputs = []
    }

main =
  app.html

port tasks : Signal (Task.Task Never())
port tasks =
  app.tasks
