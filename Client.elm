module Client exposing (..)
import Array exposing (Array)
import AnimationFrame
import Color exposing (..)
import Collage exposing (..)
import Text exposing (..)
import Element exposing (toHtml)
import Html exposing (..)
import Mouse
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import WebSocket



main : Program String Model Msg
main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model =
  { messages: List String
  , input: String
  , server: String
  , mouse : Mouse.Position
  , clicked : Bool
  , field : Array (Array Int)
  }

init : String -> ( Model, Cmd msg )
init server =
  ( { messages = []
    , input = ""
    , server = server
    , mouse = { x = 0, y = 0 }
    , clicked = False
    , field = Array.initialize 10 (always (Array.initialize 10 (always 0)))
    }
  , Cmd.none
  )


-- UPDATE

type Msg
  = InputMessage String
  | SubmitMessage
  | ServerMessage String
  | MoveMessage Mouse.Position
  | DownMessage Mouse.Position
  | UpMessage Mouse.Position

updateCell : Int -> Int -> Array (Array Int) -> Array (Array Int)
updateCell x y field =
  Array.indexedMap
    (\rowIdx -> \row ->
      (if rowIdx == y
       then (Array.indexedMap
              (\colIdx -> \n ->
                (if colIdx == x
                 then 1
                 else n))
              row
            )
        else row
      )
    )
    field

update : Msg -> Model -> ( Model, Cmd msg )
update message model =
  case message of
    InputMessage value ->
      ( { model | input = value}
      , Cmd.none
      )
    SubmitMessage ->
      ( { model | input = ""}
      , WebSocket.send model.server model.input
      )
    ServerMessage message ->
      ( { model
        | messages = message :: model.messages
        }
      , Cmd.none
      )
    MoveMessage position ->
      let
        gridX = position.x // 32
        gridY = position.y // 32
      in
      ( { model
          | mouse = position
          , field = if model.clicked
                    then updateCell gridX gridY model.field
                    else model.field
        }
      , Cmd.none
      )
    UpMessage position ->
      ( { model | mouse = position, clicked = False }
      , Cmd.none
      )
    DownMessage position ->
      let
        gridX = position.x // 32
        gridY = position.y // 32
      in
      ( { model
          | mouse = position
          , clicked = True
          , field = updateCell gridX gridY model.field}
      , Cmd.none
      )



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Mouse.moves MoveMessage
    , Mouse.ups UpMessage
    , Mouse.downs DownMessage
    , WebSocket.listen model.server ServerMessage
    ]


-- VIEW

view : Model -> Html Msg
view model =
  let
    hoge = (String.concat model.messages)
      |> Text.fromString
      |> Collage.text
    gridX = (toFloat (model.mouse.x // 32))
    gridY = (toFloat (model.mouse.y // 32))
    canvas =
    collage 640 480 
      (List.concat
      (List.indexedMap
        (\rowIdx -> \row ->
          (List.indexedMap
            (\colIdx -> \n ->
              (rect 32 32
                |> (case n of
                  0 -> filled white
                  1 -> filled red
                  2 -> filled blue
                  _ -> filled white)
                |> move ( (toFloat (-320 + 16 + colIdx * 32))
                        , (toFloat (240 - 16 - rowIdx * 32))
                        )
              ))
          (Array.toList row))
        )
        (Array.toList model.field)
      )
      )
      --rect 32 32
      --  |> filled red
      --  |> move (-320 + 16 + gridX * 32, 240 - 16 - gridY * 32),
      --hoge
      
  in
    Html.div
      []
      [ toHtml canvas ]


