module Client exposing (..)
import AnimationFrame
import Color exposing (..)
import Collage exposing (..)
import Text exposing (..)
import Element exposing (toHtml)
import Html exposing (..)
import Mouse
import Keyboard
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

type Scene
  = Mapping
  | Battle

type alias Model =
  { messages: List String
  , input: String
  , server: String
  , mouse : Mouse.Position
  , clicked : Bool
  , field : List (List Int)
  , scene : Scene
  }

init : String -> ( Model, Cmd msg )
init server =
  ( { messages = []
    , input = ""
    , server = server
    , mouse = { x = 0, y = 0 }
    , clicked = False
    , field = List.repeat 15 (List.repeat 20 0)
    , scene = Mapping
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
  | PressMessage Keyboard.KeyCode

sliceAround : Int -> Int -> List (List Int) -> List Int
sliceAround x y field =
  List.concat
    ( let
        yf = (y - 1) < 0
      in
      List.map
        (\row ->
          let
            xf = (x - 1) < 0
            from = (List.drop (x - 1) row)
          in
          (if xf then (List.take 1 from) else [])
          ++ (List.take 1 (List.drop 2 from)))
        (List.take (if yf then 2 else 3) (List.drop (y - 1) field)))

updateCell : Int -> Int -> List (List Int) -> List (List Int)
updateCell x y field =
  List.indexedMap
    (\rowIdx -> \row ->
      (if rowIdx == y
       then (List.indexedMap
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
  let
    next =
      case model.scene of
        Mapping -> model.field
        Battle ->
          List.indexedMap
            (\rowIdx -> \row ->
              (List.indexedMap
                (\colIdx -> \n ->
                  let
                    s = List.sum (sliceAround colIdx rowIdx model.field)
                  in
                  if s <= 1 || 4 <= s then 0 else 1
                )
                row
              )
            )
            model.field
  in
  case message of
    InputMessage value ->
      ( { model | input = value, field = next}
      , Cmd.none
      )
    SubmitMessage ->
      ( { model | input = "", field = next}
      , WebSocket.send model.server model.input
      )
    ServerMessage message ->
      ( { model
        | messages = message :: model.messages
        , field = next
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
                    then updateCell gridX gridY next
                    else next
        }
      , Cmd.none
      )
    UpMessage position ->
      ( { model
        | mouse = position
        , clicked = False
        , field = next
        }
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
          , field = updateCell gridX gridY next}
      , Cmd.none
      )
    PressMessage code ->
      case model.scene of
        Mapping ->
          ( { model | scene = Battle }
          , Cmd.none
          )
        Battle ->
          ( { model | scene = Mapping }
          , Cmd.none
          )



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Mouse.moves MoveMessage
    , Mouse.ups UpMessage
    , Mouse.downs DownMessage
    , Keyboard.presses PressMessage
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
            row)
          )
          model.field
        )++
        [rect 32 32
          |> filled (rgba 255 0 0 0.5)
          |> move (-320 + 16 + gridX * 32, 240 - 16 - gridY * 32)]
      )
  in
    Html.div
      []
      [ toHtml canvas ]


