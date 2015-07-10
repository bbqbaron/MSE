module Ms where

import Debug exposing (log)
import Dict exposing (Dict, foldl, insert)
import Html exposing (Html, text)
import Html.Attributes
import Html.Events exposing(on)
import List
import Maybe exposing (Maybe(..))
import Signal exposing ((<~), (~), Address, foldp, Mailbox, mailbox, message, Signal)
import Time exposing (Time)

import Html.Decoder exposing (mouseEvent)
import Svg exposing (rect, Svg, svg)
import Svg.Attributes as Attr
import Svg.Events as Ev

import Map
import Util exposing (..)
import Walker exposing(peekAndOpen)

type Action = Idle|Click Map.Point|Mark Map.Point
type GameState = InGame|Dead|Victorious

type alias Model = {
        state : GameState,
        tiles : Map.Map
    }

height = 15
width = 15

tileWidth = 30
tileHeight = 30

init : Model
init = {
        state = InGame,
        tiles = Map.makeMap width height |> Map.addCounts
    }

isNothing : Maybe a -> Bool
isNothing a = case a of
    Nothing -> True
    _ -> False

isJust : Maybe a -> Bool
isJust = isNothing >> not

update : Action -> Model -> Model
update action model =
    let model' = { model
                    | tiles <- 
                        case action of
                            Click p -> 
                                let tiles' = Dict.update p Map.setClicked model.tiles
                                in peekAndOpen (tiles', [p]) |> fst
                            Mark p -> 
                                case Dict.get p model.tiles of
                                    Just tile -> if tile.clicked then Map.mash p model.tiles else Dict.update p Map.setMarked model.tiles
                            _ -> model.tiles }
    in if | Map.checkBoom model'.tiles -> {model'|state<-Dead}
          | Map.checkRemaining model'.tiles -> {model'|state<-Victorious}
          | otherwise -> model'

updates : Mailbox Action
updates = mailbox Idle

state : Signal Model
state = foldp update init updates.signal

timer : Signal Int
timer = foldp (\dt ct -> ct + (dt//1000)) 0 (round <~ (Time.fps 1))

toPx : a -> String
toPx = toString >> ((flip (++)) "px")

renderTile : Address Action -> Model -> Map.Point -> Map.Tile -> Svg
renderTile channel model (pX,pY) tile = 
    let visible = tile.clicked || (model.state == Dead)
        baseColor = cond (not visible) (cond tile.marked "yellow" "black")
        -- curried ifs! as delicious as other curried things
        color = baseColor (cond (Map.isBomb tile) "red" "white")
        baseAttrs = [
                (pX*tileWidth)|>toPx|>Attr.x, 
                (pY*tileHeight)|>toPx|>Attr.y, 
                tileWidth|>toPx|>Attr.width, 
                tileHeight|>toPx|>Attr.height
            ]
        baseRect = rect (baseAttrs++[
            Attr.fill color
        ]) []
    in Svg.g (baseAttrs++[
                    Ev.onClick (message channel (Click (pX,pY))), Html.Events.on "contextmenu" mouseEvent (\_-> message channel (Mark (pX,pY)))
                ]
            ) ([baseRect] ++ case tile.contents of
        Map.Bomb -> if visible then [Svg.text (baseAttrs++[Attr.dy "1em"]) [text "B"]] else []
        Map.Neighbors n -> if visible then [Svg.text (baseAttrs++[Attr.dy "1em"]) [n|>toString|>text]] else []
        Map.Empty -> [])

concatMap : Map.Point -> Svg -> List Svg -> List Svg
concatMap _ v l = v :: l

renderTimer : a -> Html
renderTimer = toString >> Html.text

renderField : Address Action -> Model -> Html
renderField channel model = 
    case model.state of
        Victorious -> Html.text "NOICE"
        _ -> Dict.map (renderTile channel model) model.tiles 
                |> foldl concatMap [] 
                |> svg [
                        (tileWidth*width)|>toPx|>Attr.width, (tileHeight*height)|>toPx|>Attr.height, Html.Attributes.style [("user-select", "none")]
                    ]

render : Address Action -> Model -> Int -> Html
render channel model time = Html.div [] [
        renderTimer time,
        renderField channel model
    ]

main : Signal Html
main = render updates.address <~ state ~ timer