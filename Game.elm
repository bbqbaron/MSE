module Game where

import Debug exposing (log)
import Dict exposing (Dict, foldl, insert)
import Html exposing (Html, text)
import Html.Attributes
import Html.Events exposing (on, onClick)
import List
import Maybe exposing (Maybe(..))
import Signal exposing (Address, message)

import Flex
import Html.Decoder exposing (mouseEvent)
import Svg exposing (rect, Svg, svg)
import Svg.Attributes as Attr
import Svg.Events as Ev
import Svg.Lazy exposing (lazy, lazy2, lazy3)

import Map
import Type exposing (GameRequest)
import Util exposing (..)

type Action = Click Map.Point|Mark Map.Point|NewGame|Tick
type GameState = InGame|Dead|Victorious

type alias Model = {
        state : GameState,
        tiles : Map.Map,
        time : Int
    }

height = 25
width = 25

tileWidth = 30
tileHeight = 30

init : GameRequest -> Model
init {numberOfMines, width, height} = {
        state = InGame,
        tiles = Map.makeMap ((toFloat numberOfMines)/((toFloat width)*(toFloat height))) width height |> Map.addCounts,
        time = 0
    }

update : Action -> Model -> (Model, Maybe Type.Mode)
update action model =
    case action of
        NewGame -> (model, Just Type.New)
        Tick -> ({model|time<-model.time+1}, Nothing)
        _ ->
            (
                let tiles' = case action of
                                Click p -> 
                                    Dict.update p Map.setClicked model.tiles
                                Mark p -> 
                                    case Dict.get p model.tiles of
                                        Just tile -> if tile.clicked then Map.mash p model.tiles else Dict.update p Map.toggleMarked model.tiles
                                _ -> model.tiles
                    model' = {model|tiles<-Map.ensureOpen tiles'}
                in if | Map.checkBoom model'.tiles -> {model'|state<-Dead}
                      | Map.checkRemaining model'.tiles |> not -> {model'|state<-Victorious}
                      | otherwise -> model'
            ,
            Nothing)

toPx : a -> String
toPx = toString >> ((flip (++)) "px")

renderTile : Address Action -> Model -> Map.Point -> Map.Tile -> Svg
renderTile channel model (pX,pY) tile = 
    let visible = tile.clicked || model.state == Dead
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
    in (lazy2 Svg.g)
            (
                baseAttrs
                ++[
                    Ev.onClick (message channel (Click (pX,pY))), 
                    Html.Events.on "contextmenu" mouseEvent (\_-> message channel (Mark (pX,pY)))
                ]
            ) 
            (
                [baseRect] 
                ++ case tile.contents of
                        Map.Bomb -> if visible then [
                            (lazy2 Svg.text) (baseAttrs++[Attr.dy "1.5em", Attr.dx "0.5em"]) [text "B"]] else []
                        Map.Neighbors n -> if visible then [
                            (lazy2 Svg.text) (baseAttrs++[Attr.dy "1.5em", Attr.dx "0.5em"]) [n|>toString|>text]] else []
                        Map.Empty -> []
            )

renderCount : {a|tiles:Map.Map} -> Html
renderCount = (.tiles) >> Map.countRemaining >> toString >> Html.text

renderTimer : a -> Html
renderTimer = toString >> Html.text

renderUI : Address Action -> Model -> Html
renderUI channel model = 
    List.map 
        (Html.p [])
        [
            [Html.text "Elapsed: ", renderTimer model.time],
            [Html.text "Remaining: ", renderCount model]
        ]
    |> ((++) [Html.button [Html.Events.onClick channel NewGame] [Html.text "New"]])
    |> Html.div []

renderField : Address Action -> Model -> Html
renderField channel model = 
    case model.state of
        Victorious -> Html.text "NOICE"
        _ -> Dict.map (renderTile channel model) model.tiles 
                |> foldl (padl (::)) []
                |> (lazy2 svg) [
                        (tileWidth*width)|>toPx|>Attr.width, (tileHeight*height)|>toPx|>Attr.height, Html.Attributes.style [("user-select", "none")]
                    ]

render : Address Action -> Model -> Html
render channel model = Html.div [] [
        renderUI channel model,
        renderField channel model
    ]