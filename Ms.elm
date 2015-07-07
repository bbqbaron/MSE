module Ms where

import Dict exposing (Dict, empty, foldl, insert)
import Html exposing (Html, text)
import List exposing (concat)
import Random exposing (generate, Generator, initialSeed, list, Seed)
import Random.Float exposing (probability)
import Signal exposing ((<~), foldp, Mailbox, mailbox, Signal)

import Svg exposing (rect, Svg, svg)
import Svg.Attributes as Attr

type Action = Idle
type Contents = Bomb|Neighbors Int|Nothing

type alias Tile = {contents : Contents, clicked : Bool}
type alias Map = Dict Point Tile
type alias Point = (Int,Int)

type alias Model = {
        tiles : Map
    }

height = 15
width = 15

tileWidth = 15
tileHeight = 15

cond : Bool -> a -> a -> a
cond b t f = if b then t else f

makeTile : Float -> Tile
makeTile roll = {contents = cond (roll <= 0.10) Bomb Nothing, clicked = False}

listGenerator : Generator (List Float)
listGenerator = list (height*width) probability

tiles : List Tile
tiles = 
    let (list, _) = generate listGenerator (initialSeed 0)
    in List.map makeTile list

range : Int -> List Int
range n = [0..n-1]

addTile : (Tile, Point) -> Map -> Map
addTile (tile, point) map = insert point tile map

combine : (a -> b -> c) -> List a -> List b -> List c
combine f l1 l2 = List.map f l1 |> List.map (flip List.map l2) |> concat

tupleMap : (a->b) -> (a,a) -> (b,b)
tupleMap f (a,a) = (f a, f a)

spread : (a -> b -> c) -> (a,b) -> c
spread f (a,b) = f a b

makeMap : Map
makeMap = (width, height)
    |> tupleMap range
    |> spread (combine (,))
    |> List.map2 (,) tiles
    |> List.foldl addTile empty

init : Model
init = {
        tiles = makeMap
    }

update : Action -> Model -> Model
update action model = case action of
    _ -> model

updates : Mailbox Action
updates = mailbox Idle

state : Signal Model
state = foldp update init updates.signal

toPx : a -> String
toPx = toString >> ((flip (++)) "px")

renderTile : Point -> Tile -> Svg
renderTile (pX,pY) {contents} = rect [
        (pX*tileWidth)|>toPx|>Attr.x, 
        (pY*tileHeight)|>toPx|>Attr.y, 
        tileWidth|>toPx|>Attr.width, 
        tileHeight|>toPx|>Attr.height,
        Attr.fill "black"
    ]
    (case contents of
        Bomb -> [text "bomb"]
        Neighbors n -> [n|>toString|>text]
        _ -> [])

concatMap : Point -> Svg -> List Svg -> List Svg
concatMap _ v l = v :: l

render : Model -> Html
render model = Dict.map renderTile model.tiles 
    |> foldl concatMap [] 
    |> svg [
            (tileWidth*width)|>toPx|>Attr.width, (tileHeight*height)|>toPx|>Attr.height
        ]

main : Signal Html
main = render <~ state