module Ms where

import Debug exposing (log)
import Dict exposing (Dict, empty, foldl, insert)
import Html exposing (Html, text)
import Html.Attributes
import List exposing (concat)
import Maybe exposing (Maybe(..), withDefault)
import Random exposing (generate, Generator, initialSeed, list, Seed)
import Random.Float exposing (probability)
import Signal exposing ((<~), Address, foldp, Mailbox, mailbox, message, Signal)

import Svg exposing (rect, Svg, svg)
import Svg.Attributes as Attr
import Svg.Events as Ev

type Action = Idle|Click (Int,Int)
type Contents = Bomb|Neighbors Int|Empty

type alias Tile = {contents : Contents, clicked : Bool}
type alias Map = Dict Point Tile
type alias Point = (Int,Int)

type alias Model = {
        dead : Bool,
        tiles : Map
    }

id : a -> a
id x = x

combine : (a -> b -> c) -> List a -> List b -> List c
combine f l1 l2 = List.map f l1 |> List.map (flip List.map l2) |> concat

tupleMap : (a->b) -> (a,a) -> (b,b)
tupleMap f (a,a) = (f a, f a)

spread : (a -> b -> c) -> (a,b) -> c
spread f (a,b) = f a b

condM : (a->b) -> b -> Maybe a -> b
condM j n m = case m of
    Just something -> j something
    Nothing -> n

condR : a -> a -> Bool -> a
condR t f b = if b then t else f

neighbors : List (Int,Int)
neighbors =
    -- list of coord tuples
    combine (,) [-1..1] [-1..1]
    -- remove 0,0
    |> List.filter (\(x',y')->not (x'==0 && y'==0))

neighborsOf : Point -> List Point
neighborsOf (pX,pY) = neighbors |> List.map (\(x,y) -> (pX+x, pY+y))

neighborMap : Point -> Map -> List (Point, Contents)
neighborMap p map = neighbors |> List.map (\p->(p, Dict.get p map |> force (.contents)))

count : Point -> Map -> Int
count (x,y) map = 
    neighborsOf (x,y)
    -- reduce with addition if bomb
    |> List.foldl (\(x,y) -> Dict.get (x,y) map |> condM isBomb False |> ((condR 1 0) >> (+)))
    0

addCount : Int -> Tile -> Tile
addCount n tile = 
    if tile.contents /= Bomb then 
        {tile|contents<-Neighbors n}
    else
        tile

addCounts : Map -> Map
addCounts map = Dict.map (\p t -> addCount (count p map) t) map

height = 15
width = 15

tileWidth = 30
tileHeight = 30

cond : Bool -> a -> a -> a
cond b t f = if b then t else f

makeTile : Float -> Tile
makeTile roll = {contents = cond (roll <= 0.10) Bomb Empty, clicked = False}

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

makeMap : Map
makeMap = (width, height)
    |> tupleMap range
    |> spread (combine (,))
    |> List.map2 (,) tiles
    |> List.foldl addTile empty

init : Model
init = {
        dead = False,
        tiles = makeMap |> addCounts
    }

setClicked : Maybe Tile -> Maybe Tile
setClicked mTile = case mTile of
    Just someTile -> Just {someTile|clicked<-True}
    _ -> Nothing

force : (a->b) -> Maybe a -> b
force fn v = case v of
    Just a -> fn a

lose : Model -> Model
lose model = {model|dead<-True}

isSomething : Contents -> Tile -> Bool
isSomething what tile = tile.contents == what

isBomb : Tile -> Bool
isBomb = isSomething Bomb

isEmpty : Tile -> Bool
isEmpty = isSomething Empty

walkOpen : Point -> Model -> Model
-- TODO recursively find and open all empty neighbors
walkOpen p m = m

update : Action -> Model -> Model
update action model = case action of
    Click p -> 
        -- nonexhaustive; cowboy coding
        let tiles' = Dict.update p setClicked model.tiles
            tile = Dict.get p tiles'
            empty = force isEmpty tile
            boom = force isBomb tile
            model' = {model|tiles<-tiles'}
        in model' |> cond boom lose id |> cond empty (walkOpen p) id 
    _ -> model

updates : Mailbox Action
updates = mailbox Idle

state : Signal Model
state = foldp update init updates.signal

toPx : a -> String
toPx = toString >> ((flip (++)) "px")

renderTile : Address Action -> Model -> Point -> Tile -> Svg
renderTile channel model (pX,pY) tile = 
    let baseColor = cond (not (tile.clicked||model.dead)) "black"
        -- curried ifs! as delicious as other curried things
        color = baseColor (cond (isBomb tile) "red" "white")
        baseAttrs = [
                (pX*tileWidth)|>toPx|>Attr.x, 
                (pY*tileHeight)|>toPx|>Attr.y, 
                tileWidth|>toPx|>Attr.width, 
                tileHeight|>toPx|>Attr.height
            ]
        baseRect = rect (baseAttrs++[
            Attr.fill color
        ]) []
    in Svg.g (baseAttrs++[Ev.onClick (message channel (Click (pX,pY)))]) ([baseRect] ++ case tile.contents of
        Bomb -> if color /= "black" then [Svg.text (baseAttrs++[Attr.dy "1em"]) [text "B"]] else []
        Neighbors n -> if color /= "black" then [Svg.text (baseAttrs++[Attr.dy "1em"]) [n|>toString|>text]] else []
        Empty -> [])

concatMap : Point -> Svg -> List Svg -> List Svg
concatMap _ v l = v :: l

render : Address Action -> Model -> Html
render channel model = Dict.map (renderTile channel model) model.tiles 
    |> foldl concatMap [] 
    |> svg [
            (tileWidth*width)|>toPx|>Attr.width, (tileHeight*height)|>toPx|>Attr.height, Html.Attributes.style [("user-select", "none")]
        ]

main : Signal Html
main = (render updates.address) <~ state