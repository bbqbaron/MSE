module Ms where

import Debug exposing (log)
import Dict exposing (Dict, empty, foldl, insert)
import Html exposing (Html, text)
import Html.Attributes
import Html.Events exposing(on)
import List exposing (concat)
import Maybe exposing (Maybe(..), withDefault)
import Random exposing (generate, Generator, initialSeed, list, Seed)
import Random.Float exposing (probability)
import Signal exposing ((<~), (~), Address, foldp, Mailbox, mailbox, message, Signal)
import Time exposing (Time)

import Html.Decoder exposing (mouseEvent)
import Svg exposing (rect, Svg, svg)
import Svg.Attributes as Attr
import Svg.Events as Ev

type alias Point = (Int,Int)

type Action = Idle|Click Point|Mark Point
type Contents = Bomb|Neighbors Int|Empty
type GameState = InGame|Dead|Victorious

type alias Tile = {contents : Contents, clicked : Bool, marked : Bool}
type alias Map = Dict Point Tile
type alias Model = {
        state : GameState,
        tiles : Map
    }
type alias Walker = (Map, List Point) -- a map and a list of undone points

move : Point -> Point -> Point
move (x1,y1) (x2,y2) = (x1+x2,y1+y2)

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
    |> List.foldl (\(x,y) -> Dict.get (x,y) map |> condM isBomb False |> ((condR 1 0) >> (+))) 0

addCount : Int -> Tile -> Tile
addCount n tile = 
    if tile.contents /= Bomb then 
        {tile|contents<-if n > 0 then Neighbors n else Empty}
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
makeTile roll = {contents = cond (roll <= 0.15) Bomb Empty, clicked = False, marked = False}

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
        state = InGame,
        tiles = makeMap |> addCounts
    }

setClicked : Maybe Tile -> Maybe Tile
setClicked mTile = case mTile of
    Just someTile -> Just {someTile|clicked<-True}
    _ -> Nothing

setMarked : Maybe Tile -> Maybe Tile
setMarked mTile = case mTile of
    Just someTile -> Just {someTile|marked<-True}
    _ -> Nothing

force : (a->b) -> Maybe a -> b
force fn v = case v of
    Just a -> fn a

lose : Model -> Model
lose model = {model|state<-Dead}

isSomething : Contents -> Tile -> Bool
isSomething what tile = tile.contents == what

isBomb : Tile -> Bool
isBomb = isSomething Bomb

isEmpty : Tile -> Bool
isEmpty = isSomething Empty

isNothing : Maybe a -> Bool
isNothing a = case a of
    Nothing -> True
    _ -> False

isJust : Maybe a -> Bool
isJust = isNothing >> not

peekAndOpen : Walker -> Walker
peekAndOpen (map, points) =
    case List.head points of
        Just p ->
            let mTile = Dict.get p map
                -- do we open the tile? do we keep going?
                (openIt, continue) = case mTile of
                    Just {contents, clicked} -> (
                            contents /= Bomb && not clicked,
                            contents == Empty && not clicked
                        )
                    _ -> (False, False) 
                -- update the map
                map' = if openIt then Dict.update p setClicked map else map
                -- update the list of points
                points' = 
                    (withDefault [] (List.tail points)) 
                    ++ (if continue then List.map (move p) neighbors else [])
            in peekAndOpen (map', points')
        _ -> (map, points)

chain : List (a->a) -> a -> a
chain fns a = case List.head fns of
    Just fn -> fn a |> chain (withDefault [] (List.tail fns))
    _ -> a

openTile : Map -> Point -> Map
openTile map p =
    let (map', _) = peekAndOpen (map, [p])
    in map'

walkOpen : Point -> Map -> Map
walkOpen p map = openTile map p

mash : Point -> Map -> Map
mash p map = List.foldl (\dir map'->
    let dest = move p dir
    in case Dict.get dest map' of
        Just tile -> if tile.marked then map' else Dict.update dest setClicked map'
        _ -> map') map neighbors

update : Action -> Model -> Model
update action model =
    let tiles = model.tiles
        tiles' = case action of
            Click p -> walkOpen p tiles
            Mark p ->
                case Dict.get p tiles of
                    Just tile -> if tile.clicked then mash p tiles else Dict.update p setMarked tiles
                    -- should never fail. BOOM.
            _ -> tiles
        tileList = Dict.values tiles'
        closedNotBomb = List.filter (\t->t.contents /= Bomb && not t.clicked) tileList |> List.length
        bombNotClosed = List.filter (\t->t.contents == Bomb && t.clicked) tileList |> List.length
    in case closedNotBomb of
        0 -> {model|state<-Victorious}
        _ -> if bombNotClosed > 0 then lose model else
            case action of
                Click p -> 
                    let tile = Dict.get p tiles'
                        tileIsEmpty = force isEmpty tile
                        tileIsBomb = force isBomb tile
                        model' = {model|tiles<-tiles'}
                    in model' |> cond tileIsBomb lose identity
                Mark p -> {model|tiles<-tiles'}
                _ -> model

updates : Mailbox Action
updates = mailbox Idle

state : Signal Model
state = foldp update init updates.signal

timer : Signal Int
timer = foldp (\dt ct -> ct + (dt//1000)) 0 (round <~ (Time.fps 1))

toPx : a -> String
toPx = toString >> ((flip (++)) "px")

renderTile : Address Action -> Model -> Point -> Tile -> Svg
renderTile channel model (pX,pY) tile = 
    let visible = tile.clicked || (model.state == Dead)
        baseColor = cond (not visible) (cond tile.marked "yellow" "black")
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
    in Svg.g (baseAttrs++[
                    Ev.onClick (message channel (Click (pX,pY))), Html.Events.on "contextmenu" mouseEvent (\_-> message channel (Mark (pX,pY)))
                ]
            ) ([baseRect] ++ case tile.contents of
        Bomb -> if visible then [Svg.text (baseAttrs++[Attr.dy "1em"]) [text "B"]] else []
        Neighbors n -> if visible then [Svg.text (baseAttrs++[Attr.dy "1em"]) [n|>toString|>text]] else []
        Empty -> [])

concatMap : Point -> Svg -> List Svg -> List Svg
concatMap _ v l = v :: l

renderTimer : Int -> Html
renderTimer time = time |> toString |> Html.text

renderField : Address Action -> Model -> Html
renderField channel model = 
    if model.state == Victorious then 
        Html.text "NOICE"
    else    
        Dict.map (renderTile channel model) model.tiles 
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