module Map where

import Debug exposing (log)
import Dict exposing (Dict, empty)
import List
import Maybe exposing (withDefault)
import Random exposing (generate, Generator, initialSeed, list, Seed)
import Random.Float exposing (probability)

import Util exposing (..)

type Contents = Bomb|Neighbors Int|Empty

type alias Point = (Int,Int)
type alias Tile = {contents : Contents, clicked : Bool, marked : Bool}
type alias Map = Dict Point Tile

density : Float
density = 0.2

addCount : Int -> Tile -> Tile
addCount n tile =
    if tile.contents /= Bomb then
        {tile|contents<-if n > 0 then Neighbors n else Empty}
    else
        tile

addCounts : Map -> Map
addCounts map = Dict.map (\p t -> addCount (countBombs p map) t) map

addTile : (Tile, Point) -> Map -> Map
addTile (tile, point) map = Dict.insert point tile map

checkFor : (Tile -> Bool) -> Map -> Bool
checkFor fn map = List.filter fn (Dict.values map) |> List.length |> ((flip (>)) 0)

checkBoom : Map -> Bool
checkBoom = checkFor (\{contents, clicked} -> contents == Bomb && clicked)

checkRemaining : Map -> Bool
checkRemaining = checkFor (\{contents, clicked} -> contents /= Bomb && not clicked)

countBombs : Point -> Map -> Int
countBombs (x,y) map =
    neighborsOf (x,y)
    -- reduce with addition if bomb
    |> List.foldl (\(x,y) -> Dict.get (x,y) map |> condM isBomb False |> ((condR 1 0) >> (+))) 0

countRemaining : Map -> Int
countRemaining map =
    let tiles = Dict.values map
        marked = List.foldl ((.marked)>>condR 1 0>>(+)) 0 tiles
        bombs = List.foldl ((.contents)>>((==)Bomb)>>condR 1 0>>(+)) 0 tiles
    in bombs - marked

isBomb : Tile -> Bool
isBomb = tileIs Bomb

isEmpty : Tile -> Bool
isEmpty = tileIs Empty

listGenerator : Int -> Int -> Generator (List Float)
listGenerator width height = list (width*height) probability

makeMap : Int -> Int -> Map
makeMap width height = (width, height)
    |> tupleMap range
    |> spread (combine (,))
    |> List.map2 (,) (tiles width height)
    |> List.foldl addTile empty

makeTile : Float -> Tile
makeTile roll = {contents = cond (roll <= density) Bomb Empty, clicked = False, marked = False}

mash : Point -> Map -> Map
mash p map =
    let ts = neighborTiles p map
        markedNeighbors = ts |> List.filter (.marked) |> List.length
        bombNeighbors = ts |> List.filter ((.contents)>>((==)Bomb)) |> List.length
        equal = markedNeighbors == bombNeighbors
    in 
        if equal then
            openUnmarkedNeighborsOfIn p map
        else
            map

move : Point -> Point -> Point
move (x1,y1) (x2,y2) = (x1+x2,y1+y2)

neighbors : List Point
neighbors =
    -- list of coord tuples
    combine (,) [-1..1] [-1..1]
    -- remove 0,0
    |> List.filter (\(x',y')->not (x'==0 && y'==0))

neighborsOf : Point -> List Point
neighborsOf (pX,pY) = neighbors |> List.map (\(x,y) -> (pX+x, pY+y))

neighborTiles : Point -> Map -> List Tile
neighborTiles p map = neighbors |> List.map ((move p) >> ((flip Dict.get) map)) |> List.filter isJust |> List.map (force identity)

setClicked : Maybe Tile -> Maybe Tile
setClicked mTile = case mTile of
    Just someTile -> Just {someTile|clicked<-True}
    _ -> Nothing

toggleMarked : Maybe Tile -> Maybe Tile
toggleMarked mTile = case mTile of
    Just someTile -> 
        Just {someTile|marked<-not someTile.marked}
    _ -> Nothing

tiles : Int -> Int -> List Tile
tiles width height =
    let (list, _) = generate (listGenerator width height) (initialSeed 0)
    in List.map makeTile list

tileIs : Contents -> Tile -> Bool
tileIs what tile = tile.contents == what

openUnmarkedNeighborsOfIn : Point -> Map -> Map
openUnmarkedNeighborsOfIn p map =
    case Dict.get p map of
        Just tile ->
            neighborsOf p
            |> List.foldl
                (\p' m' ->
                    case Dict.get p' m' of
                        Just tile -> 
                            if not tile.marked then
                                Dict.update p' setClicked m'
                            else
                                m'
                        _ -> m')
                map
        _ -> map

openNeighborsOfIn : Point -> Map -> Map
openNeighborsOfIn p map =
    case Dict.get p map of
        Just tile ->
            neighborsOf p
            |> List.foldl
                (\p' m' ->
                    Dict.update p' setClicked m')
                map
        _ -> map

shouldOpenNeighbors : Map -> Point -> Tile -> Bool
shouldOpenNeighbors map p tile =
    let closedNeighbors =
            neighborsOf p
            |> List.filter (
                \p' -> case Dict.get p' map of
                    Just tile -> not tile.clicked
                    _ -> False)
            |> List.isEmpty
            |> not
    in tile.contents == Empty && tile.clicked && closedNeighbors

ensureOpen : Map -> Map
ensureOpen map =
    let found = Dict.filter (shouldOpenNeighbors map) map |> Dict.keys |> List.head
    in case log "found" found of
        Just p -> openNeighborsOfIn p map |> ensureOpen
        _ -> map