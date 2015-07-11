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
density = 0.03

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
checkFor fn map = List.filter fn (Dict.values map) |> log "filter list" |> List.length |> ((flip (>)) 0)

checkBoom : Map -> Bool
checkBoom = checkFor (\{contents, clicked} -> contents == Bomb && clicked)

checkRemaining : Map -> Bool
checkRemaining = checkFor (\{contents, clicked} -> contents /= Bomb && not clicked)

countBombs : Point -> Map -> Int
countBombs (x,y) map = 
    neighborsOf (x,y)
    -- reduce with addition if bomb
    |> List.foldl (\(x,y) -> Dict.get (x,y) map |> condM isBomb False |> ((condR 1 0) >> (+))) 0

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
mash p map = List.foldl (\dir map'->
    let dest = move p dir
    in case Dict.get dest map' of
        Just tile -> if tile.marked then map' else (peekAndOpen (map', [dest]) True |> fst)
        _ -> map') map neighbors

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

setMarked : Maybe Tile -> Maybe Tile
setMarked mTile = case mTile of
    Just someTile -> Just {someTile|marked<-not someTile.marked}
    _ -> Nothing

tiles : Int -> Int -> List Tile
tiles width height = 
    let (list, _) = generate (listGenerator width height) (initialSeed 0)
    in List.map makeTile list

tileIs : Contents -> Tile -> Bool
tileIs what tile = tile.contents == what

-- recursive tile opener
type alias Walker = (Map, List Point) -- a map, a list of points to check

peekAndOpen : Walker -> Bool -> Walker
peekAndOpen (map, points) force =
    case List.head points of
        Just p ->
            let maybeTile = Dict.get p map
                -- do we open the tile? do we keep going?
                (openIt, continue) = case maybeTile of
                    Just {contents, clicked} -> (
                            (contents /= Bomb && not clicked)||force,
                            (contents == Empty && not clicked)||force
                        )
                    _ -> (False, False)
                -- update the map
                map' = if openIt then Dict.update p setClicked map else map
                -- update the list of points
                points' = 
                    (withDefault [] (List.tail points)) 
                    ++ (if continue then List.map (move p) neighbors else [])
            in peekAndOpen (map', points') False
        _ -> (map, points)