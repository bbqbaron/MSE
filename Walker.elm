module Walker where

import Debug exposing (log)

import Dict
import List
import Maybe exposing (withDefault)

import Map exposing (Contents(Bomb, Empty), Map, Point)

unopenedNeighbors : Point -> Map -> Bool
unopenedNeighbors p map =
    case Dict.get p map of
        Just tile -> Map.neighborTiles p map |> List.filter ((.clicked) >> not) |> length |> ((>) 0)
        _ -> False

cascade : Map -> Map
cascade map = 
    -- get first empty, open tile with unopened neighbors
    case Dict.filter (\p t->t.clicked && t.contents == Empty && unopenedNeighbors p map) map |> Dict.values |> List.head of
        Just 
    
    -- open those neighbors
    -- recur
    -- else return




type alias Walker = (Map, List Point) -- a map, a list of points to check

peekAndOpen : Walker -> Walker
peekAndOpen (map, points) =
    case List.head points of
        Just p ->
            let maybeTile = Dict.get p map
                -- do we open the tile? do we keep going?
                (openIt, continue) = (case maybeTile of
                    Just {contents, clicked} -> (
                            contents /= Bomb && not clicked,
                            contents == Empty && not clicked
                        )
                    _ -> (False, False) ) |> log "open,continue"
                -- update the map
                map' = if openIt then Dict.update p Map.setClicked map else map
                -- update the list of points
                points' = 
                    (withDefault [] (List.tail points)) 
                    ++ (if continue then List.map (Map.move p) Map.neighbors else [])
            in peekAndOpen (map', points')
        _ -> (map, points)