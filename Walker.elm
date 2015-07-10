module Walker where

import Dict
import List
import Maybe exposing (withDefault)

import Map exposing (Contents(Bomb, Empty), Map, Point)

type alias Walker = (Map, List Point) -- a map, a list of points to check, a list of points already seen

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
                map' = if openIt then Dict.update p Map.setClicked map else map
                -- update the list of points
                points' = 
                    (withDefault [] (List.tail points)) 
                    ++ (if continue then List.map (Map.move p) Map.neighbors else [])
            in peekAndOpen (map', points')
        _ -> (map, points)