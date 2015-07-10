module Util where

import List exposing (concat)
import Maybe exposing (withDefault)

chain : List (a->a) -> a -> a
chain fns a = case List.head fns of
    Just fn -> fn a |> chain (withDefault [] (List.tail fns))
    _ -> a

combine : (a -> b -> c) -> List a -> List b -> List c
combine f l1 l2 = List.map f l1 |> List.map (flip List.map l2) |> concat

cond : Bool -> a -> a -> a
cond b t f = if b then t else f

condM : (a->b) -> b -> Maybe a -> b
condM j n m = case m of
    Just something -> j something
    Nothing -> n

condR : a -> a -> Bool -> a
condR t f b = if b then t else f

force : (a->b) -> Maybe a -> b
force fn v = case v of
    Just a -> fn a

range : Int -> List Int
range n = [0..n-1]

spread : (a -> b -> c) -> (a,b) -> c
spread f (a,b) = f a b

tupleMap : (a->b) -> (a,a) -> (b,b)
tupleMap f (a,a) = (f a, f a)
