module NewGame where

import Debug exposing (log)
import Html as H
import Html.Attributes as HAttr
import Html.Events as HEv
import List
import Maybe exposing (Maybe(..))
import Result exposing (Result(Ok))
import Signal exposing (Address, message)

import Html.Events.Extra as HEvX

import Type exposing (GameRequest)
import Util exposing (..)

type alias Model = GameRequest

type Action = NumberOfMines Int|Width Int|Height Int|Submit
type Request = CreateGame Model|None

init : Model
init = {
        numberOfMines = 0,
        width = 0,
        height = 0
    }

update : Action -> Model -> (Model, Request)
update action model =
    let model' = 
            case action of
                NumberOfMines n -> {model|numberOfMines<-n}
                Width w -> {model|width<-w}
                Height h -> {model|height<-h}
                _ -> model
    in (
        model',
        case action of
            Submit -> CreateGame model'
            _ -> None
        )

-- helper function to dedupe inputs that update numbers
intSender : Address Action -> Model -> String -> (Model -> Int) -> (Int -> Action) -> H.Html
intSender channel model label accessor actionType =
    H.p [] [
        H.text label,
        H.input [
            actionType >> message channel |> HEv.on "input" HEvX.targetValueIntParse
        ] []
    ]
    
render : Address Action -> Model -> H.Html
render channel model = 
    List.map (intSender channel model |> uncurry2) 
        [
            ("Number of Mines", (.numberOfMines), NumberOfMines), ("Width", (.width), Width), ("Height", (.height), Height)]
    |> ((flip (++)) [H.button [HEv.onClick channel Submit] [H.text "OK"]])
    |> H.div []