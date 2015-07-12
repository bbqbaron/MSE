module NewGame where

import Html as H
import Html.Attributes as HAttr
import Html.Events as HEv
import List
import Signal exposing (Address, message)

import Html.Events.Extra as HEvX

import Type exposing (GameRequest)

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
intSender : Address Action -> String -> (Int -> Action) -> H.Html
intSender channel label actionType =
    H.p [] [
        H.text label,
        H.input [
            actionType >> message channel |> HEv.on "input" HEvX.targetValueInt
        ] []
    ]
    
render : Address Action -> Model -> H.Html
render channel model = 
    List.map (intSender channel |> uncurry) [("Number of Mines", NumberOfMines), ("Width", Width), ("Height", Height)]
    |> ((++) [H.button [HEv.onClick channel Submit] [H.text "OK"]])
    |> H.div []