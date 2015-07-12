module Ms where

import Html exposing (Html)
import Maybe exposing (Maybe(..))
import Signal exposing ((<~), (~), Address, foldp, forwardTo, Mailbox, mailbox, message, Signal)
import Time exposing (Time)

import Game
import NewGame
import Util exposing (..)

type Mode = New|Playing

type alias Model = {
        game : Maybe Game.Model,
        newGame : NewGame.Model,
        mode : Mode
    }

type Action = Idle|Switch Mode|GameAction Game.Action|NewGameAction NewGame.Action

init : Model
init = {
        newGame = NewGame.init,
        game = Nothing,
        mode = New
    }

update : Action -> Model -> Model
update action model = case action of
    Switch mode -> {model|mode<-mode}
    NewGameAction action -> 
        let (newGame', request) = NewGame.update action model.newGame
            model' = {model|newGame<-newGame'}
        in case request of
            NewGame.CreateGame gameRequest -> {
                model|
                    game<-Just Game.init gameRequest
                    mode<-Playing}
            _ -> model'
    GameAction action -> {model|game<-Game.update action model.game}
    _ -> model

render : Signal.Address Action -> Model -> Html
render channel model = case model.mode of
    New -> NewGame.render (forwardTo channel NewGameAction) model.newGame
    Playing -> Game.render (forwardTo channel GameAction) model.game
    -- BOOM
    _ -> Html.text "Uh...this is embarrassing."

-- top-level stuff

updates : Mailbox Action
updates = mailbox Idle

state : Signal Model
-- worth noting here
-- the fact that signals all enter data from the top does mean that nested components
-- can exercise some control over their parents,
-- a la this addition of a time signal.
-- perhaps i'm doing it wrong?
state = foldp update init (Signal.merge updates.signal ((\_->GameAction Game.Tick) <~ Time.every 1000))

main : Signal Html
main = render updates.address <~ state