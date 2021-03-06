module Ms where

import Debug exposing (log)
import Html exposing (Html)
import Maybe exposing (Maybe(..))
import Signal exposing ((<~), (~), Address, foldp, forwardTo, Mailbox, mailbox, message, Signal)
import Time exposing (Time)

import Game
import NewGame
import Type exposing (Mode)
import Util exposing (..)

type alias Model = {
        -- state of the game
        game : Game.Model,
        -- state of the newgame menu
        newGame : NewGame.Model,
        -- where are we?
        mode : Mode
        -- i could probably use polymorphism to avoid tracking both at once,
        -- but this is a simple example
    }

type Action = Idle
    -- in-game or new-game?
    |Switch Mode
    -- anything in-game
    |GameAction Game.Action
    -- anything new-game
    |NewGameAction NewGame.Action

init : Model
init = {
        newGame = NewGame.init,
        -- placeholder to avoid unwrapping Maybes just because the game might not exist yet : p
        game = Game.init {numberOfMines=0, width=0, height=0},
        mode = Type.New
    }

update : Action -> Model -> Model
update action model = case action of
    Switch mode -> {model|mode<-mode}
    NewGameAction action -> 
        let (newGame', request) = NewGame.update action model.newGame
            model' = {model|newGame<-newGame'}
        in case request of
            NewGame.CreateGame gameRequest -> {
                model'|
                    game<-Game.init gameRequest,
                    mode<-Type.Playing}
            _ -> model'
    GameAction action ->
        let (game', request) = Game.update action model.game
            model' = {model|game<-game'}
        in case request of
            Just mode -> {model'|mode<-mode}
            _ -> model'
    _ -> model

render : Signal.Address Action -> Model -> Html
render channel model = case model.mode of
    Type.New -> NewGame.render (forwardTo channel NewGameAction) model.newGame
    Type.Playing -> Game.render (forwardTo channel GameAction) model.game
    -- BOOM
    _ -> Html.text "Uh...this is embarrassing."

-- top-level app organization.
-- updates, state, and main will typically only appear at the top level of an app.

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