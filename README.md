# Minesweeper in [Elm](elm-lang.org)

## What is Elm?
* Language that compiles into JS, HTML, and CSS
* Designed for front-end apps

## Why Elm?
* Strongly Typed
** Haskell-esque
** No nulls
** Types can have parameters
* Pure
** Testing is easier
* Built-in FRP
* More immediately accessible than Haskell

## Functional Primer
* Function definition
* Type signatures
* Function call syntax
* Signals

## A Simple Elm App - By Convention
1. A Model

    How do I represent the state of the app?

    `type alias Model = {name : String}`

1. An initializer

    How do I know where the app starts?

    ```
    init : Model
    init = { name = "Steve "}
    ```

1. An Action type

    How do I define the changes that may occur to app state?

    `type Action = Noop|ChangeName String`

1. An update function

    How do I determine the effects of an Action on a Model?

    ```
    update : Action -> Model -> Model
    update action model =
        case action of
            ChangeName someString -> {model|name<-someString}
            _ -> model
    ```

1. A Mailbox

    What receives Actions and knows when to change app state?

    ```
    updates : Mailbox Action
    updates = mailbox Noop
    ```

    A mailbox provides:

        1. A Signal, which is the latest Action taken

        1. An Address, which is where UI elements, timers, etc can send Actions

1. A Signal of state

    How do I know the _current_ state of the Model, with all the changes that have happened?

    ```
    state : Signal Model
    state = foldp update init updates.signal
    ```

1. A renderer

    How do I transform the current state of the Model into HTML, with event listeners that will capture user interaction?

    ```
    render : Address Action -> Model -> Html
    render channel model = 
        Html.input [Html.Events.on "input" Html.Events.targetValue (\v -> message channel (ChangeName v))] []
    ```

1. A main function

    Zip everything together. Feed app state into the renderer and sit back waiting for Actions.

    ```
    main : Signal Html
    main = render updates.address <~ state
    ```
