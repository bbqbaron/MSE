module Type where

type alias GameRequest = {
        numberOfMines : Int,
        width : Int,
        height : Int
    }

type Mode = New|Playing