module State

type State =
    { word: string
      guessedChars: char list
      guesses: int
      hasWon: bool }

let initialState =
    { word = ""
      guessedChars = []
      guesses = 0
      hasWon = false }
