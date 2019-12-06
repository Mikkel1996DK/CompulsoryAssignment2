module WordGuesserGame

open Configuration
open System
open State
open Randomizer
open IO

let checkWin (state: State) =
    let matchGuesses c = state.guessedChars |> List.contains (normalize c)
    String.forall matchGuesses state.word

let getCorrectGuess state =
    state.word
    |> String.filter (fun c ->
        state.guessedChars
        |> List.contains c
        |> not)
    |> Seq.toList
    |> List.head

let gameLoop =
    while true do
        let mutable state = { initialState with word = randomWord() }
        printfn "\nThe length of the word is %i\n" (state.word |> String.length)

        while not state.hasWon do
            printf "%s   Used %A. Guess: " (printWord state) state.guessedChars
            let rawInput = getInput state getCorrectGuess
            printfn ""

            let input =
                if MULTIPLE then
                    rawInput |> List.map normalize
                else
                    if rawInput.Length > 1 then printfn "Only one charcter allowed, taking first"
                    [ rawInput
                      |> List.head
                      |> normalize ]

            state <-
                { state with
                      guessedChars =
                          state.guessedChars
                          |> List.append input
                          |> List.distinct }

            state <-
                { state with
                      hasWon = checkWin state
                      guesses = state.guesses + 1 }

        printfn "\nYou guessed it! Using only %i guesses!" state.guesses
        printfn "The word was: '%s'\n" state.word
