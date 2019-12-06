module IO

open System
open Configuration
open State

let normalize c =
    if CASE_SENSITIVE then c
    else Char.ToLowerInvariant c

let printWord (state: State) =
    let replaceHidden c =
        if state.guessedChars |> List.contains (normalize c) then c
        else HIDDEN
    state.word |> String.map replaceHidden

let rec getInput state getCorrectGuess =
    let key = Console.ReadKey()
    if key.Modifiers = ConsoleModifiers.Control && key.Key = ConsoleKey.H && HELP then [ getCorrectGuess state ]
    elif key.Key = ConsoleKey.Enter then []
    else if key.KeyChar
            |> Char.IsLetterOrDigit
            || (ALLOW_BLANKS && key.KeyChar = ' ') then key.KeyChar :: getInput state getCorrectGuess
    else getInput state getCorrectGuess
