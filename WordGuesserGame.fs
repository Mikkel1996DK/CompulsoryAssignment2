module WordGuesserGame

open Configuration
open System

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

let random = Random()
let randomIndex = List.length >> random.Next

// Get a random word based on the configuration modules
// "ALLOW_BLANKS" and "WORDS" value.
let randomWord =
    let noWhiteSpace = String.forall (Char.IsWhiteSpace >> not)

    match ALLOW_BLANKS with
    | true ->
        WORDS
        |> randomIndex
        |> List.item
        <| WORDS
    | false ->
        let filteredList = WORDS |> List.filter noWhiteSpace
        match filteredList with
        | [] -> failwith "could not get random word since the wordlist is empty!"
        | l -> List.item (randomIndex l) l

let normalize c =
    if CASE_SENSITIVE then c
    else Char.ToLowerInvariant c

let printWord (state: State) =
    let replaceHidden c =
        if state.guessedChars |> List.contains (normalize c) then c
        else HIDDEN
    state.word |> String.map replaceHidden

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

let rec getInput state =
    let key = Console.ReadKey()
    if key.Modifiers = ConsoleModifiers.Control && key.Key = ConsoleKey.H && HELP then [ getCorrectGuess state ]
    elif key.Key = ConsoleKey.Enter then []
    else if key.KeyChar |> Char.IsLetterOrDigit then key.KeyChar :: getInput state
    else getInput state

let gameLoop =
    while true do
        let mutable state = { initialState with word = randomWord }
        printfn "\nThe length of the word is %i\n" (state.word |> String.length)

        while not state.hasWon do
            //print
            printf "%s   Used %A. Guess: " (printWord state) state.guessedChars

            //play the round
            //get input
            let rawInput = getInput state
            printfn ""

            let input =
                if MULTIPLE then
                    rawInput |> List.map normalize
                else
                    if rawInput.Length > 1 then printfn "Only one charcter allowed, taking first"
                    [ rawInput
                      |> List.head
                      |> normalize ]


            //call recursive fn to process all letters in input
            state <-
                { state with
                      guessedChars =
                          state.guessedChars
                          |> List.append input
                          |> List.distinct }

            //check win
            state <-
                { state with
                      hasWon = checkWin state
                      guesses = state.guesses + 1 }



//show win
//next game
