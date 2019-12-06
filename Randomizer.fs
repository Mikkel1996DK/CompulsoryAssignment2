module Randomizer

open System
open Configuration

let random = Random()
let randomIndex = List.length >> random.Next

let randomWord() =
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
