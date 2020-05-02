// Learn more about F# at http://fsharp.org

open System
open Types
open Lexer

[<EntryPoint>]
let main argv =
    let file = System.IO.File.ReadAllText "..\\..\\..\\files\\sample-01.txt"
    let nextToken = makeNext file
    printfn "File Content: \n %s" file
    printfn "Lexical Analysis:"
    let rec main' token =
        match token with
        | Token.Failure f -> printfn "%A" token
        | Token.Eof _ -> printfn "Done!"
        | _ -> printfn "%A" token
               main' (nextToken())
        
    main' (nextToken())
    0
