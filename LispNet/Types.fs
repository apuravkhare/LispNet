module Types

type EmptyToken = { name: string; lexeme: string; }

type ValueToken = { name: string; lexeme: string; value: obj }

type FailureToken = { src: string; processingIndex: int; msg: string; }

type EofToken = EofToken

type Token = Empty of EmptyToken
             | Value of ValueToken
             | Eof of EofToken
             | Failure of FailureToken

type NamedRegex = Epsilon
                  | Character of char
                  | All
                  | AllExcept of char list
                  | Range of char * char
                  | String of string
                  | Name of string
                  | Or of NamedRegex * NamedRegex
                  | Sequence of NamedRegex * NamedRegex
                  | Star of NamedRegex
                  | Optional of NamedRegex
                  | Plus of NamedRegex

type LexSpec = { eof: Token; names: (string * NamedRegex) list; patterns: NamedRegex list; }