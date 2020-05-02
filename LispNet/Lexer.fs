module Lexer

open Types
open GenLex

let lexSpec =
    { LexSpec.eof = Token.Eof EofToken;
      names = [ ("upper", NamedRegex.Range('A', 'Z'));
                ("lower", NamedRegex.Range('a', 'z'));
                ("letter", NamedRegex.Or(NamedRegex.Name "upper", NamedRegex.Name "lower"));
                ("letters", NamedRegex.Plus(NamedRegex.Name "letter"));
                ("digit", NamedRegex.Range('0', '9'));
                ("digits", NamedRegex.Plus(NamedRegex.Name "digit")); ];
      patterns = [ NamedRegex.Plus(NamedRegex.Name "whitespace");
                   NamedRegex.String "=";
                   NamedRegex.String ",";
                   NamedRegex.String "+";
                   NamedRegex.String "-";
                   NamedRegex.String "*";
                   NamedRegex.String "/";
                   NamedRegex.Name "letters";
                   NamedRegex.Name "digits"; ]; }

let lexActions = [(fun _ -> None);
                  (fun lexeme -> Token.Empty { EmptyToken.name = "EQ"; lexeme = lexeme } |> Some);
                  (fun lexeme -> Token.Empty { EmptyToken.name = "COMMA"; lexeme = lexeme } |> Some);
                  (fun lexeme -> Token.Empty { EmptyToken.name = "PLUS"; lexeme = lexeme } |> Some);
                  (fun lexeme -> Token.Empty { EmptyToken.name = "MINUS"; lexeme = lexeme } |> Some);
                  (fun lexeme -> Token.Empty { EmptyToken.name = "STAR"; lexeme = lexeme } |> Some);
                  (fun lexeme -> Token.Empty { EmptyToken.name = "SLASH"; lexeme = lexeme } |> Some);
                  (fun lexeme -> Token.Value { ValueToken.name = "ID"; lexeme = lexeme; value = lexeme } |> Some);
                  (fun lexeme -> Token.Value { ValueToken.name = "INT"; lexeme = lexeme; value = int lexeme } |> Some);]

let makeNext = makeLexer lexSpec lexActions
(*
(def lex-spec
'((def-eof (token "" EOF))
  (def-names 
     ((upper (range \A \Z))
      (lower (range \a \z))
      (letter (or upper lower))
      (letters (+ letter))
      (digit (range \0 \9))
      (digits (+ digit))))
  (def-patterns
     ((+ whitespace) ; make-ignore
      "="            ; make-equal-tok
      ","            ; make-comma-tok
      "+"            ; make-plus-tok
      "-"            ; make-minus-tok
      "*"            ; make-star-tok
      "/"            ; make-slash-tok
      letters        ; make-special-id-tok
      digits         ; make-intConst-tok
))))

(def lex-spec-actions
   (list make-ignore
         make-equal-tok
         make-comma-tok
         make-plus-tok
         make-minus-tok
         make-star-tok
         make-slash-tok
         make-special-id-tok
         make-intConst-tok
))

(def make-next (make-lexer lex-spec lex-spec-actions))
*)