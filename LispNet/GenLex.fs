module GenLex

open HelperFunctions
open Types

type RegularExpression = Epsilon
                         | Character of char
                         | Or of RegularExpression * RegularExpression
                         | Sequence of RegularExpression * RegularExpression
                         | Star of RegularExpression

type ExtendedRegex = Epsilon
                     | Character of char
                     | All
                     | AllExcept of char list
                     | Range of char * char
                     | String of string
                     | Or of ExtendedRegex * ExtendedRegex
                     | Sequence of ExtendedRegex * ExtendedRegex
                     | Star of ExtendedRegex
                     | Optional of ExtendedRegex
                     | Plus of ExtendedRegex


type Symbol = Epsilon
              | Character of char

type SimpleTransition = { inState: int; inSymbol: Symbol; outState: int list; }

type Transition = Simple of SimpleTransition
                  | Compound  of Transition * Transition

type Nfa = { transition: Transition; start: int; final: int list; }

type Driver = { table: Set<int>[,]; epsilonClosure: (Set<int> -> Set<int>); q0: int; final: int list }

let private allChars = [0..126] |> List.map char

let rec private extendedFromNames namedRegex extendedBindingList =
    match namedRegex with
    | NamedRegex.Epsilon -> ExtendedRegex.Epsilon
    | NamedRegex.Character c -> ExtendedRegex.Character c
    | NamedRegex.All -> ExtendedRegex.All
    | NamedRegex.AllExcept cs -> ExtendedRegex.AllExcept cs
    | NamedRegex.Range (c1, c2) -> ExtendedRegex.Range (c1, c2)
    | NamedRegex.String str -> ExtendedRegex.String str
    | NamedRegex.Name str -> (List.find (fun (name, _) -> name = str) extendedBindingList) |> snd
    | NamedRegex.Or (e1, e2) -> ExtendedRegex.Or (extendedFromNames e1 extendedBindingList, extendedFromNames e2 extendedBindingList)
    | NamedRegex.Sequence (e1, e2) -> ExtendedRegex.Sequence (extendedFromNames e1 extendedBindingList, extendedFromNames e2 extendedBindingList)
    | NamedRegex.Star e -> ExtendedRegex.Star (extendedFromNames e extendedBindingList)
    | NamedRegex.Optional e -> ExtendedRegex.Optional (extendedFromNames e extendedBindingList)
    | NamedRegex.Plus e -> ExtendedRegex.Plus (extendedFromNames e extendedBindingList)

let private buildEnv namedBindingList =
    List.fold (fun acc (name, ne) -> (name, extendedFromNames ne acc)::acc) [] namedBindingList

let private charSeqToOr cs =
    List.fold (fun acc c -> RegularExpression.Or(RegularExpression.Character c, acc) ) (RegularExpression.Character (List.head cs)) (List.tail cs)

let rec private regularFromExtended extRegex =
    match extRegex with
    | ExtendedRegex.Epsilon -> RegularExpression.Epsilon
    | ExtendedRegex.Character c -> RegularExpression.Character c
    | ExtendedRegex.All -> charSeqToOr allChars
    | ExtendedRegex.AllExcept cs -> List.except cs allChars |> charSeqToOr
    | ExtendedRegex.Range (c1, c2) -> [(int c1)..(int c2)] |> List.map char |> charSeqToOr
    | ExtendedRegex.String str -> str.ToCharArray() |> List.ofSeq |> charSeqToOr
    | ExtendedRegex.Or (e1, e2) -> RegularExpression.Or (regularFromExtended e1, regularFromExtended e2)
    | ExtendedRegex.Sequence (e1, e2) -> RegularExpression.Sequence (regularFromExtended e1, regularFromExtended e2)
    | ExtendedRegex.Star e -> RegularExpression.Star (regularFromExtended e)
    | ExtendedRegex.Optional e -> RegularExpression.Or ((regularFromExtended e), RegularExpression.Epsilon)
    | ExtendedRegex.Plus e -> let re = regularFromExtended e in RegularExpression.Or (re, (RegularExpression.Star re))

let mutable private stateNum = 0

let private resetStateNum () =
    stateNum <- 0

let private genStateNum () =
    let current = stateNum
    stateNum <- stateNum + 1
    current

let rec private nfaFromRegex re =
    match re with
    | RegularExpression.Epsilon -> let q0 = genStateNum()
                                   let qf = [genStateNum()]
                                   { Nfa.start = q0;
                                     final = qf;
                                     transition = Transition.Simple { SimpleTransition.inState = q0; inSymbol = Symbol.Epsilon; outState = qf } }
    | RegularExpression.Character c -> let q0 = genStateNum()
                                       let qf = [genStateNum()]
                                       { Nfa.start = q0;
                                         final = qf;
                                         transition = Transition.Simple { SimpleTransition.inState = q0; inSymbol = Symbol.Character c; outState = qf } }
    | RegularExpression.Or (e1, e2) -> 
        let q0 = genStateNum()
        let m1 = nfaFromRegex e1
        let { Nfa.start = m1q0; transition = m1t; final = m1f } = m1
        let m2 = nfaFromRegex e2
        let { Nfa.start = m2q0; transition = m2t; final = m2f } = m2
        let qf = [genStateNum()]
        { Nfa.start = q0;
          final = qf;
          transition = Transition.Compound
                           (Transition.Simple { SimpleTransition.inState = q0; inSymbol = Symbol.Epsilon; outState = [m1q0] },
                              Transition.Compound
                                  (Transition.Simple { SimpleTransition.inState = q0; inSymbol = Symbol.Epsilon; outState = [m2q0] },
                                   Transition.Compound (m1t,
                                       Transition.Compound(m2t,Transition.Compound
                                           (Transition.Simple { SimpleTransition.inState = List.head m1f; inSymbol = Symbol.Epsilon; outState = qf },
                                            Transition.Simple { SimpleTransition.inState = List.head m2f; inSymbol = Symbol.Epsilon; outState = qf }))))) }
    | RegularExpression.Sequence (e1, e2) -> 
        let q0 = genStateNum()
        let m1 = nfaFromRegex e1
        let { Nfa.start = m1q0; transition = m1t; final = m1f } = m1
        let m2 = nfaFromRegex e2
        let { Nfa.start = m2q0; transition = m2t; final = m2f } = m2
        let qf = [genStateNum()]
        { Nfa.start = q0;
          final = qf;
          transition = Transition.Compound
                           (m1t,
                            Transition.Compound
                                (Transition.Simple { SimpleTransition.inState = List.head m1f; inSymbol = Symbol.Epsilon; outState = [m2q0] },
                                 m2t)) }
    | RegularExpression.Star e ->
        let q0 = genStateNum()
        let m1 = nfaFromRegex e
        let { Nfa.start = m1q0; transition = m1t; final = m1f } = m1
        let qf = [genStateNum()]
        { Nfa.start = q0;
          final = qf;
          transition = Transition.Compound
                           (Transition.Simple { SimpleTransition.inState = q0; inSymbol = Symbol.Epsilon; outState = qf },
                            Transition.Compound
                                (m1t,
                                 Transition.Compound
                                     (Transition.Simple { SimpleTransition.inState = q0; inSymbol = Symbol.Epsilon; outState = [m1q0] },
                                      Transition.Compound
                                          (Transition.Simple { SimpleTransition.inState = List.head m1f; inSymbol = Symbol.Epsilon; outState = [m1q0] },
                                           Transition.Simple { SimpleTransition.inState = List.head m1f; inSymbol = Symbol.Epsilon; outState = qf })))) }

let private glueNfas nfas =
    let q0 = genStateNum()
    let f acc { Nfa.start = mq0; transition = mt; } =
        Transition.Compound
            (Transition.Compound
                (Transition.Simple { SimpleTransition.inState = q0; inSymbol = Symbol.Epsilon; outState = [mq0] },
                 mt),
             acc)
    let { Nfa.start = firstQ0; transition = firstT; } = List.head nfas
    let first = Transition.Compound(Transition.Simple { SimpleTransition.inState = q0; inSymbol = Symbol.Epsilon; outState = [firstQ0] }, firstT)
    let transitions = List.fold f first (List.tail nfas)
    let finals =
        nfas
        |> List.map (fun nfa -> nfa.final)
        |> List.concat

    { Nfa.start = q0; transition = transitions; final = finals; }

let private applyTrans transition state sym =
    let rec applyTrans' ts acc =
        match ts with
        | [] -> acc
        | t::rest ->
            match t with
            | Simple { SimpleTransition.inState = ist; inSymbol = isym; outState = osts; } ->
                if ist = state && isym = sym then
                    applyTrans' rest (Set.union acc (set osts))
                else
                    applyTrans' rest acc
            | Compound (t1, t2) -> applyTrans' (t1::(t2::rest)) acc

    applyTrans' [transition] Set.empty

let private arrayFromTransTree delta =
    let table = Array2D.init stateNum (List.length allChars) (fun i1 i2 -> Set.empty)
    let rec arrayFromTransTree' ds =
        match ds with
        | [] -> table
        | d::rest ->
            match d with
            | Simple { SimpleTransition.inState = ist; inSymbol = isym; outState = osts; } ->
                match isym with
                | Symbol.Epsilon -> arrayFromTransTree' rest
                | Symbol.Character c -> Array2D.set table ist (int c) (Set.union (Array2D.get table ist (int c)) (set osts))
                                        arrayFromTransTree' rest
            | Compound (t1, t2) -> arrayFromTransTree' (t1::(t2::rest))
    
    arrayFromTransTree' [delta]

let rec private epsilonClosureHelper deltaFn qs =
    let next = set (deltaFn qs Symbol.Epsilon)
    if qs = (Set.union qs next) then
        qs
    else
        Set.union qs (epsilonClosureHelper deltaFn next)

let private driverFromNfa nfa =
    let { Nfa.start = q0; transition = delta; final = qfs } = nfa
    let table = arrayFromTransTree delta
    let transitionFn qs sym =
        Set.fold (fun acc q -> (applyTrans delta q sym) |> Set.union acc) Set.empty qs
    let memTransitionFn = memoize transitionFn
    let epsilonClosure qs = epsilonClosureHelper memTransitionFn qs
    let memEpsilonClosure = memoize epsilonClosure
    { Driver.table = table;
      epsilonClosure = memEpsilonClosure;
      q0 = q0;
      final = qfs; }

let rec private driverActionHelper table epsilonClosure str initIndex currIndex qs triples =
    if initIndex = ((String.length str) - 1) then
        None
    else
        if currIndex = (String.length str) then
            triples
        else
            let c = str.[currIndex]
            let intC = int c
            let newStates = Set.fold (fun acc q -> (Array2D.get table q intC) |> Set.union acc) Set.empty qs
            let closed = epsilonClosure newStates
            if Set.isEmpty closed then
                triples
            else
                let triples' = Some ((initIndex, currIndex, closed)::(Option.defaultWith (fun _ -> []) triples))
                driverActionHelper table epsilonClosure str initIndex (currIndex + 1) closed triples'

let rec private identifyLexeme triples (str: string) finals (ref: byref<int>) fnEnv =
    match triples with
    | [] -> Token.Failure { FailureToken.src = str; processingIndex = ref; msg = "An error occurred while processing - Empty triples"; } |> Some
    | (initIndex, endIndex, states)::rest ->
        let intersect = (Set.intersect finals states)
        if Set.isEmpty intersect then
            identifyLexeme rest str finals &ref fnEnv
        else
            let minState = Set.minElement intersect
            let matchedFn = List.find (fun (id, _) -> id = minState) fnEnv |> snd
            ref <- endIndex + 1
            matchedFn (str.Substring(initIndex, (endIndex - initIndex + 1)))

let private driverAction driver str eof fnEnv =
    let { Driver.table= table; epsilonClosure = ec; q0 = q0; final = final } = driver
    let finalSet = set final
    let mutable i0 = 0
    let rec fn () =
        let stack = driverActionHelper table ec str i0 i0 (ec (set [q0])) None
        match stack with
        | None -> eof
        | Some s -> let token = identifyLexeme s str finalSet &i0 fnEnv
                    
                    match token with
                    | None -> fn()
                    | Some t -> t
    fn

let private whitespace = ("whitespace", NamedRegex.Range(char 0, char 32))

let makeLexer defs actions =
    let { eof = eof; names = names; patterns = patterns; } = defs
    let nameEnv = buildEnv (whitespace::names)
    resetStateNum() |> ignore
    let rec makeLexer' ps acs nfas fnEnv =
        match ps with
        | [] -> let dr = driverFromNfa (glueNfas nfas)
                fun s -> driverAction dr s eof fnEnv
        | ne::rest ->
            let f = List.head acs
            let nfa = extendedFromNames ne nameEnv |> regularFromExtended |> nfaFromRegex
            // printfn "NE: %A" ne
            // printfn "NFA: %A" nfa

            let { Nfa.final = finals } = nfa
            let finalState = List.head finals
            makeLexer' (rest) (List.tail acs) (nfa::nfas) ((finalState, f)::fnEnv)
    makeLexer' patterns actions [] []