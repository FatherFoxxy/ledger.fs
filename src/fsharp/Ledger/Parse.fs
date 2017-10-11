﻿module Parse


open InputTypes
open Misc

open FParsec

// XXX: Yuck! Can I do this without global state!
// Yes: http://www.quanttec.com/fparsec/users-guide/parsing-with-user-state.html
let mutable transactionId = 0
let nextTransactionId () =
    transactionId <- transactionId + 1
    transactionId
let resetTransactionId () =
    transactionId <- 0

type ParseResult =
    | ParseError of string
    | ParseSuccess of InputFile

let nonEolWhiteSpace = " \t"
// At least one space
let pMandatorySpace =
    (skipMany1 (anyOf nonEolWhiteSpace)) <?> "space"
// Optional space
let pOptionalSpace =
    (skipMany (anyOf nonEolWhiteSpace)) <?> "space"

let pEntity =
    let isEntityFirstChar c = isLetter c
    let isEntityChar c = isLetter c || isDigit c || c = '-' || c = '_' || c = '.' //can't use '/' because that's our ending character
    (many1Satisfy2L isEntityFirstChar isEntityChar "entity") .>>? skipString "/"

let pOptEntity =
     opt pEntity
     |>> (fun optEntity -> 
              match optEntity with 
              | Some entity -> Entity entity 
              | None -> Default)

let pAccount =
    let isAccountFirstChar c = isLetter c
    let isAccountChar c = isLetter c || isDigit c || c = '-' || c = '_' || c = '/' || c = ':' || c = '.'
    pipe2 pOptEntity (many1Satisfy2L isAccountFirstChar isAccountChar "account")
            (fun entity account -> InputName(entity, account))

let pAudAmount =
    let isAudAmountFirstChar c = isDigit c || c = '-' || c = '$' || c = '+'
    let isAudAmountChar c = isDigit c || c = '-' || c = '$' || c = '.' || c = ','
    (many1Satisfy2L isAudAmountFirstChar isAudAmountChar "amount")  .>> pOptionalSpace .>> (opt (pstring "AUD"))
    |>> fun s ->
            let s = (stripChars "$," s) in
            AUD (int (round (100.0 * (float s))))

let pAmount =
    pAudAmount

let pYear =
    (pipe4 digit digit digit digit (fun a b c d -> System.String.Concat(Array.ofList([a;b;c;d])))) <?> "year"
let pMonth =
    (pipe2 digit digit (fun a b -> System.String.Concat(Array.ofList([a;b])))) <?> "month"
let pDay =
    (pipe2 digit digit (fun a b -> System.String.Concat(Array.ofList([a;b])))) <?> "day"
let pDate =
    (pipe3 (pYear .>> (pchar '-'))
          (pMonth .>> (pchar '-'))
          (pDay)
          (fun y m d -> (sprintf "%s-%s-%s") y m d)) <?> "date"

let pBlankLine =
    newline
    |>> fun _ -> BlankLine

let pCommentLine =
    (pchar '#') >>. (restOfLine true)
    |>> fun c -> Comment c

let pVerifyBalance =
    pipe3 ((pstring "VERIFY-BALANCE") >>. pMandatorySpace >>. pDate)
          (pMandatorySpace >>. pAccount)
          (pMandatorySpace >>. pAmount .>> pOptionalSpace .>> newline)
        (fun date account amount -> BalanceVerfication { BalanceVerfication.date = date
                                                         BalanceVerfication.account = account
                                                         BalanceVerfication.amount = amount})
let pPosting =
    pipe2 (pOptionalSpace >>. pAccount)
          (pMandatorySpace >>. pAmount .>> pOptionalSpace .>> newline)
        (fun account amount -> { Posting.account = account;
                                 Posting.amount = amount})

let pPostings =
    (many1 (attempt pPosting))

let pTransaction =
    pipe3 pDate
          (pMandatorySpace >>. (restOfLine false) .>> newline)
          pPostings
        (fun date description postings ->
           Transaction { Transaction.date = date;
                         Transaction.description = description;
                         Transaction.postings = postings
                         Transaction.id = nextTransactionId()})

let pInput =
    pOptionalSpace >>. (pCommentLine <|> pBlankLine <|> pVerifyBalance <|> pTransaction)

let pInputs =
    (many pInput)

// Stuff we need to parse a transaction file
let pInputFile =
    pInputs .>> eof

// Top-level parsing routine(s).
let parseInputString str =
    resetTransactionId() // XXX: Yuck! Can I do this without global state!
    // Yes: http://www.quanttec.com/fparsec/users-guide/parsing-with-user-state.html
    match run pInputFile str with
        | Success(result, _, _) -> ParseSuccess(result)
        | Failure(errorMessage, _, _) -> ParseError(errorMessage)

let readInputFile filename =
    (parseInputString (System.IO.File.ReadAllText filename))
