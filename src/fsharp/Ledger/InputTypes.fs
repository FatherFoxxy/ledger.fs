module InputTypes

// Will start this quick and dirty, and upgrade types as practically required.
//
// This began life as a rewrite of python code, so any static typing here is better than that.

/// XXX: While I'm making the name parts newtypes (single-discriminated-unions), I should probably
/// make Date/Description newtypes too....
type Date = string
type Description = string

type AccountName = string

type AccountEntity = 
    | Default 
    | TEMPENTITY
    | Entity of string
    with
    member this.AsString =
        match this with
        | Default -> "Default"
        | TEMPENTITY -> "TEMPENTITY"
        | Entity e -> e

type InputNameAccount = InputName of (AccountEntity * AccountName)
    with
        member this.AsString =
            match this with (InputName (entity,name)) -> entity.AsString + "/" + name 
        member this.Name =
            match this with (InputName (entity,name)) -> name
        member this.Entity =
            match this with (InputName (entity,name)) -> entity

exception BadAccountNameException of name: InputNameAccount * problem: string

exception EntityMismatch of acc1:InputNameAccount * acc2:InputNameAccount
    with
        member this.ToString =
            "Entity Mismatch: " + this.acc1.Entity.AsString + " <> " + this.acc2.Entity.AsString + "\r\n" +
            "Additional Information: " + this.acc1.AsString + " <> " + this.acc2.AsString + "\r\n" +
            "Stacktrace: " + this.StackTrace

exception BadEntityNameException of name:InputNameAccount * problem: string

type Amount =
    /// AUD amounts are stored as cents, and converted to dollars on input/output.
    | AUD of int

let zeroAmount = AUD 0

/// Add two amounts
let addAmounts (a: Amount) (b: Amount) =
   match (a, b) with
    | (AUD a, AUD b) -> AUD (a+b)

let multAmount (i: int) (a: Amount) =
    match a with
    | AUD a -> AUD (i * a)

let absAmount (a: Amount) =
    match a with
    | AUD a -> AUD (abs a)

type Posting = { account: InputNameAccount
                 amount:  Amount}

type Transaction = { date:    Date
                     description: Description
                     postings: Posting list
                     id: int}

type BalanceVerfication = { date:    Date
                            account: InputNameAccount
                            amount: Amount}

type Input =
    | Transaction of Transaction
    | BalanceVerfication of BalanceVerfication
    | BlankLine
    | Comment of string

type InputFile = Input list
