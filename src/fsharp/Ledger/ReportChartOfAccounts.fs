module ReportChartOfAccounts

/// Report showing structure of accounts.

open Parse
open Calculations
open InputTypes
open InternalTypes
open Misc
open TextOutput
open PersistentCollections

type Line = { Account: InputNameAccount
              SubAccounts: Line list}

type Report = { Lines: Line list }

let rec constructLine (account: Account) =
    let subAccounts = account.SubAccountsOrderedByInputName
    { Account = (toInputName [account.LastName]);
      SubAccounts = [for child in subAccounts ->
                        (constructLine child)]}
                                                                                                                                        
let addLine (accounts: Accounts) (name: InputNameAccount)  linesSoFar =
    match accounts.find(name)  with
        | Some account -> ((constructLine account) :: linesSoFar)
        | None -> linesSoFar

let generateReport (input: InputFile) =
    let accounts = (Accounts (transactions input))
    let addLine = addLine accounts //partial application, one of the greatest features in F#, allows for cleaner code when we're simply passing multiple static variables
    let lines = [for account in accounts.Accounts -> account.Key.AsInputName] 
                |> List.sortBy (fun x -> x.Entity)
                |> List.fold (fun (acc:Line list) (elem:InputNameAccount) -> 
                   addLine elem acc) []
    {Lines=lines}
             
let rec printLine indent (line : Line) =
    for i in 1 .. indent do
        printf " "
    match line.Account with
        (InputName (entity,name)) -> printf "%s\n" name
    for subLine in line.SubAccounts do
        printLine (indent+2) subLine

let printReport report =
    (* Balance/Change headings line *)
    printf "Account\n"
    printf "-------\n"

    let collapsed = 
        report.Lines 
        |> List.map (fun l -> (l.Account.Entity, l)) 
        |> List.collapse

    for (entity,lines) in collapsed do
        printfn "%s/" entity.AsString
        for line in lines do
            printLine 1 line
        printfn ""