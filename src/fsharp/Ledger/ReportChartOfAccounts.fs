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
                                                                                                                                        
let addLine (name: InputNameAccount) (accounts: Accounts) linesSoFar =
    match accounts.find(name)  with
        | Some account -> ((constructLine account) :: linesSoFar)
        | None -> linesSoFar

let generateReport (input: InputFile) =
    let accounts = (Accounts (transactions input))
    {Lines = (addLine (InputName(Default, "Assets")) accounts
             (addLine (InputName(Default, "Liabilities")) accounts
             (addLine (InputName(Default, "Income")) accounts
             (addLine (InputName(Default, "Expenses")) accounts
             (addLine (InputName(Default, "Equity")) accounts [])))))}

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
    for line in report.Lines do
        printLine 0 line