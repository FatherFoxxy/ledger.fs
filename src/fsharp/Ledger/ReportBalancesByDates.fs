﻿module ReportBalancesByDate

/// Report showing balances at various dates and changes between those dates.

/// XXX/TODO: Maybe allow optional account name to restrict report to a single (sub-) account????

open Parse
open Calculations
open InputTypes
open InternalTypes
open Misc
open TextOutput
open PersistentCollections

type BalancesAndDifferences = {
        // balances will have one entry for each element in the report's date list,
        // differences will have an entry for every date but the first.
        Balances : Amount list
        Differences : Amount list}

type Line = { Account:     InputNameAccount
              Amounts:     BalancesAndDifferences
              SubAccounts: Line list
              Postings:    PostingDetail list}

type Report = {Dates: Date list;
               Lines: Line list}

type DatedAccounts = PersistentDictionary<Date, Accounts>

/// Can't I do this without a helper function?
let extractBalance (a: Account option) =
    match a with
    | None -> zeroAmount
    | Some a -> a.Balance

/// Can't I do this without a helper function?
let extractSubAccount (a: Account option) (subAccountName: InternalNameAccount)=
    match a with
    | None -> None
    | Some a -> (a.find subAccountName)

/// Diffences between consecutive amounts
let rec differences (amounts: Amount list) =
    let difference (first: Amount) (second: Amount) =
        match first with
            | AUD a -> match second with
                        | AUD b -> (AUD (b - a))
    match amounts with
        | first::second::rest -> (difference first second):: (differences (second::rest))
        | _ -> []

let rec constructReportBalancesByDateLine (accounts : Account option List) (accountTree : AccountNameTree) =
    let balances =
        (List.map (fun (a : Account option) ->
                        match a with
                        | Some account -> account.Balance
                        | None -> zeroAmount)
                  accounts)
    { Account = accountTree.Name.[0].Input //XXX: Ugh, I can't believe I keep doing this. There has to be a better way.......
      Amounts =
          { Balances = balances
            Differences = (differences balances) }
      SubAccounts =
          [ for child in accountTree.Children ->
                let childAccounts = [ for a in accounts -> (extractSubAccount a child.Name) ]
                (constructReportBalancesByDateLine childAccounts child) ]
      Postings = accountTree.Postings }

let addLine (name: InputNameAccount) (accounts: DatedAccounts) (dates: Date list) linesSoFar =
    let lastDate = (List.max dates)
    let finalAccounts = accounts.[lastDate] in
    match finalAccounts.find(name)  with
        | Some finalAccount ->
            let accountTree = (constructAccountNameTree finalAccount)
            let accountsAtDates = (List.map (fun date -> accounts.[date]) dates)
            let accountNamedInTreeAtDates = (List.map (fun (accounts:Accounts) -> accounts.find(accountTree.Name)) accountsAtDates)
            ((constructReportBalancesByDateLine accountNamedInTreeAtDates accountTree) :: linesSoFar)
        | None -> linesSoFar

let generateReport (input: InputFile) (dates: Date list)  =
    let datedAccounts = (accountsByDate input dates)
    {Dates = dates;
     Lines = (addLine (InputName(Default, "Assets")) datedAccounts dates
             (addLine (InputName(Default, "Liabilities")) datedAccounts dates
             (addLine (InputName(Default, "Income")) datedAccounts dates
             (addLine (InputName(Default, "Expenses")) datedAccounts dates
             (addLine (InputName(Default, "Equity")) datedAccounts dates [])))))}

let rec printReportLine indent (line : Line) =
    for balance in line.Amounts.Balances do
        printf "%s\t" (Text.fmt balance)
    for difference in line.Amounts.Differences do
        printf "%s\t" (Text.fmt difference)
    for i in 1 .. indent do
        printf " "
    printf "%s\n" line.Account.AsString
    for subLine in line.SubAccounts do
        printReportLine (indent+2) subLine

let printReport report =
    (* Balance/Change headings line *)
    if (report.Dates.Length > 0) then
        printf "Balance\t"
        for i in 1 .. (report.Dates.Length-1) do
            printf "\t"
    if (report.Dates.Length > 1) then
        printf "Change"
        for i in 1 .. (report.Dates.Length-2) do
            printf "\t"
        printf "\n"
    (* date/"Account" headings line*)
    for date in report.Dates do
        printf "%s\t" (Text.fmtDate date)
    match report.Dates with
        | first::rest ->
            for date in rest do
                printf "%s\t" date
        | _ -> ()
    printf "Account\n"
    (printf "%s%s-------\n"
        (String.replicate report.Dates.Length "----------\t")
        (String.replicate (report.Dates.Length-1) "----------\t"))
    for line in report.Lines do
        printReportLine 0 line
