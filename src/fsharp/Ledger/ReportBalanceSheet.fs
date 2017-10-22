module ReportBalanceSheet

/// Report showing balances at various dates. No changes in this report.

open Parse
open Calculations
open InputTypes
open InternalTypes
open Misc
open TextOutput
open PersistentCollections

type Balances = {
        // balances will have one entry for each element in the report's date list.
        Balances : Amount list}

type Line = { Account: InputNameAccount
              Amounts: Balances
              SubAccounts: Line list
              Postings: PostingDetail list}

type Report = { Dates: Date list
                Lines: Line list}

type DatedAccounts = PersistentDictionary<Date, Accounts>

/// Can't I do this without a helper function?
/// XXX: duplicate code.
let extractBalance (a: Account option) =
    match a with
    | None -> zeroAmount
    | Some a -> a.Balance

/// Can't I do this without a helper function?
/// XXX: duplicate code.
let extractSubAccount (a: Account option) (subAccountName: InternalNameAccount)=
    match a with
    | None -> None
    | Some a -> (a.find subAccountName)

/// Diffences between consecutive amounts
/// XXX: duplicate code
let rec differences (amounts: Amount list) =
    let difference (first: Amount) (second: Amount) =
        match first with
            | AUD a -> match second with
                        | AUD b -> (AUD (b - a))
    match amounts with
        | first::second::rest -> (difference first second):: (differences (second::rest))
        | _ -> []

let rec constructReportBalanceSheetLine (accounts : Account option List) (accountTree : AccountNameTree) =
    let balances =
        (List.map (fun (a : Account option) ->
                        match a with
                        | Some account -> account.Balance
                        | None -> zeroAmount)
                  accounts)
    { Account =  InputName (accountTree.Name.[0].Input.Entity, Text.fmt accountTree.Name) //XXX: Ugh, I can't believe I keep doing this. There has to be a better way.......
      Amounts =
          { Balances = balances }
      SubAccounts =
          [ for child in accountTree.Children ->
                (constructReportBalanceSheetLine [ for a in accounts -> (extractSubAccount a child.Name) ] child) ]
      Postings = accountTree.Postings }

let addLine (accounts: DatedAccounts) (dates: Date list) (name: InputNameAccount) linesSoFar =
    let lastDate = (List.max dates)
    let finalAccounts = accounts.[lastDate] in
    match finalAccounts.find(name)  with
        | Some finalAccount ->
            let accountTree = (constructAccountNameTree finalAccount)
            let accountsAtDates = (List.map (fun date -> accounts.[date]) dates)
            let accountNamedInTreeAtDates = (List.map (fun (accounts:Accounts) -> accounts.find(accountTree.Name)) accountsAtDates)
            let newLine = (constructReportBalanceSheetLine accountNamedInTreeAtDates accountTree)
            ( newLine :: linesSoFar)
        | None -> linesSoFar

let generateReport (input: InputFile) (dates: Date list)  =
    let datedAccounts = (accountsByDate input dates)
    let addLine = addLine datedAccounts dates
    let lines = [for account in datedAccounts.[dates|>List.max].Accounts -> account.Value.FullInputName]
                |> List.sortBy (fun x -> x.Entity.AsString)
                |> List.fold (fun (acc:Line list) (elem:InputNameAccount) -> 
                                addLine(elem) acc) []
    {Dates = dates;
     Lines = lines}

let rec printReportLine indent (line : Line) =
    for balance in line.Amounts.Balances do
        printf "%s\t" (Text.fmt balance)    
    for i in 1 .. indent do
        printf " "
    match line.Account with
        (InputName (entity,name)) -> printf "%s\n" name
    for subLine in line.SubAccounts do
        printReportLine (indent+2) subLine

let printReport report =
    (* Balance/Change headings line *)
    if (report.Dates.Length > 0) then
        printf "Balance\t"
        for i in 1 .. (report.Dates.Length-1) do
            printf "\t"    
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
