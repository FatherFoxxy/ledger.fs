﻿module Calculations

open InputTypes
open InternalTypes
open Misc
open PersistentCollections

/// Extract Transaction items
let transactions inputs =
    let rec helper items soFar =
        match items with
        | (Transaction t) :: tail -> helper tail (t :: soFar)
        | (BalanceVerfication _) :: tail -> helper tail soFar
        | (BlankLine :: tail) -> helper tail soFar
        | (Comment _) :: tail -> helper tail soFar
        | [] -> soFar
    List.rev (helper inputs [])

/// Extract BalanceVerfication items
let balanceVerifications inputs =
    let rec helper items soFar =
        match items with
        | (BalanceVerfication b) :: tail -> helper tail (b :: soFar)
        | (Transaction _) :: tail -> helper tail soFar
        | (BlankLine :: tail) -> helper tail soFar
        | (Comment _) :: tail -> helper tail soFar
        | [] -> soFar
    List.rev (helper inputs [])

type DateOrderCheck =
    | OK
    | Problem of previous: Transaction * next: Transaction

type EntityVerification =
    | EntityVerificationPassed
    | UnbalancedEntities of AccountEntity list

/// Check transactions are in date order. Give two problem transactions if not.
let checkDateOrder (transactions : Transaction list) =
    let rec helper (previous : Transaction) (transactions : Transaction list) =
        match transactions with
            | [] -> DateOrderCheck.OK
            | (t :: tail) ->
                if t.date < previous.date then
                    DateOrderCheck.Problem(previous, t)
                else
                    (helper t tail)
    match transactions with
        | [] -> DateOrderCheck.OK
        | (t :: tail) -> (helper t tail)

/// This function will check a list of postings for an uneven number of the entities seen within
/// for example:
///
/// Jacob/Expenses:Motor:Fuel   $58.01
/// Liabilities:Bankwest:Visa   $58.01
///
/// Will result in the following function result:
/// UnbalancedEntities [(Entity Jacob,true); (Default,true)]
///
/// Assuming that they do balanced, like so:
/// Expenses:Motor:Fuel   $58.01
/// Liabilities:Bankwest:Visa   $58.01
///
/// then the function will return EntityVerificationPassed. Perhaps there is a better way to check for the #
/// such as keeping a running total and "modding" (%) by 2 to see if there is an even number of entities
/// I'm keeping it like this because I think the concept of flipping "bits" is neat, although overcomplicated
let verifyEntitiesInPostings (postings:Posting list) =
    let errors = postings
                 |> List.fold (fun (bins:(AccountEntity*bool) list) (elem:Posting) -> 
                         let entity = elem.account.Entity
                         let bin = bins |> List.tryFind (fst >> (=) entity)
                         match bin with
                         | Some b -> bins |> List.map (fun (entity,b) -> (entity,not b))
                         | None -> (entity, true) :: bins) []
                 |> List.filter (snd >> (=) true)
                 |> List.map (fun (entity,b) -> entity)
    match errors with
    | [] -> EntityVerificationPassed
    | _ -> UnbalancedEntities errors

/// Performs verifyEntitiesInPostings of each transaction's postings in the supplied list
/// It will then map the results into a tuple of (Transaction*EntityVerification List)
/// This allows us to refer to the offending transaction in errors, as well as checking each
/// transaction individually rather than checking the ENTIRE file for unbalanced entities
/// this allows for a faster tracking of entity issues
let verifyEntitiesInTransactions (transactions:Transaction list) =
    transactions
    |> List.map (fun transaction -> 
                      (transaction, match verifyEntitiesInPostings transaction.postings with 
                                    | EntityVerificationPassed -> [] 
                                    | UnbalancedEntities entities -> entities))
/// Is transaction unbalanced?
let balance (t:Transaction) =
    let signedAmount (p: Posting) =
        (multAmount (sign (accountType p.account)) p.amount)
    (List.reduce addAmounts (List.map signedAmount t.postings))

let unbalanced (t:Transaction) =
    (balance t) <> zeroAmount

/// Filter transactions by optional (inclusive) dates.
let filter (transactions : Transaction list) (first : Date option) (last : Date option) =
    let transactions = match first with
                        | None -> transactions
                        | Some date -> List.filter (fun t -> t.date >= date) transactions
    let transactions = match last with
                        | None -> transactions
                        | Some date -> List.filter (fun t -> t.date <= date) transactions
    transactions

// Is a a sub-account of b?
let isSubAccountOf (a: InputNameAccount) (b: InputNameAccount) =
    startsWith (canonicalAccountName a) (canonicalAccountName b)

/// XXX: affectedBy(Posting/Transaction) should be a method on AccountName, which should be a class. Do we even need these at all?
let postingAffects (p:Posting) (a: InputNameAccount) =
    (isSubAccountOf p.account a)
/// XXX: affectedBy(Posting/Transaction) should be a method on AccountName, which should be a class. Do we even need these at all?
let transactionAffects (t: Transaction) (a: InputNameAccount) =
    let rec helper postings =
        match postings with
            | p::postings -> match (postingAffects p a) with
                                | true -> true
                                | false -> (helper postings)
            | [] -> false
    (helper t.postings)

/// A tree-structured set of account names. The reason we want this is sometimes we want to deal
/// with a sequence of accounts more or less in parallel, and to visit a particular subset of
/// them. Typically the last set of accounts will contain all the ones we want to visit. Here's
/// a way of specifying the map of accounts we want to traverse.
/// (There must be a better name for this type.)
type AccountNameTree = { Name:     InternalNameAccount;
                         Children: AccountNameTree list;
                         Postings: PostingDetail List}

// Given an account (and it's canonical name), construct the tree of account names rooted at the account.
// Here's the only real trick in the whole system:
// If an account has no postings of its own and exactly one sub-account, we treat the subaccount as the
// account. This produces neater reports - especially in text.
let rec constructAccountNameTree (a: Account) =
    let subAccounts = a.SubAccountsOrderedByInputName
    match subAccounts with
        | [onlyChild] when (a.Postings = PersistentQueue.Empty)
            -> let childTree = (constructAccountNameTree onlyChild)
               {childTree with Name = a.LastName::childTree.Name}
        | _ -> {Name = [a.LastName];
                Children = [for subAccount in subAccounts -> (constructAccountNameTree subAccount)];
                Postings =  (List.ofSeq a.Postings)}

// Construct map where keys are the given dates and values are the "Accounts" at that date.
let accountsByDate (input: InputFile) (dates: Date list) =
    let rec helper (accounts: Accounts) (dates : Date list) (transactions: Transaction list) (soFar:PersistentDictionary<Date,Accounts>) =
        match dates with
        | [] -> soFar
        | date::dates ->
            let transactionsToDate = (List.filter (fun (t:Transaction) -> t.date <= date) transactions)
            let transactionsAfterDate = (List.filter (fun (t:Transaction) -> t.date > date) transactions)
            let accountsAtDate = (accounts.Book transactionsToDate)
            let soFar = soFar.Add(date, accountsAtDate)
            (helper accountsAtDate
                    dates
                    transactionsAfterDate
                    soFar)
    (helper (new Accounts()) (List.sort dates) (transactions input) PersistentDictionary.Empty)

(* XXX: When I add tests for this module, want to ensure that on the account tree constructed from
   the sample transactions we get the following result:

  let a = Accounts(transactions input)

  let tryit rootAccName =
    match a.find(rootAccName) with
    | None -> None
    | Some a -> Some (constructAccountNameTree a rootAccName)

  (tryit "expenses") ==  Some (Node
                                ([{canonical = "expenses";
                                   input = "EXPENSES";}],
                                 [Node ([{canonical = "ELECTRICITY";
                                          input = "Electricity";}],
                                        []);
                                  Node ([{canonical = "FOOD";
                                          input = "Food";};
                                         {canonical = "GROCERIES";
                                          input = "Groceries";}],
                                        []);
                                  Node ([{canonical = "MOTOR";
                                          input = "Motor";};
                                         {canonical = "FUEL";
                                          input = "Fuel";}],
                                        [])]))
 ie: expenses:motor gets converted to motor:fuel, expenses:food gets converted to food:groceries,
 because in both of these, there is only a single sub-account and that sub-account contains all
 the postings to the original account.
 *)
