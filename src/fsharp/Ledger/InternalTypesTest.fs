module InternalTypesTest

open NUnit.Framework
open FsUnit
open Parse
open InputTypes
open InternalTypes
open PersistentCollections


[<TestFixture>]
type ``Test Internal Types`` () =
    [<Test>]
    member test.``splitAccountName.`` () =
        splitAccountName (InputName(Entity "TEST_ENTITY", "Expenses:BankFees:AccountServiceFee"))
        |> should equal [{Canonical = (Canonical (Entity "TEST_ENTITY", "EXPENSE")); Input = (InputName(Entity "TEST_ENTITY", "Expenses"));};
                         {Canonical = (Canonical (Entity "TEST_ENTITY", "BANKFEES")); Input = (InputName(Entity "TEST_ENTITY", "BankFees"));};
                         {Canonical = (Canonical (Entity "TEST_ENTITY", "ACCOUNTSERVICEFEE")); Input = (InputName(Entity "TEST_ENTITY", "AccountServiceFee"));}]
    [<Test>]
    member test.``Book posting to Account.``() =
        /// I think this might even work ... It did - on the first attempt.
        // It's a lot easier to write working code in F# than it is in python.
        let a = Account(InputName(Entity "TEST_ENTITY", "Assets"))
        let p = {Posting.account = (InputName(Entity "TEST_ENTITY", "Assets:Bankwest:Cheque"));
                 Posting.amount = AUD 100000;}
        let t = {Transaction.id = 1;
                 Transaction.date = "1999-12-31";
                 Transaction.postings = [p];
                 Transaction.description = "dummy"}
        let detail = {posting=p;transaction=t}
        let a2 = a.Book(p, t, (List.tail (splitAccountName p.account)))
        a2.SubAccounts.ContainsKey(Canonical (Entity "TEST_ENTITY", "BANKWEST")) |> should be True
        a2.SubAccounts.[Canonical (Entity "TEST_ENTITY", "BANKWEST")].SubAccounts.ContainsKey(Canonical (Entity "TEST_ENTITY", "CHEQUE")) |> should be True
        a2.Balance |> should equal (AUD 100000)
        a2.SubAccounts.[Canonical (Entity "TEST_ENTITY", "BANKWEST")].Balance |> should equal (AUD 100000)
        a2.SubAccounts.[Canonical (Entity "TEST_ENTITY", "BANKWEST")].SubAccounts.[Canonical(Entity "TEST_ENTITY",  "CHEQUE")].Balance |> should equal (AUD 100000)
        a2.Postings |> should equal PersistentQueue.Empty
        a2.SubAccounts.[Canonical (Entity "TEST_ENTITY", "BANKWEST")].Postings |> should equal PersistentQueue.Empty
        a2.SubAccounts.[Canonical (Entity "TEST_ENTITY", "BANKWEST")].SubAccounts.[Canonical (Entity "TEST_ENTITY", "CHEQUE")].Postings |> should equal (PersistentQueue.Empty.Enqueue detail)
    [<Test>]
    member test.``Book posting to Accounts.``() =
        /// And this also seems to work on first attempt.
        let a = Accounts()
        let p = {Posting.account = (InputName(Entity "TEST_ENTITY", "Assets:Bankwest:Cheque"));
                 Posting.amount = AUD 100000;}
        let t = {Transaction.id = 1;
                 Transaction.date = "1999-12-31";
                 Transaction.postings = [p];
                 Transaction.description = "dummy"}
        let detail = {posting=p;transaction=t}
        let a2 = a.Book(p, t)
        a2.Accounts.ContainsKey(Canonical (Entity "TEST_ENTITY", "ASSETS")) |> should be True
        a2.Accounts.[Canonical (Entity "TEST_ENTITY", "ASSETS")].SubAccounts.ContainsKey(Canonical (Entity "TEST_ENTITY", "BANKWEST")) |> should be True
        a2.Accounts.[Canonical (Entity "TEST_ENTITY", "ASSETS")].SubAccounts.[Canonical (Entity "TEST_ENTITY", "BANKWEST")].SubAccounts.ContainsKey(Canonical(Entity "TEST_ENTITY", "CHEQUE")) |> should be True
        a2.Accounts.[Canonical (Entity "TEST_ENTITY", "ASSETS")].Balance |> should equal (AUD 100000)
        a2.Accounts.[Canonical (Entity "TEST_ENTITY", "ASSETS")].SubAccounts.[Canonical (Entity "TEST_ENTITY", "BANKWEST")].Balance |> should equal (AUD 100000)
        a2.Accounts.[Canonical (Entity "TEST_ENTITY", "ASSETS")].SubAccounts.[Canonical (Entity "TEST_ENTITY", "BANKWEST")].SubAccounts.[Canonical (Entity "TEST_ENTITY",  "CHEQUE")].Balance |> should equal (AUD 100000)
        a2.Accounts.[Canonical (Entity "TEST_ENTITY", "ASSETS")].Postings |> should equal PersistentQueue.Empty
        a2.Accounts.[Canonical (Entity "TEST_ENTITY", "ASSETS")].SubAccounts.[Canonical (Entity "TEST_ENTITY", "BANKWEST")].Postings |> should equal PersistentQueue.Empty
        a2.Accounts.[Canonical (Entity "TEST_ENTITY", "ASSETS")].SubAccounts.[Canonical (Entity "TEST_ENTITY", "BANKWEST")].SubAccounts.[Canonical (Entity "TEST_ENTITY", "CHEQUE")].Postings |> should equal (PersistentQueue.Empty.Enqueue detail)
    [<Test>]
    member test.``Book transaction to Accounts.``() =
        // Maybe this is also right first time.
        let t = {Transaction.date = "2013-01-01";
                 description = "I began the year with $1000 in my cheque account.";
                 // NB: Top-level account "Asset" gets created as "ASSET_S_"
                 postings = [{account = (InputName(Entity "TEST_ENTITY", "Asset:Bankwest:Cheque"));
                              amount = AUD 100000;};
                             {account = (InputName(Entity "TEST_ENTITY", "Equity:OpeningBalances"));
                              amount = AUD 100000;}];
             id=1}
        let detail0 = {posting=t.postings.[0];transaction=t}
        let detail1 = {posting=t.postings.[1];transaction=t}
        let a = Accounts()
        let a2 = a.Book(t)
        a2.Accounts.ContainsKey(Canonical (Entity "TEST_ENTITY", "ASSETS")) |> should be True
        a2.Accounts.ContainsKey(Canonical (Entity "TEST_ENTITY", "EQUITY")) |> should be True
        a2.Accounts.[Canonical (Entity "TEST_ENTITY", "ASSETS")].Balance |> should equal (AUD 100000)
        a2.Accounts.[Canonical (Entity "TEST_ENTITY", "EQUITY")].Balance |> should equal (AUD 100000)
        a2.Accounts.[Canonical (Entity "TEST_ENTITY", "ASSETS")].Postings |> should equal (PersistentQueue.Empty)
        a2.Accounts.[Canonical (Entity "TEST_ENTITY", "EQUITY")].Postings |> should equal (PersistentQueue.Empty)
        a2.Accounts.[Canonical (Entity "TEST_ENTITY", "ASSETS")].SubAccounts.[Canonical (Entity "TEST_ENTITY", "BANKWEST")].SubAccounts.[Canonical (Entity "TEST_ENTITY", "CHEQUE")].Postings |> should equal (PersistentQueue.Empty.Enqueue detail0)
        a2.Accounts.[Canonical (Entity "TEST_ENTITY", "EQUITY")].SubAccounts.[Canonical (Entity "TEST_ENTITY", "OPENINGBALANCES")].Postings |> should equal (PersistentQueue.Empty.Enqueue detail1)
