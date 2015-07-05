﻿module ParseTest

open NUnit.Framework
open FsUnit
open Parse
open InputTypes

[<TestFixture>]
type ``Test Parsing of transaction text data`` () =
    [<Test>]
    member test.``parseTransactionData.`` () =
        let parse = (parseInputString "2122-22-01 foo\n foo 10AUD\n bar 11\n baz 12\n2012-12-22 foo  sss\nacc $10\n") in do
            parse |> should equal (ParseSuccess ([Transaction {date = "2122-22-01";
                                                              description = "foo";
                                                              postings = [{account = "foo";
                                                                           amount = AUD 1000;};
                                                                          {account = "bar";
                                                                           amount = AUD 1100;};
                                                                          {account = "baz";
                                                                           amount = AUD 1200;}];};
                                                 Transaction {date = "2012-12-22";
                                                              description = "foo  sss";
                                                              postings = [{account = "acc";
                                                                           amount = AUD 1000;}];}]))
    [<Test>]
    member test.``Example from readme.md.`` () =
        let parse = (parseInputString ("""2013-01-01 I began the year with $1000 in my cheque account.
                                               Assets:Bankwest:Cheque      $1,000
                                               Equity:OpeningBalances      $1,000

                                              2013-01-05 I bought some groceries and paid using the cheque account.
                                               Expenses:Food:Groceries    $98.53
                                               Assets:Bankwest:Cheque    -$98.53

                                              2013-01-10 I bought some petrol, and paid using a credit card.
                                               Expenses:Motor:Fuel    $58.01
                                               Liabilities:Bankwest:Visa   $58.01

                                              2013-01-15 I paid my electricity bill.
                                                Expenses:Electricity    $280.42
                                                Assets:Bankwest:Cheque  -$280.42

                                              # I checked my bank statement on the 1st of Feb, and this is what it said.
                                              VERIFY-BALANCE 2013-02-01 Assets:Bankwest:Cheque 621.05""" + "\n"))
        let expected = (ParseSuccess [Transaction {date = "2013-01-01";
                                                              description = "I began the year with $1000 in my cheque account.";
                                                              postings = [{account = "Assets:Bankwest:Cheque";
                                                                           amount = AUD 100000;};
                                                                          {account = "Equity:OpeningBalances";
                                                                           amount = AUD 100000;}];};
                                                 BlankLine;
                                                 Transaction {date = "2013-01-05";
                                                              description = "I bought some groceries and paid using the cheque account.";
                                                              postings = [{account = "Expenses:Food:Groceries";
                                                                           amount = AUD 9853;};
                                                                          {account = "Assets:Bankwest:Cheque";
                                                                           amount = AUD -9853;}];};
                                                 BlankLine;
                                                 Transaction {date = "2013-01-10";
                                                              description = "I bought some petrol, and paid using a credit card.";
                                                              postings = [{account = "Expenses:Motor:Fuel";
                                                                           amount = AUD 5801;};
                                                                          {account = "Liabilities:Bankwest:Visa";
                                                                           amount = AUD 5801;}];};
                                                 BlankLine;
                                                 Transaction {date = "2013-01-15";
                                                              description = "I paid my electricity bill.";
                                                              postings = [{account = "Expenses:Electricity";
                                                                           amount = AUD 28042;};
                                                                          {account = "Assets:Bankwest:Cheque";
                                                                           amount = AUD -28042;}];};
                                                 BlankLine;
                                                 Comment " I checked my bank statement on the 1st of Feb, and this is what it said.";
                                                 BalanceVerfication {date = "2013-02-01";
                                                                     account = "Assets:Bankwest:Cheque";
                                                                     amount = AUD 62105;}]) in do
            parse |> should equal expected
