module ParseTest

open NUnit.Framework
open FsUnit
open Parse
open InputTypes

[<TestFixture>]
type ``Test Parsing of transaction text data`` () =
    [<Test>]
    member test.``parseTransactionData.`` () =
        let parse = (parseInputString "2122-22-01 foo\n TEST_ENTITY/foo 10AUD\n TEST_ENTITY/bar 11\n TEST_ENTITY/baz 12\n2012-12-22 foo  sss\nTEST_ENTITY/acc $10\n") in do
            parse |> should equal (ParseSuccess ([Transaction {date = "2122-22-01";
                                                               description = "foo";
                                                               postings = [{account = (InputName(Entity "TEST_ENTITY", "foo"));
                                                                            amount = AUD 1000;};
                                                                           {account = (InputName(Entity "TEST_ENTITY", "bar"));
                                                                            amount = AUD 1100;};
                                                                           {account = (InputName(Entity "TEST_ENTITY", "baz"));
                                                                            amount = AUD 1200;}];
                                                               id=1};
                                                 Transaction {date = "2012-12-22";
                                                              description = "foo  sss";
                                                              postings = [{account = (InputName(Entity "TEST_ENTITY","acc"));
                                                                           amount = AUD 1000;}];
                                                              id=2}]))
    [<Test>]
    member test.``Example from readme.md.`` () =
        let parse = (parseInputString ("""2013-01-01 I began the year with $1000 in my cheque account.
                                               TEST_ENTITY/Assets:Bankwest:Cheque      $1,000
                                               TEST_ENTITY/Equity:OpeningBalances      $1,000

                                              2013-01-05 I bought some groceries and paid using the cheque account.
                                               TEST_ENTITY/Expenses:Food:Groceries    $98.53
                                               TEST_ENTITY/Assets:Bankwest:Cheque    -$98.53

                                              2013-01-10 I bought some petrol, and paid using a credit card.
                                               TEST_ENTITY/Expenses:Motor:Fuel    $58.01
                                               TEST_ENTITY/Liabilities:Bankwest:Visa   $58.01

                                              2013-01-15 I paid my electricity bill.
                                                TEST_ENTITY/Expenses:Electricity    $280.42
                                                TEST_ENTITY/Assets:Bankwest:Cheque  -$280.42

                                              # I checked my bank statement on the 1st of Feb, and this is what it said.
                                              VERIFY-BALANCE 2013-02-01 TEST_ENTITY/Assets:Bankwest:Cheque 621.05""" + "\n"))
        let expected = (ParseSuccess [Transaction {date = "2013-01-01";
                                                   description = "I began the year with $1000 in my cheque account.";
                                                   postings = [{account = (InputName (Entity "TEST_ENTITY","Assets:Bankwest:Cheque"));
                                                                amount = AUD 100000;};
                                                               {account = (InputName (Entity "TEST_ENTITY","Equity:OpeningBalances"));
                                                                amount = AUD 100000;}];
                                                    id=1};
                                                 BlankLine;
                                                 Transaction {date = "2013-01-05";
                                                              description = "I bought some groceries and paid using the cheque account.";
                                                              postings = [{account = (InputName(Entity "TEST_ENTITY", "Expenses:Food:Groceries"));
                                                                           amount = AUD 9853;};
                                                                          {account = (InputName(Entity "TEST_ENTITY", "Assets:Bankwest:Cheque"));
                                                                           amount = AUD -9853;}];
                                                              id=2};
                                                 BlankLine;
                                                 Transaction {date = "2013-01-10";
                                                              description = "I bought some petrol, and paid using a credit card.";
                                                              postings = [{account = (InputName(Entity "TEST_ENTITY", "Expenses:Motor:Fuel"));
                                                                           amount = AUD 5801;};
                                                                          {account = (InputName(Entity "TEST_ENTITY", "Liabilities:Bankwest:Visa"));
                                                                           amount = AUD 5801;}];
                                                              id=3};
                                                 BlankLine;
                                                 Transaction {date = "2013-01-15";
                                                              description = "I paid my electricity bill.";
                                                              postings = [{account = (InputName(Entity "TEST_ENTITY", "Expenses:Electricity"));
                                                                           amount = AUD 28042;};
                                                                          {account = (InputName(Entity "TEST_ENTITY", "Assets:Bankwest:Cheque"));
                                                                           amount = AUD -28042;}];
                                                              id=4};
                                                 BlankLine;
                                                 Comment " I checked my bank statement on the 1st of Feb, and this is what it said.";
                                                 BalanceVerfication {date = "2013-02-01";
                                                                     account = (InputName(Entity "TEST_ENTITY", "Assets:Bankwest:Cheque"));
                                                                     amount = AUD 62105;}]) in do
            parse |> should equal expected
