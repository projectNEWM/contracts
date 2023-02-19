# Test Suite Instructions

The test suite is designed to run in two terminals, one for the private Cardano testnet and the other for running the tests. Inside the test-suite folder is the `node.env` file, the `start_testnet_node` file,  the TestSuite folder, and the Testnet folder. The env file is used to set the network, cli, and socket path. The start testnet file is an autmated script that will build a private testnet and prep the blockchain with the testing environment. The TestSuite folder holds all the testing logic and the Testnet folder holds all the testnet scripts. Inside the Testnet folder is a `balance.sh` and `tip.sh` file. These are helper scripts to check of the current status of the testnet environment.

### TestSuite Structure

The test suite runs a test group, a list of test trees. A test tree is a list of tests. A test is a statement, an assertion, and a function to validate the assertion against. Each function is a transaction built for the Cardano blockchain use the `cardano-cli`. Many transactions can be made to verify the contract, stress test endpoints, and find vulnerabilities to the validation logic.

## Steps

First open two terminals. Inside the first terminal run the `start_testnet_node.sh` file. This will turn that terminal into the testnet being used in this testing process. Wait for the node to start, sync, and prep before moving on. The message `Testnet is ready for testing!` will display when the private testnet is ready.

Open the second terminal and run `python run_tests.py`. This will perform the automated testing of all the tests inside the `TestSuite.Tests` test group.

Thats it!

## Helpers

The `tip.sh` file can be used to view the current sync process. This is a great way to check that everything is working and the testnet is live. The `balance.sh` file queries the utxo distribution for the test wallets. 

There is a test called the `Force Block Delay`. It is design to test if the current block has changed, resulting in a transaction being accepted onto the chain. This test can be used to verify chain progression when the contract depends on a previously validated UTxO, ensuring the transaction hits the chain in time for the next validation to occur.


