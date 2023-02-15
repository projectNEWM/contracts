# Test Suite Instructions

The test suite is designed to run in two terminals on a Cardano testnet. Inside the test-suite folder is the node.env file This file is used to set the network, cli, and socket path. One terminal will be used to run the private testnet nodes and the other will be used for test scripts.

The first step is opening a two terminals. Inside the first terminal run the start_testnet_node.sh file. This will prep the folder for the node and will start a private testnet with 3 stake pool operators. Wait for the node to sync before moving on. The node will print to the console `Congrats! Your network is ready for use!`.

Open the second terminal after the node is synced and ready to use. The tip.sh file can be used to view the current sync process. This is a great way to check that everything is working and the testnet is live.

The second step is creating the testnet wallets using the create_testnet_wallet.sh file. This will auto produce all required wallets and funds for the test scripts. A success text will print to the console upon completion.

The next step is running the main.py file. This will run every test within the TestSuite folder.

Thats it!