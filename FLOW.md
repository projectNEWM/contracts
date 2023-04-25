1. A clear user story/intention for what the script is being used for;

An artist comes to NEWM wishing to tokenize a song's royality streaming rights onto the Cardano blockchain. The artist will upload the song information to NEWM which gets transfer to our data verification backend for registry with the legal side of the streaming rights. That official information is given back to NEWM which is then used inside the metadata of the minting transaction for the artist. At this point the tokenization will occur, minting the next sequential NFT from the NEWM catalog. The resulting NFTs will have a token name like "NEWM_0" or "NEWM_414". Typically, the artist will fractionalize their tokenization but the contract does allow for just the NEWM token to be minted. The fractionalization is the splitting of the streaming royality rights. The fractionalization process is splitting the tokenization into 100,000,000 fractions. This is achieve by locking the tokenization into the fractionalization contract which triggers the minting of the fractions. At this point, the fractions are sent to either NEWM, the artist, or to a smart contract for secondary sales.

2. List of expected inputs and outputs for each minting/validation script;

- Tokenization
    - Inputs
        - The NFT locking script UTxO that holds the NEWM_ token.
        - The payee script UTxOs.
        - The collateral which may or not be from the payee.
        - Metadata for the tokenization.
        - NFT locking script reference UTxO.
        - NFT minting script reference UTxO.
        - The mint redeemer.
        - Current datum on the script UTxO that holds the NEWM_ token.
        - The NEWM master key public key hash.
    - Outputs :
        - The payee change.
        - The token output for the tokenization.
        - The returning script UTxO with incremented inline datum.
        - The prep output for the fractionalization contract with correct inline datum.

- Fractionalization
    - Inputs
        - The Locking script UTxO that is prepped to receive the newly minted NEWM token.
        - The payee script UTxOs.
        - The collateral which may or not be from the payee.
        - Metadata for the tokenization.
        - Locking script reference UTxO.
        - Minting script reference UTxO.
        - The lock redeemer.
        - Current datum on the script UTxO prepped for the newly minted NEWM token.
        - The NEWM master key public key hash.
    - Outputs
        - The payee change.
        - The token output for the fractionalization.
        - The returning script UTxO with the same inline datum.


3. Which operations should be accessible to only the master key and which to the multi-sig?
    a) e.g. only NEWM master key can fractionalize tokenized NFT
    b) e.g. only multi-sig (2 of 3) can burn a tokenized NFT

The minting of the tokenization NFT can only be done by the master NEWM key. The burning of a tokenization NFT can only be dont by the multisig. The locking contract determines who has to sign and the minting contracts actually allows either. I assume this works because the locking contract must be spent to burn or mint thus who is the correct signer would have to exist in the tx.

The minting and burning of the fractionalizaton FT can only be done by the master NEWM key. The multisig does not participate in the fractional contract.

The 2 of 3 multisig is used only for the burning of the tokenization NFT.

4. Can you describe the expected workflow when creating a new catalog (new prefix)?

The process of creating a new catalog involves a multisig minting of a new starter token into the NFT locking contract. The starter token will have the same name as the other catalog starter tokens as the token name and policy id are hardcoded into the contract. The multisig agreement will have to ensure that this is a singular token minting transaction with a starting number of zero and a unique prefix inside the datum. This process is a known attack vector though. The multisig could in theory reproduce the same catalog prefix and overwrite all existing NEWM tokenizations. The assumption for this release is the multisig actors are good and getting this to occur will be hard. The multisig minting policy is always open and may always create new catalogs


5. The artist can fractionalize and solidify an NFT back and forth based on the provided info. However, it seems to be the case that the non-artist can fractionalize but not solidify an NFT (even if they own all the fractionalized tokens). Is this intended?

The only allowed minter and burning is the NEWM master key or the multisig for the specific case of the tokenization NFT burn. This is/was intended for this release.