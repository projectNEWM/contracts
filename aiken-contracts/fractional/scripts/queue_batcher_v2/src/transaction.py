import subprocess

def txid(file_path: str) -> str:
    """
    Get the tx id of a signed transactions.
    """
    func = [
        'cardano-cli',
        'transaction',
        'txid',
        '--tx-file',
        file_path
    ]

    p = subprocess.Popen(func, stdout=subprocess.PIPE).stdout
    if p is not None:
        return p.read().decode('utf-8').rstrip()
    else:
        return ''

def signing_keys(signer_keys):
    """
    Create a list of the signing key files. This can not be empty.
    """
    output = []
    for sk in signer_keys:
        output.append('--signing-key-file')
        output.append(sk)
    return output


def sign(draft_file_path, signed_file_path, network, batcher_skey_path, collat_skey_path):
    """
    Sign a transaction with a list of payment keys.
    """
    func = [
        'cardano-cli',
        'transaction',
        'sign',
        '--tx-body-file',
        draft_file_path,
        '--tx-file',
        signed_file_path,
        '--signing-key-file',
        batcher_skey_path,
        '--signing-key-file',
        collat_skey_path
    ]
    func += network.split(" ")

    # print(func)
    result = subprocess.run(func, capture_output=True, text=True, check=True)
    if result.stderr != "":
        print(result.stderr)
        exit(1)


def submit(signed_file_path, socket_path, network):
    """
    Submit the transaction to the blockchain.
    """
    func = [
        'cardano-cli',
        'transaction',
        'submit',
        '--socket-path', socket_path,
        '--tx-file',
        signed_file_path
    ]
    func += network.split(" ")

    result = subprocess.run(func, stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE, text=True)
    if result.stderr != "":
        pass
        # print('\nERROR:', result.stderr)
    else:
        print(result.stdout.strip())
    return result.stdout.strip()
