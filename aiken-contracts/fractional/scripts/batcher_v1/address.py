#!/usr/bin/python
"""
All cardano-cli address functions required for testing.
"""
import subprocess

def create_script(spend, stake):
    """
    Get the address of a script by file path.
    """
    func = [
        'cardano-cli',
        'address',
        'build',
        '--testnet-magic', '1'
    ]
    if stake == "":
        func += [
            '--payment-script-file', spend
        ]
    else:
        func += [
            '--payment-script-file', spend,
            '--stake-script-file', stake
        ]

    p = subprocess.Popen(func, stdout=subprocess.PIPE).stdout.read().decode('utf-8').rstrip()
    return p


if __name__ == "__main__":
    # staking contract
    stake_script_path="../../contracts/stake_contract.plutus"
    # cip 68 contract
    cip68_script_path="../../contracts/cip68_contract.plutus"
    addr = create_script(cip68_script_path, stake_script_path)
    print(addr)