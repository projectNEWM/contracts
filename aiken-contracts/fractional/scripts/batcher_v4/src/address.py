import subprocess

from pycardano import Address, Network, VerificationKeyHash


def from_script_files(spend: str, stake: str, constants: dict):
    """Get the address of a script by file path.
    """
    func = [
        'cardano-cli',
        'address',
        'build'
    ]

    # pick out the correct network
    func += constants['network'].split(' ')

    # account for the stake key being empty
    if stake == "":
        func += ['--payment-script-file', spend]
    else:
        func += [
            '--payment-script-file', spend,
            '--stake-script-file', stake
        ]

    p = subprocess.Popen(func, stdout=subprocess.PIPE).stdout
    if p is not None:
        return p.read().decode('utf-8').rstrip()
    return p


def from_pkh_sc(pkh: str, sc: str, network: Network):
    vkh = VerificationKeyHash(bytes.fromhex(pkh))
    if sc == "":
        return Address(payment_part=vkh, network=network).encode()
    else:
        kh = VerificationKeyHash(bytes.fromhex(sc))
        return Address(payment_part=vkh, staking_part=kh, network=network).encode()
