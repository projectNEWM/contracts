#!/usr/bin/env bash

/usr/local/bin/cardano-node run \
  --config                          'node/configuration.yaml' \
  --topology                        'node/node-spo3/topology.json' \
  --database-path                   'node/node-spo3/db' \
  --socket-path                     'node/node-spo3/node.sock' \
  --shelley-kes-key                 'node/node-spo3/kes.skey' \
  --shelley-vrf-key                 'node/node-spo3/vrf.skey' \
  --byron-delegation-certificate    'node/node-spo3/byron-delegation.cert' \
  --byron-signing-key               'node/node-spo3/byron-delegate.key' \
  --shelley-operational-certificate 'node/node-spo3/opcert.cert' \
  --port                            3003 \
  | tee -a 'node/node-spo3/node.log'
