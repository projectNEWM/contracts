#!/usr/bin/env bash

/usr/local/bin/cardano-node run \
  --config                          'node/configuration.yaml' \
  --topology                        'node/node-spo1/topology.json' \
  --database-path                   'node/node-spo1/db' \
  --socket-path                     'node/node-spo1/node.sock' \
  --shelley-kes-key                 'node/node-spo1/kes.skey' \
  --shelley-vrf-key                 'node/node-spo1/vrf.skey' \
  --byron-delegation-certificate    'node/node-spo1/byron-delegation.cert' \
  --byron-signing-key               'node/node-spo1/byron-delegate.key' \
  --shelley-operational-certificate 'node/node-spo1/opcert.cert' \
  --port                            3001 \
  | tee -a 'node/node-spo1/node.log'
