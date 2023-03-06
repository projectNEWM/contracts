#!/usr/bin/env bash

/usr/local/bin/cardano-node run \
  --config                          'node/configuration.yaml' \
  --topology                        'node/node-spo2/topology.json' \
  --database-path                   'node/node-spo2/db' \
  --socket-path                     'node/node-spo2/node.sock' \
  --shelley-kes-key                 'node/node-spo2/kes.skey' \
  --shelley-vrf-key                 'node/node-spo2/vrf.skey' \
  --byron-delegation-certificate    'node/node-spo2/byron-delegation.cert' \
  --byron-signing-key               'node/node-spo2/byron-delegate.key' \
  --shelley-operational-certificate 'node/node-spo2/opcert.cert' \
  --port                            3002 \
  | tee -a 'node/node-spo2/node.log'
