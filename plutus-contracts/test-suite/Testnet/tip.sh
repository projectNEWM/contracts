#!/usr/bin/bash
set -e

source .node.env

${cli} query tip ${network} | jq
