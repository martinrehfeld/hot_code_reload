#!/bin/bash

RUNNER_SCRIPT_DIR=$(cd ${0%/*} && pwd)
RUNNER_BASE_DIR=${RUNNER_SCRIPT_DIR%/*}

cd $RUNNER_BASE_DIR
cp ./src/hcr_config.erl.v2 ./src/hcr_config.erl
cp ./src/hcr_model.erl.v2 ./src/hcr_model.erl

./rebar compile skip_deps=true
