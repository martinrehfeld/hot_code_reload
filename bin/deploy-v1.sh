#!/bin/bash -e

RUNNER_SCRIPT_DIR=$(cd ${0%/*} && pwd)
RUNNER_BASE_DIR=${RUNNER_SCRIPT_DIR%/*}

cd $RUNNER_BASE_DIR
cp ./src/hcr_config.erl.v1 ./src/hcr_config.erl
cp ./src/hcr_model.erl.v1 ./src/hcr_model.erl

./rebar compile skip_deps=true
echo "Reloading code in node:"
exec erl_call -a "hcr_deploy reload_app []" -n hot_code_reload
