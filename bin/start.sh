#!/bin/bash

RUNNER_SCRIPT_DIR=$(cd ${0%/*} && pwd)
RUNNER_BASE_DIR=${RUNNER_SCRIPT_DIR%/*}

erl -pa deps/*/ebin ${RUNNER_BASE_DIR}/ebin \
    -boot start_sasl \
    -sasl sasl_error_logger '{file, "logs/sasl.log"}' \
    -s hot_code_reload_app \
    -name hot_code_reload@$(hostname)
