#!/bin/bash
ROOT_DIR="`dirname $0`/.."
export ERL_LIBS="${ROOT_DIR}/deps/:${ROOT_DIR}/..:${ERL_LIBS}"
exec erl -boot start_sasl -s dreambook -sname sonnik@`hostname` -config "${ROOT_DIR}/priv/example"
