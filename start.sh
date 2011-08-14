#!/bin/bash
export ERL_LIBS="./deps/:../:${ERL_LIBS}"
exec erl -pa ebin -boot start_sasl -s dreambook -sname sonnik@`hostname` -config priv/example
