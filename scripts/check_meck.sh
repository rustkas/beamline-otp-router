#!/bin/bash
cd /home/rustkas/aigroup/apps/otp/router
erl -pa _build/test/lib/meck/ebin -noshell -eval '
io:format("meck module_info exports: ~p~n", [meck:module_info(exports)]),
io:format("meck which: ~p~n", [code:which(meck)]),
halt().
'

