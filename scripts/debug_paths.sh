#!/bin/bash
cd /home/rustkas/aigroup/apps/otp/router
erl -pa _build/test/lib/beamline_router/ebin -noshell -eval '
io:format("lib_dir: ~p~n", [code:lib_dir(beamline_router)]),
io:format("paths: ~p~n", [code:get_path()]),
halt().
'

