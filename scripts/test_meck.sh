#!/bin/bash
cd /home/rustkas/aigroup/apps/otp/router
erl -pa _build/test/lib/meck/ebin -noshell -eval 'io:format("meck loaded: ~p~n", [code:ensure_loaded(meck)]), halt().'

