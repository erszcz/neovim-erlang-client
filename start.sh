#!/bin/sh

erl -sname nvim -pa deps/*/ebin ebin -s sync
