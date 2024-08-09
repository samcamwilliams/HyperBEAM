#!/bin/bash

# Build jiffy
cd deps
git clone https://github.com/davisp/jiffy.git
cd jiffy
make
cd ../..

# Start the server
erl -sname supersu -pa ebin \
    -pa deps/jiffy/_build/default/lib/jiffy/ebin \
    -s make all -s su start