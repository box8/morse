#!/bin/bash

PA="ebin deps/cowboy/ebin deps/cowlib/ebin deps/ranch/ebin"
erl -config morse.config -pa $PA -run morse
