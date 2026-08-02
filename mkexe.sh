#!/bin/sh
stack install --local-bin-path . && mv trout-exe trout-exe-$1
