#!/bin/sh

find . -name "*.el" | xargs emacs -batch -f batch-byte-compile
