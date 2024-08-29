#!/bin/bash

set -ex

awk 'BEGIN{block=0} { if ($0 ~ /```/) { block = 0; print("") }; if (block == 0) {print} if ($0 ~ /# answer/) {block=1}}' "$1" > "${1%_answers.qmd}.qmd"