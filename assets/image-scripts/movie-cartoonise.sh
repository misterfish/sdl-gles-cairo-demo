#!/bin/bash

set -eu
set -o pipefail

bindir=$(realpath "$(dirname "$0")")

. "$bindir"/functions.bash

USAGE="Usage: $0"

mci
mcb gimp --batch-interpreter python-fu-eval
mcb     --batch "pdb.gimp_file_load('frame.png', RUN_INTERACTIVE)"
#mcg

rootdir="$bindir"

file="$rootdir"/movie.mp4

start=0:51
end=1:01

mkchd a
cmd mpf -ao null -vo jpeg "$file" -ss "$start" -endpos "0:10"
