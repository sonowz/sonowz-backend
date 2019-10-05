#!/bin/sh

ID=$1
CONFIG_STR=$2
RT_PATH=$3
OUTPUT_PATH=$4

cd "$RT_PATH"
echo "$CONFIG_STR" | cat > "RTConf.h"
make &&
./runner &&
eval "cp ./out.png $OUTPUT_PATH$ID.png"
