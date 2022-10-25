#!/bin/bash

####################################################################################
### DESCRIPTION
# Runs LC-3 linker inside a docker container
# The folder containing the asm files is mounted inside the container for it to
# have access to the files
####################################################################################

####################################################################################
### USAGE
# 1. lc3linker file1.asm [file2.asm ... fileN.asm] file.obj (all files must be on the same folder)
# 2. lc3linker file.asm (object file will be file.obj)
####################################################################################

if [ $# -lt 1 ]; then
    echo "at least one asm file needs to be specified"
    exit 1
fi

realpath() {
    [[ $1 = /* ]] && echo "$1" || echo "$PWD/${1#./}"
}

####################################################################################
###### Mapping script args into args for the application in the container
####################################################################################
PREFIXED_ARGS=
for f in $*; do
  PREFIXED_ARGS="$PREFIXED_ARGS /data/$(basename "$f")"
done

####################################################################################
###### Determining local folder to be bind mounted to /data inside container
####################################################################################
FILE_PATH=$1
# docker -v requires an absolute path
REAL_PATH=$(realpath "$FILE_PATH")
DIR_PATH=$(dirname "$REAL_PATH")


# shellcheck disable=SC2086
# as we want $PREFIXED_ARGS to split along the whitespaces
docker run --rm -v "$DIR_PATH":/data --name lc3linker fab/lc3linker:0.1.0 $PREFIXED_ARGS

