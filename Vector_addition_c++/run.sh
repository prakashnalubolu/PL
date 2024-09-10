#!/bin/bash
path=$(realpath $0)
dir=$(dirname $path)

# Run the program using an absolute path and pass all arguments
"$dir"/vector_addition "$@"

