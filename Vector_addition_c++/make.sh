#!/bin/bash
path=$(realpath $0)
dir=$(dirname $path)
g++ -o "$dir"/vector_addition "$dir"/vector_addition.cpp -std=c++11
