#!/bin/bash

inputs=$(ls inputs)
outputs=$(ls outputs)
for i in "${!inputs[@]}"; do
    echo -n "Input: "
    cat "inputs/${inputs[i]}"
    echo -n " Expected Output: "
    cat "outputs/${outputs[i]}"
    echo
done
