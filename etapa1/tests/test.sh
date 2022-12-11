#!/bin/bash

SUCCESS=0
FAIL=1

failedTests=0
passedTests=0

runTestScript () {
    script=$1
    expectedOutput=$2

    ((inputCounter++))
    echo "Test $inputCounter"

    expect -c "$script"
    result=$?

    if [ $result -eq $FAIL ]
    then
        echo "TEST FAILED!"
        echo "Expected output: $expectedOutput"
        ((failedTests++))
    else
        echo "SUCCESS!"
        ((passedTests++))
    fi

    echo ""
}

inputs=(inputs/*)
outputs=(outputs/*)
for i in ${!inputs[@]}; do
    input=$(cat "${inputs[i]}")
    input="${input//"["/"\\["}"
    input="${input//"]"/"\\]"}"
    input="${input//"\""/"\\\""}"

    output=$(cat "${outputs[i]}")
    output="${output//"["/"\\["}"
    output="${output//"]"/"\\]"}"
    output="${output//"\""/"\\\""}"

    script='
        set timeout 1

        set input "'$input'"
        set output "'$output'"

        spawn -noecho "../etapa1"
        send_user "Input: "
        send -- "$input\n"

        expect {
            -ex "$output" { exit '"$SUCCESS"' }
            default { exit '"$FAIL"' }
        }

        exit 1
    '

    runTestScript "$script" "$output"
done
echo "Failed: $failedTests"
echo "Passed: $passedTests"
