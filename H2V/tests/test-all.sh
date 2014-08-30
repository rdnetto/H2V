#!/bin/bash
#================================================================================
# USAGE:
# ./test-all.sh                 Run all tests
# ./test-all.sh test.hs         Run one test
# ./test-all.sh -clean          Delete test output
#================================================================================

if [[ "$1" == "-clean" ]]; then
    rm -f *.png *.gv
elif [[ "$1" == "" ]]; then
    for i in *.hs; do
        echo "================================================================================"
        $0 "$i"
    done

else
    FAILED=0
    ../H2V -g $1 || FAILED=1

    if [ $FAILED  == 1 ]; then
        echo -e '\e[1;31mFAILED\e[0m'
    else
        dot -Tpng "${1%.*}.gv" > "${1%.*}.png"
        echo -e '\e[1;32mSUCCESS\e[0m'
    fi
fi

