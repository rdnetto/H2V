#!/bin/bash
#================================================================================
# USAGE:
# ./test-all.sh                 Run all tests
# ./test-all.sh test.hs         Run one test
# ./test-all.sh -clean          Delete test output
#================================================================================

if [[ "$1" == "-clean" ]]; then
    rm -f *.png *.gv *.v
elif [[ "$1" == "" ]]; then
    RESULT=0

    for i in *.hs; do
        if [[ "$i" != "H2V_Compat.hs" ]]; then
            echo "================================================================================"
            $0 "$i"
            RESULT=$(echo "$RESULT + $?" | bc)
        fi
    done

    exit $RESULT

else
    FAILED=0
    ../H2V -g -v $1 || FAILED=1

    if [ $FAILED  == 1 ]; then
        echo -e '\e[1;31mFAILED\e[0m'
        exit 1
    else
        dot -Tpng "${1%.*}.gv" > "${1%.*}.png"
        echo -e '\e[1;32mSUCCESS\e[0m'
    fi
fi

