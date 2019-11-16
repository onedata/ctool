#!/bin/bash

# @author Wojciech Geisler
# @copyright (C) 2019 ACK CYFRONET AGH
# This script appends error definitions to the hrl, erl and test files.
# The added code must be then manually moved to the correct place in file. 


INCLUDE_PATH=include/errors.hrl
ERL_PATH=src/errors.erl
TEST_PATH=test/errors_tests.erl

MACRO_NAME="$1"

shift
DETAILS=$@

if [ \( -z "$MACRO_NAME" \) -o "$MACRO_NAME" = "-h" -o "$MACRO_NAME" = "--help" ] ; then
    echo "Usage: $0 <macro-name> [details-field, ...]" >&2
    echo "For example: $0 ERROR_NODE_ALREADY_IN_CLUSTER HostnameBin" >&2
    exit 1
fi

# Converts underscore_separated_words to PascalCase
function to_pascal_case {
    echo ${1} | sed -E 's/_(.)/\U\1\E/g'
}

function first_to_lower {
    echo ${1,}
}

LOWER_NAME=${MACRO_NAME,,}
LOWER_NAME=${LOWER_NAME:6} # strip error_
ID=$(to_pascal_case "${LOWER_NAME}")

ARGS=""
if [ -n "$DETAILS" ]; then
    for detail in $DETAILS; do
        ARGS+=', '$detail
    done
    ARGS+=")"
    # strip first ', '
    ARGS="("${ARGS:2}
fi


## write to errors.hrl
{
    printf -- "-define(%s%s, " "$MACRO_NAME" "$ARGS"
    if [ -n "$DETAILS" ]; then
        printf "{error, {%s, %s}}).\n" "$LOWER_NAME" "${ARGS:1:-1}"
    else
        printf "{error, %s}).\n" "$LOWER_NAME"
    fi
} >> "$INCLUDE_PATH"


## write to errors.erl
{
    # add type spec entry
    if [ -n "$DETAILS" ]; then
        printf "| {%s, %s}\n" "$LOWER_NAME" "${ARGS:1:-1}"
    else
        printf "| %s\n" "$LOWER_NAME"
    fi

    # add to_json clause
    printf 'to_json(?%s%s) -> #{\n    <<"id">> => <<"%s">>,\n' \
        "$MACRO_NAME" "$ARGS" "$ID"

    if [ -n "$DETAILS" ]; then
        printf '    <<"details">> => #{\n'
        HAS_PREVIOUS=false
        for detail in $DETAILS; do
            if [ $HAS_PREVIOUS = true ]; then
                printf ',\n'
            fi
            printf '%8s<<"%s">> => %s' '' "$(first_to_lower "${detail}")" "${detail}"
            HAS_PREVIOUS=true
        done
        printf '\n    },\n'
    fi
    printf '    <<"description">> => <<"@FIXME">>\n'
    printf '};\n'

    # add from_json clause
    printf 'from_json(#{<<"id">> := <<"%s">>' "$ID"

    if [ -n "$DETAILS" ]; then
        printf ', <<"details">> := #{'
        HAS_PREVIOUS=false
        for detail in $DETAILS; do
            if [ $HAS_PREVIOUS = true ]; then
                printf ', '
            fi
            printf '<<"%s">> := %s' "$(first_to_lower "${detail}")" "${detail}"
            HAS_PREVIOUS=true
        done
        printf '}'
    fi
    printf '}) ->\n'
    printf '    ?%s%s;\n' "$MACRO_NAME" "$ARGS"


    # add to_http_code clause
    PLACEHOLDERS=$(echo $ARGS | tr -s '[:alnum:]' _)
    printf "to_http_code(?%s%s) -> '@FIXME';\n" "$MACRO_NAME" "$PLACEHOLDERS"
} >> $ERL_PATH


## write to errors_test.erl

LINE_TO_ADD="    ?$MACRO_NAME$ARGS",
# change line ending the test cases list, identified by "]."
sed -i "s/\]\./$LINE_TO_ADD\n\]./g" $TEST_PATH
