#!/usr/bin/env bash

if [[ $# -ne 1 ]]; then
    cat <<EOF
Usage: ./to_sty.sh <source-file>
Converts lines of the form
    ("\\aleph" ?ℵ)
into
    \DeclareUnicodeCharacter{2135}{\aleph}
EOF
    exit 1
fi

while read first rest; do
    # Check and remove the leading question mark.
    [[ $rest =~ ^\\?...$ ]] || continue
    rest=${rest#?}
    # Remove trailing parenthesis.
    rest=${rest%)}
    # Clean first.
    first=${first#(\"}
    first=${first%\"}
    printf '\DeclareUnicodeCharacter{%04X}{%s}\n' "'$rest" "$first"
done < "$1"
