#!/bin/sh
UPDATE=../dist/build/update-dataset/update-dataset


IFS='
' # newline :)

sites="$($UPDATE list-sites)"

retrive_date="$(date -u --iso-8601=minutes)"
for s in $sites; do
    site=$(printf '%s' "$s" | awk '{ print $1 }')
    url=$(printf '%s' "$s" | awk '{ print $2 }')

    echo "Getting $site">&2

    mkdir -p "$site/dumps/"

    $UPDATE download "$url" \
        > "$site/dumps/$retrive_date"

done

if which git-annex >/dev/null; then

    for s in $sites; do
        site=$(printf '%s' "$s" | awk '{ print $1 }')
        git annex add "$site/dumps/$retrive_date"
    done

fi
