#!/bin/sh
UPDATE=../dist/build/update-dataset/update-dataset

IFS='
' # newline :)

sites="$($UPDATE list-sites)"

for s in $sites; do
    site=$(printf '%s' "$s" | awk '{ print $1 }')

    echo "Updating $site">&2

    for dump in "$site"/dumps/*; do

        $UPDATE locator-from-xpath $(cat "$site/login-form-xpath") \
            < "$dump" \
            > "$dump".login-form-locator

    done

done

if which git-annex >/dev/null; then
fi
