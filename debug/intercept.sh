#!/bin/sh

# Usage: Add hosts to intercept to /etc/hosts and give each a unique localhost
# address, i.e. 127.0.0.2-254 and add records for real.$domain with the address
# of the real server
#
# A little something like this:
#    127.0.0.254     google.com
#    127.0.0.254     www.google.com
#    127.0.0.253     google.at
#    127.0.0.253     www.google.at
#    127.0.0.252     accounts.google.com
#    127.0.0.251     myaccount.google.com
#
#    64.15.113.30    real.google.com
#    64.15.113.30    real.www.google.com
#    64.15.113.109   real.www.google.at
#    64.15.113.109   real.google.at
#    216.58.211.45   real.accounts.google.com
#    64.15.113.30    real.myaccount.google.com%
#
# Also this script needs root

hosts="www.google.at google.at www.google.com google.com accounts.google.com myaccount.google.com"

pids=$(mktemp /tmp/intercept.pidXXXXXX)

# {www.,}
for h in $hosts; do

    piddir=$(sudo -u dxld mktemp -d /tmp/intercept.pidXXXXXX)

    echo "$piddir" >> "$pids"

    stunnel3 -s dxld -g dxld -P "$piddir/client.pid" -o stunnel.log \
        -c -d "$h":1443 -r real."$h":443
    stunnel3 -s dxld -g dxld -P "$piddir/server.pid" -o stunnel.log \
        -p chain.pem -d "$h":443 -r "$h":1443
done

read asdf

for p in $(cat "$pids"); do
    kill $(cat "$p/server.pid") $(cat "$p/client.pid")
done
