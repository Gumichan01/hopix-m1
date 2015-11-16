#!/usr/bin/env bash

# Automatically import parameters from hackojo-param.sh (or hackojo-param.sh.template if the former does not exist)
# Login to the Dojo

set -e

root="$(dirname $0)"

if [ ! -e "$root/hackojo-param.sh" ]
then
    cp "$root/hackojo-param.sh.template" "$root/hackojo-param.sh"
fi

source "$root/hackojo-param.sh"

if $HJC whoami | grep -q logged_as; then
    :
else

    if [ "x$PASSWORD" = "xunset" ]; then
        $HJC login --dojo $HACKOJO
    else
        $HJC login --dojo $HACKOJO --username $LOGIN --password $PASSWORD
    fi

fi
