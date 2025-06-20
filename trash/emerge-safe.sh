#!/bin/bash

# Usage: ./emerge-safe.sh @world  or  ./emerge-safe.sh sys-apps/coreutils

TARGET=$@
while true; do
    # Show the next package to be merged
    NEXT=$(emerge --pretend --quiet --emptytree --columns $TARGET | head -n 1)

    if [ -z "$NEXT" ]; then
        echo "No more packages to install. Done!"
        break
    fi

    echo ">>> Installing next package safely: $NEXT"
    emerge --ask --oneshot $(echo $NEXT | awk '{print $1}')

    echo
    echo ">>> Press ENTER to continue to the next package..."
    echo ">>> Or press Ctrl+C now to safely stop."
    read -r
done
