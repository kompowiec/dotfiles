#!/bin/bash

# Mode 1: Mouse down for 135 ms, then up for 135 ms
mode1() {
    while true; do
        xdotool mousedown 1
        sleep 0.135
        xdotool mouseup 1
        sleep 0.135
    done
}

# Mode 2: Mouse down for 80 ms, then up for 80 ms
mode2() {
    while true; do
        xdotool mousedown 1
        sleep 0.08
        xdotool mouseup 1
        sleep 0.08
    done
}

# Prompt user to choose a mode
echo "Choose a mode:"
echo "1: Mouse down for 135 ms, up for 135 ms"
echo "2: Mouse down for 80 ms, up for 80 ms"
read -p "Enter mode (1 or 2): " mode

if [[ $mode == "1" ]]; then
    mode1
elif [[ $mode == "2" ]]; then
    mode2
else
    echo "Invalid mode selected. Exiting."
    exit 1
fi
