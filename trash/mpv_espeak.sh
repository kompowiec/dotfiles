#!/bin/bash

# Check if the video file is provided
if [ -z "$1" ]; then
    echo "Usage: $0 <video-file>"
    exit 1
fi

VIDEO_FILE="$1"

# Create a temporary file for subtitles
SUB_FILE=$(mktemp /tmp/mpv_subs.XXXXXX)

# Function to clean up the temporary file
cleanup() {
    rm -f "$SUB_FILE"
}
trap cleanup EXIT

# Run mpv with subtitles extraction
mpv --sub-file="$SUB_FILE" "$VIDEO_FILE" &

# Function to read subtitles using espeak
read_subtitles() {
    local prev_sub=""

    while IFS= read -r line; do
        if [[ "$line" != "$prev_sub" && -n "$line" ]]; then
            espeak "$line"
            prev_sub="$line"
        fi
    done < <(tail -F -n0 "$SUB_FILE")
}

# Extract subtitles using ffmpeg
ffmpeg -i "$VIDEO_FILE" -map 0:s:0 "$SUB_FILE"

# Read the subtitles
read_subtitles

# Wait for mpv to finish
wait
