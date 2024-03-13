#!/bin/bash

# Check if two arguments are given
if [ $# -ne 2 ]; then
  echo "Usage: $0 video_file subtitles_file"
  exit 1
fi

# Assign the arguments to variables
video_file=$1
subtitles_file=$2

# Check if the files exist and are readable
if [ ! -f "$video_file" ] || [ ! -r "$video_file" ]; then
  echo "Video file $video_file does not exist or is not readable"
  exit 2
fi

if [ ! -f "$subtitles_file" ] || [ ! -r "$subtitles_file" ]; then
  echo "Subtitles file $subtitles_file does not exist or is not readable"
  exit 3
fi

# Play the video with mpv in the background
mpv --no-terminal "$video_file" &

# Extract the text from the .srt file and pipe it to espeak
sed -r 's/^[0-9]+|^[0-9:,]+ --> [0-9:,]+|\r$//g' "$subtitles_file" | espeak -v en -s 150

# Wait for the video to finish and kill espeak
wait $!
killall espeak

