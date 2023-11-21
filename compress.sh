#!/bin/bash

if [ $# -ne 3 ]; then
  echo "Usage: $0 input_video output_video target_size_in_bytes"
  exit 1
fi

input_video="$1"
output_video="$2"
target_size="$3"

# Calculate video duration in seconds
duration=$(ffprobe -i "$input_video" -show_entries format=duration -v quiet -of csv="p=0")

# Calculate video bitrate needed to achieve the target size
bitrate=$(bc -l <<< "($target_size * 8) / $duration")

# Use ffmpeg to compress the video with the calculated bitrate
ffmpeg -i "$input_video" -b:v "$bitrate" "$output_video"

echo "Compressed video saved as $output_video"

