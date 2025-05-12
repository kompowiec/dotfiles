#!/bin/bash

# Directory containing video files (default: current directory)
DIR="${1:-.}"

# Loop through all video files in the directory
for file in "$DIR"/*.{mp4,mkv,webm}; do
    # Skip if no files match the pattern
    [ -e "$file" ] || continue
    
    echo "Checking: $file"
    
    # Get video and audio codec information
    video_codec=$(ffprobe -v error -select_streams v:0 -show_entries stream=codec_name -of csv=p=0 "$file")
    audio_codec=$(ffprobe -v error -select_streams a:0 -show_entries stream=codec_name -of csv=p=0 "$file")
    
    echo "Video Codec: $video_codec"
    echo "Audio Codec: $audio_codec"
    
    if [[ "$video_codec" == "av1" && "$audio_codec" == "opus" ]]; then
        echo "File is already in AV1/Opus format. Skipping..."
    else
        output_file="converted_${file##*/}"
        echo "Converting to AV1 and Opus: $output_file"
        ffmpeg -i "$file" -c:v libaom-av1 -crf 30 -b:v 0 -c:a libopus -b:a 128k "$output_file"
    fi
    
    echo "--------------------------------------"
done
