#!/bin/bash

# Directory containing image files (default: current directory)
DIR="${1:-.}"

# Loop through all image files in the directory
for file in "$DIR"/*.{jpg,jpeg,png,gif,webp}; do
    # Skip if no files match the pattern
    [ -e "$file" ] || continue
    
    echo "Checking: $file"
    
    # Get file extension
    ext="${file##*.}"
    
    if [[ "$ext" == "jxl" ]]; then
        echo "File is already in JXL format. Skipping..."
    else
        output_file="${file%.*}.jxl"
        echo "Converting to JXL: $output_file"
        cjxl "$file" "$output_file" --lossless
    fi
    
    echo "--------------------------------------"
done
