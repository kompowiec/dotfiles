#!/bin/bash

# Ask the user for the MIME type
read -p "Enter the MIME type: " mime_type

# Check if the user provided a MIME type
if [ -z "$mime_type" ]; then
    echo "MIME type cannot be empty. Exiting."
    exit 1
fi

# Ask for the input file
read -p "Enter the path to the input file: " input_file

# Check if the input file exists
if [ ! -f "$input_file" ]; then
    echo "Input file does not exist. Exiting."
    exit 1
fi

# Get the base64 encoded content of the input file
base64_content=$(base64 -w 0 "$input_file")

# Create a temporary file to store the MIME type and base64 content
temp_file=$(mktemp)

# Combine the MIME type and base64 content
echo "$mime_type" > "$temp_file"
echo "$base64_content" >> "$temp_file"

# Ask for the output file
read -p "Enter the path for the output file: " output_file

# Write the combined data to the output file
cat "$temp_file" > "$output_file"

# Clean up the temporary file
rm "$temp_file"

echo "File converted to base64 with MIME type and saved to $output_file."

