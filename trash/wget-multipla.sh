#!/bin/bash
# This script assumes that the text file with URLs is named urls.txt and is in the same directory as the script
# You can change the file name and path as per your requirement
# The script will download the files in the current directory and name them as file1, file2, file3, etc.

# Loop through each line of the text file
count=1
while read -r url; do
  # Use wget to download the file from the URL
  wget -O file$count "$url"
  # Increment the count variable
  ((count++))
done < urls.txt

