#!/bin/bash
# This script assumes that the text file with URLs is named urls.txt and is in the same directory as the script
# You can change the file name and path as per your requirement
# The script will download the files in the current directory and name them as file1.ext, file2.ext, file3.ext, etc. where ext is the file extension derived from the URL

# Loop through each line of the text file
count=1
while read -r url; do
  # Extract the file extension from the URL
  ext=${url##*.}
  # Use wget to download the file from the URL
  wget -O file$count.$ext "$url"
  # Increment the count variable
  ((count++))
done < urls.txt

