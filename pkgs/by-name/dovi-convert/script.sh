#!/usr/bin/env bash

# Directory to scan (defaults to current directory)
DIR="${1:-.}"

run_if_dovi() {
    local file="$1"
    local tmpfile="${file%.*}_new.${file##*.}"

    echo "🎬 Processing Dolby Vision file: $file"

    # Example ffmpeg command (adjust as needed)
    if
        ffmpeg -y -hide_banner -stats -fflags +genpts+igndts -loglevel error -i "$file" -map 0 -bsf:v hevc_metadata=remove_dovi=1 -codec copy -max_muxing_queue_size 2048 -max_interleave_delta 0 -avoid_negative_ts disabled "$tmpfile" >/dev/null 2>&1;
    then
        echo "✅ Success: replacing original file"
        mv -f "$tmpfile" "$file"
    else
        echo "❌ Failed to process $file — keeping original"
        rm -f "$tmpfile"
    fi
}

# Iterate over all files in the directory
while IFS= read -r file; do
    # Use mediainfo to check if the file has Dolby Vision metadata
    if mediainfo "$file" >/dev/null 2>&1 | grep -q -E "Dolby Vision|dvhe|dvh1|dovi"; then
        echo "✅ Dolby Vision detected in: $file"
        run_if_dovi "$file"
    else
        echo "❌ Not Dolby Vision: $file"
    fi
done < <(find "$DIR" -type f)
