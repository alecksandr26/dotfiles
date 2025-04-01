#!/bin/bash

# Function to get the geometry of the monitor where the cursor is located
get_active_monitor_geometry() {
    # Get mouse position
    eval "$(xdotool getmouselocation --shell)"

    # Iterate over connected monitors and their geometries
    while IFS= read -r line; do
        if [[ $line == *" connected"* ]]; then
            # Extract monitor geometry
            geometry=$(echo "$line" | grep -oP '\d+x\d+\+\d+\+\d+')
            IFS='x+' read -r width height x_offset y_offset <<< "$geometry"

            # Check if the cursor is within this monitor's boundaries
            if (( X >= x_offset && X < x_offset + width && Y >= y_offset && Y < y_offset + height )); then
                echo "$x_offset,$y_offset,$width,$height"
                return
            fi
        fi
    done < <(xrandr --query)
}

# Set a file path
FILE_PATH=~/Pictures/screenshots/screenshot_"$(date +'%Y-%m-%d_%H-%M-%S-%3N')".png

# Check for arguments
if [[ "$1" == "-s" || "$1" == "--select" ]]; then
    # Interactive selection
    scrot -s $FILE_PATH
    notify-send "Screenshot saved (manual selection)!"
else
    # Get the geometry of the active monitor
    monitor_geometry=$(get_active_monitor_geometry)

    # If a monitor is found, capture only that region
    if [[ -n "$monitor_geometry" ]]; then
        scrot -a "$monitor_geometry" $FILE_PATH
        notify-send "Screenshot saved from monitor at $monitor_geometry!"
    else
        notify-send "No monitor detected!"
    fi
fi

# Then copy the picture into the clipboard
xclip -selection clipboard -t image/png -i $FILE_PATH


