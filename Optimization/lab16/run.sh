#!/bin/bash

ISO_TIME=20

N_WORKERS=4
PORT=8082

# Capture the start time
START_TIME=$(date +%s)

# Start the master process in the background
julia master.jl &

# Loop to start the worker processes
for (( i=1; i<=N_WORKERS; i++ ))
do
  julia worker.jl &
done

# Wait for all background processes to finish
wait

# Capture the end time
END_TIME=$(date +%s)

# Calculate the duration
DURATION=$(( END_TIME - START_TIME ))

# Print the duration in seconds
echo "Total execution time: $DURATION seconds"
