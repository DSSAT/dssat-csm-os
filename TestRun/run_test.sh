#!/bin/bash
# Copy executable
cp ../build/bin/dscsm048 .
cp BIOCHAR.TXT BIOCHAR.INP

# Run DSSAT
# Syntax: ./dscsm048 <ModelCode> <RunMode> <ExperimentFile>
./dscsm048 MZCER048 A TEST01MZ.MZX

# Check outputs
if [ -f "PlantGro.OUT" ]; then
    echo "Simulation completed successfully."
    echo "Outputs generated:"
    ls *.OUT
else
    echo "Simulation failed."
    if [ -f "Error.OUT" ]; then
        cat Error.OUT
    fi
fi
