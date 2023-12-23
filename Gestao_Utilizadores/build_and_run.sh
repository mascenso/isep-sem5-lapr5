#!/bin/bash

# Build the project
dotnet build

# Check if the build was successful
if [ $? -eq 0 ]; then
    echo "Build successful. Launching the project..."

    # Run the project
    dotnet run
else
    echo "Build failed. Please fix the issues before launching the project."
fi
