@echo off
:: Build the project
dotnet build

:: Check if the build was successful
if %errorlevel% equ 0 (
    echo Build successful. Launching the project...

    :: Run the project
    dotnet run
) else (
    echo Build failed. Please fix the issues before launching the project.
)

:: Pause to keep the console window open
pause
