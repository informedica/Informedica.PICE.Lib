echo Restoring dotnet tools...
dotnet tool restore
dotnet paket restore
dotnet fake build -t %*
