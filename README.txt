
I chose to read in the data files as Lazy Text (as versus Haskell Strings). Text is a much more efficient data type when dealing with large files. Additionally, the lazy version means the entire text does not need to be loaded into memory at once (strict Text requires it all in memory at once). 
fusionable

when possible used lower complexity functions like break O(n) instead of breakOn O(n + m) approaches O(n*m)
