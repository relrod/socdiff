# socdiff

A social diff.

socdiff is a simple Haskell application which allows you to visually represent
(diff-style) who follows you or unfollows you on various social media outlets.
Of course, this doesn't matter much, but it gave me a reason to play with
Facebook's awesome Haxl library, and this is what happened. :)

Simply create a Haxl data source for each network that you wish to track.

Eventually, there will be a small runner script that will actually use the data
sources. The workflow will look like this:

* Run the program for the first time, and a file is placed in
  `~/.socdiff_cache` after checking all of the provided social network data
  sources.
* The format of the file is simple: `networkname:username`, e.g.
  `github:CodeBlock`
* The next time you run the program this file is read and parsed, the data
  sources are checked again, and a diff is generated and rendered with pretty
  colors and whatnot :P
* `~/.socdiff_cache` then gets updated with the newly obtained information.

# License

BSD-2. :) Have fun!
