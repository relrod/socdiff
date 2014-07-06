# socdiff

A social diff.

socdiff is a simple Haskell application which allows you to visually represent
(diff-style) who follows you or unfollows you on various social media outlets.
Of course, this doesn't matter much, but it gave me a reason to play with
Facebook's awesome Haxl library, and this is what happened. :)

Simply create a Haxl data source for each network that you wish to track, and
reference it in the included runner program.

API docs: http://codeblock.github.io/socdiff/api/

### Examples of running it

#### After following myself with a test account

```
$ socdiff
Fetching Github followers
+ Github:CodeBlock:rublets
Stored /home/ricky/.socdiff_cache
```

#### After unfollowing myself

```
$ socdiff
Fetching Github followers
- Github:CodeBlock:rublets
Stored /home/ricky/.socdiff_cache
```

# License

BSD-2. :) Have fun!
