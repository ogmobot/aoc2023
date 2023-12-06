Advent of Code 2023
===================
Another year... *another* 25 languages. Let's see how we go this time.

Day 01: Bash (and UNIX text utilities)
--------------------------------------
I've been using shell scripts for years now. Knowing the basics of Bash has let me log debug output, hexdump malfunctioning assembly code, time my Advent of Code solutions, and so on. This is the first time I've actually combined shell utilities together to solve a puzzle. There's so much power in these tiny tools, and I've tapped only a fraction of it here. Perhaps I should try using them to solve problems more often.

My Python solution approaches this problem with regular expressions and a dictionary of english digit names, but I've used a less clever solution here (replacing word names with their matching digits, but not interfering with overlapping words). This seems like a good fit for stream- and text-editing programs. (I also could have piped the whole thing into `awk`, or `python -c`; but that would defeat the purpose, wouldn't it?) I also learned about the `mktemp` program. Originally, I was trying to figure out how to `tee` standard output into two different commands, but this solution seemed cleaner.

**Bash**: shell utilities are greater than the sum of their parts.

**Syntax Highlight**: `|` (pipes the output of one command to the input of another)

Day 02: [CBLang](https://github.com/Ceebox/cbLang)
--------------------------------------------------
This language is really just a thin layer of paint over Python. There are a lot of constructs within the language that I don't really get the purpose of. Each statement must end with a semicolon, and the program won't compile if they're omitted; but multiple semicolon-separated statements on the same line get mangled when the program is transpiled to Python. (It seems the transformation goes `a; b; c;` to `a\n b; c;` to `a\n\tb; c;`.) Curly braces are used to delimit blocks, but the braces *must* be placed on their own lines; the line containing the `{` gets completely removed during the transpiling process. (Skimming through the source code, it looks like some code is transpiled by directly calling `str.replace`, rather than tokenisation.)

The language's one saving grace is that it has all of Python's built-in functions available to it (since function names and list comprehension syntax are apparently untouched by the transpilation process). Without this feature, I don't think I would have had the patience for this language. Even Chadderbox, its author, doesn't make much use of the language; although he has put together one game project in the language, neither that game nor the CBLang transpiler have been touched for almost two years, and his other projects tend to use C#, C++ or TypeScript.

You might notice that the program file uses `crlf` (DOS-style) newline characters. This program is one of the very few I've written from a Windows OS. CBLang is only tested on Windows, but I'm sure it wouldn't be hard at all to run the `ff=unix` command on the program file from within Vim.

**CBLang**: what's the opposite of syntax sugar?

**Syntax Highlight**: `from native reference` (this specific text gets find/replace'd into `import` during transpilation)

Day 03: Smalltalk
-----------------
Wow, now I know what they mean when they say Smalltalk is a *purely* object-oriented language. Even conditionals are object oriented: Booleans have `ifTrue:` and `ifFalse:` methods that take blocks of code as parameters, hence you get stuff like `(a > b) ifTrue: [ 'yes' ] ifFalse: [ 'no' ].`. Similarly, loops tend to be the `do:` method called on collections.

I've not solved this problem in a very efficient way, and Smalltalk isn't known for being fast. Still, it was a fun system to mess around with. Ruby certainly took a huge amount of inspiration from it; I think Smalltalk must have been where declaring variables with `| x |` originated.

I used GNU Smalltalk for this, but it's my understanding that most other implementations come with an IDE that's very helpful for inspecting object properties and methods.

**Smalltalk**: every value is an object and every function is a method.

**Syntax Highlight**: `ifTrue:` (a method implemented by the Boolean class)

Day 04: [Slouch](https://github.com/lukechampine/slouch)
--------------------------------------------------------
This is the second custom programming language I've seen specifically built for competitive programming (the first being [Noulith](https://github.com/betaveros/noulith)). Slouch is certainly well-suited to the task, with built-in functions for common processes like converting a string into a list of integers, or carrying out a breadth-first search.

The language is built around pushing data through a series of composed functions (often seen in C++-inspired languages as `x.f().g().h()`, or OCaML-inspired languages as `x |> f |> g |> h`). In conjuction with the growing standard library, this makes writing programs extraordinarily quick. The Slouch REPL evaluates partially-written results in real time, I feature that I found extraordinarily helpful when trying to figure out the names and arguments of different functions.

Some of the function names were not quite what I expected them to be, but they all make sense. For instance, `prepend` instead of `cons`, `same` instead of `intersection`, `fold1` instead of `reduce`. The version of Slouch that I have downloaded lacks a `pow` function, but based on one of the solutions posted by Luke, he has recently added it to the language.

**Slouch**: solves puzzles fast.

**Syntax Highlight**: `dijkstra` (a function which, given a start point, end point, and a function that maps each point to its neighbours [along with their respective distances], carries out Dijkstra's algorithm to find the shortest path between those two points)

Day 05: Scheme 9 from Empty Space
---------------------------------
LISP is back, baby! S9fES seems like a pretty standard Scheme. It's quick to download, build from source, and install; and comes with a pretty nice library of functions. I had to fiddle with the installation a bit to get it working, though (compile with `-fno-pie`, disable randomised memory addressing with `setarch --addr-no-randomize s9`, set up a new Image file with `s9 -i - -d s9.image`).

The solution I wrote for this puzzle goes wild with the whole functions-as-first-class-values thing (perhaps as a penace for my horrible Guile Scheme program in the 2021 challenge). Every triple of integers that appears in the input file is converted to a function (a `minimapper`); each section is converted to a `mapper` that can push values through all of its `minimappers`; and finally all of the `mappers` are composed together into a single function. All that remains is to apply this function to the input and find the minimum value in the codomain.

**Scheme 9 from Empty Space**: it's just LISP, but designed with the Plan 9 OS in mind.

**Syntax Highlight**: `collect` (a library function that splits a list into sub-lists based on when adjacent elements fail a provided predicate)
