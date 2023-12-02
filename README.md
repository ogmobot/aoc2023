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
