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

Day 06: by hand
---------------
I'm not sure if I'll get the opportunity to solve one of these puzzles like this again! The input is small, and the problem is made substantially easier by the application of some algebra. Specifically, the solution to each column of input data is the difference between the two roots of a quadratic equation. Using $\Delta = b^{2} - 4ac$, the difference is:

$$\frac{-b + \sqrt{\Delta}}{2a} - \frac{-b - \sqrt{\Delta}}{2a} = \frac{\sqrt{\Delta}}{a}$$

... where $a = 1$ and $b$ and $c$ are the two values provided in each column of input data. So, all I needed to do was compute $\sqrt{\Delta} = \sqrt{b^{2} - 4c}$ for each column. (There is one more set of shenanigans to account for the integer-only requirement -- the total number of integers $n$ that lie within the range, excluding the end points, is

$$n = 2\lceil \frac{\sqrt{\Delta}}{2} \rceil - 1 \textrm{ if $b$ is even}$$

$$n = \lceil \frac{\sqrt{\Delta} - 1}{2} \rceil + \lceil \frac{\sqrt{\Delta} + 1}{2} \rceil - 1 \textrm{ if $b$ is odd}$$

I have a marvellous proof for these two equations, but it won't fit within this section of the README.)

I learned how to compute square roots digit-by-digit for part 2 of this puzzle. Squaring one eight-dight number and then finding the square root of a sixteen-digit one was a doozy. I hope to scan a re-written version of this solution in pen at some point, so it's more legible.

**Solving by hand**: look at me. I am the computer now.

**~~Syntax~~ orthographic Highlight**: the square root symbol, $\sqrt{}$, which my solution makes heavy use of

Day 07: [Kona](https://github.com/kevinlawler/kona)
---------------------------------------------------
Kona is an open-source implementation of the commercial programming language k (related to Q and the KDB+ system). Like APL and J, it's an array language; and like J, it's moved towards ASCII rather than specialised symbols. Some of the symbols used by Kona are the same (like `+` and `/`), but others are very different (like `@` and `>`). Familiarity with the style of language helped, I think, but this was nevertheless a difficult program to write. I kept expecting to be using J's verb-trains, but k operates quite differently to J in that regard. Instead, I created a tonne of explicit functions that operated on arguments. (Only a single function definition was point-free.) I found this language easier to write programs in than Uiua, but J is still my preferred array language.

To solve this puzzle, I took advantage of the fact that the finding the frequency of each rank provides a set of lists that can be sorted by element-wise comparison; so four-of-a-kind (4 1 0 0 0) beats full house (3 2 0 0 0) beats two-pair (2 2 1 0 0) beats high card (1 1 1 1 1), *et cetera*. Append this list to the original hand to get an entirely sortable list of hands. Jokers don't get counted as normal; instead, they always add to the most frequent card rank.

**Kona**: it's point-free free, and it's free!

**Syntax Highlight**: `*` (unary/monadic: get the head of a list; binary/dyadic: multiply)

Day 08: CoffeeScript
--------------------
CoffeeScript is a syntactically convenient way of writing JavaScript, itself a very flexible language. The language's syntax feels very straightforward and sparse -- no unnecessary brackets, semicolons, or the like. Comprehensions make it easy to grok how lists are being put together, and as always, the higher-order functions like `map`, `filter` and `forEach` make this JavaScript-oriented language feel very easy to deal with.

This is a puzzle that at first looks extraordinarily difficult. However, Eric has very carefully chosen a much easier special case of the problem (viz. each path starts at "XXA", ends at "XXZ", and then loops) that makes it possible to solve by taking the lowest common multiple of each path length.

**CoffeeScript**: for those who like their coffee with sugar.

**Syntax Highlight**: `->` (separates an anonymous function's arguments from its body)

Day 09: Mercury
---------------
I've seen Mercury described as a mix between Haskell and Prolog. That's not a bad description. The way programs are written in Mercury (i.e. with predicates) feels very, very similar to Prolog; but one of the most noticeable differences is the type annotations of predicates and functions, which is very Haskell-like. It was tough to figure out the exact determinism of each predicate, so I omitted most of the declarations in favour of compiling the program with the `--infer-modes` flag.

|             | 0 possible outputs | 1 possible output | Many possible outputs |
| ----------- | ------------------ | ----------------- | --------------------- |
| Cannot fail | Erroneous          | Deterministic     | Multisolution         |
| May fail    | Failure            | Semideterministic | Nondeterministic      |

My original (hasty) Python solution carried out the algorithm suggested in the problem statement. This program, in contrast, uses binomial coefficients to determine the solution without carrying that algorithm out. I'm not convinced this was a better way to solve it.

**Mercury**: strongly-typed Prolog.

**Syntax Highlight**: `(cond -> A ; B)` (an if-then-else statement that returns `B` if the `cond` predicate fails; this can be used to change a semideterministic predicate into a determistic one)

Day 10: MIPS Assembly
---------------------
One of these days, I'll have to try writing in a contemporary assembly language. Older assembly languages, like MIPS, have myriad emulators that make it much easier to use (for instance, providing pseudo-instructions or subroutines that call `printf("%d")` or `getchar` or `putchar`). I spent some time figuring out how to push and pull values onto the stack via the `$sp` register, but I ended up not needing to use this trick as much as I imagined I would. The program also uses a tiny lookup table to figure out which symbol the `S` in the input should be replaced by. I suspect my program could be refactored a bit to make more use of lookup tables, for example when determining how to change direction when tracing pipes.

One of the differences between MIPS and older assembly languages (e.g. that of the 6502 processor) is the ability to multiply numbers together in hardware. The product of the two selected 32-bit registers is placed into a *pair* of 32-bit integers, since the result may be up to 64 bits wide. This makes finding products a little finicky, but far less so than implementing it in software.

I was surprised to discover that the leaderboard for this star took 30 minutes to fill up. If I wasn't otherwise occupied on the day it was released, I might have made it there myself. From what I've seen online, a lot of people attempted to find the interior area of the shape with a modified search algorithm; but it took me less than ten minutes to find an alternative solution and write it into my program. (Another, even more efficient way to solve it is using the shoelace formula, which I hadn't heard of at the time. This method involves finding the sum of products derived from the shape's vertices, and has the advantage that it can be carried out during the process of solving the first part of the problem. I didn't use that method here, though, since I wanted to avoid using multiplication in my solution.)

Many thanks to James Larus, the author of *SPIM*, without which I would have had a much harder time.

**MIPS Assembly**: hey, you can multiply numbers together in hardware now!

**Syntax Highlight**: `$0` (a register that always contains the value 0)

Day 11: Kitten
--------------
It's been a hot minute since I used a con**cat**enative language. Kitten is a bit unusual for such a language. It uses infix arithmetic and supports locally-scoped variables. Its `if-then-else` blocks work in a way that feels quite un-FORTH-like. I made heavy use of local variables in this program, which probably contributes to its slow runtime. I found the presence of Haskell-like algebraic types helpful in some ways (keeping track of number and position of arguments), but the language's lack of integer type casting was very frustrating. Rather than writing functions that could accept any integral type, I hardcoded a lot of `Int64`s.

Speaking of functions, the language's standard library was lacking a handful of common functions that I ended up implementing by hand: `n take` (the first n items of a list), `xs ys f zip_with` (fold a pair of lists together using a given function) and `haystack needle contains` (determine whether an element is present in a list). In addition, I modified some existing functions to work with `Int64` values, rather than the default `Int32` (`map_index_i64`, `replicate_i64`).

The language is also *slow*. The syntax-checking, type-checking and interpreter startup only take a few seconds, but the run time of the program itself is about 75 minutes (to complete both parts of the problem). This seems very high for an algorithm which is, in theory, $O(n^{2})$ with $n<500$. Could it be that the time complexity of the functions I'm using is higher than I thought? Or, more likely, is it all the overhead incurred from copying local variables around? I'm not sure I want to try to optimise this by using the stack instead of local variable assignments. I feel like this is a language that is almost good, but isn't quite there yet.

My hasty Python solution for this day's task got me to the 31st place on the leaderboard! (My time of 6 minutes and 42 seconds was significantly faster than the runtime of this Kitten solution.)

**Kitten**: it might be good one day.

**Syntax Highlight**: `\f` (syntax sugar for the anonymous function `{ f }`)

Day 12: Ruby
------------
Ruby is a fun little language. Although I haven't used it very much in the past, I feel quite familiar with its syntax because of its similarities with Crystal and Python. (I wonder if I'll regret using it here, instead of on a later day...) One of the reasons I picked Ruby is because I wanted to experiment with the little memoisation pattern that you see in this program. I found a way to use it twice in this solution: once for Part 2 of the problem (for which it's practically mandatory) and one for a little subroutine that creates regular expressions (which can be done without, but cuts the runtime by 30%). I'm not sure that the way I've written and used the Memoised class is very idiomatic of the language -- presumeably it'd be more Ruby-esque to write subclasses that inherit from it, and those subclasses would then solve the problem.

The Ruby interpreter is very slow compared to some of the other languages I've used. My Python implementation of this program takes ~3 seconds, which is just over 10 times faster than my 40-second Ruby solution. Ruby's syntax is nice to work with; but for speed, Crystal would be a much better choice.

**Ruby**: an easy way to solve hard problems (at a gentle pace).

**Syntax Highlight**: `&` (calls the `to_proc` method on the following symbol, effectively letting you create blocks that consist of a single symbol)

Day 13: [Orion](https://github.com/wafelack/orion)
--------------------------------------------------
Orion is a LISP-inspired language developed by a high school student. It compiles to an intermediate representation (ORC) that runs on the Orion VM. I have to say, this is very cool. How does it compare to the other small, hobbyist languages I've tried?

The language has a very small standard library that exists externally to the language itself. Languages like Noulith and Slouch are so specialised that all of their keywords and functions are baked into the language itself; and Uxntal, while it has the ability to import and combine files, doesn't have a standard library used by every project. Languages like Kitten or Factor, on the other hand, are very small languages with very large standard libraries. Some of the functions that would have been nice to see are `filter`, `append`, `zip-with` (or even `zip`), `member?` and `intersection`. The language's syntax and semantics are far enough from most LISPs that moving another language's library over would require a fair bit of work. (For example `(car (Cons x y))` produces `(Just x)`, rather than `x`.)

Some parts of the language don't seem to be working as intended. One particularly egregious example is functions like `>=`, which seem to confuse the compiled representation. Treating functions as data (e.g. passing them as arguments, returning them from functions) also seems bug-prone. The language lacks keywords like `eval` and `apply` that might otherwise make it flexible enough to address these shortcomings. There seems to be no way to write literal negative integers. The language is also pretty sluggish (although this is nothing special for custom interpreted languages like this one). On the plus side, there is a macro system (although I didn't use it in this solution). Its pattern-matching on `Cons` and `Just` are also pretty cool.

I'm glad this language exists -- in fact, I think that every programmer that takes themselves seriously should attempt to create something like this. It was frustrating to work with, though.

**Orion**: proof that you, too, can write your own language.

**Syntax Highlight**: `λ` (or, equivalently, `\`; creates an anonymous function, like the `lambmda` keyword in other LISPs)
