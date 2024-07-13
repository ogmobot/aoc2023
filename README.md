Advent of Code 2023
===================
Another year... *another* 25 languages. Let's see how we go this time.

Day 01: [Bash](https://www.gnu.org/software/bash/manual/bash.html) (and UNIX text utilities)
--------------------------------------------------------------------------------------------
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

Day 03: [Smalltalk](https://www.gnu.org/software/smalltalk/manual/gst.html)
---------------------------------------------------------------------------
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

Day 05: [Scheme 9 from Empty Space](https://t3x.org/s9fes)
----------------------------------------------------------
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

I have discovered a truly marvellous proof for this, which this section of the README is not large enough to contain.)

I learned how to compute square roots digit-by-digit for part 2 of this puzzle. Squaring one eight-dight number and then finding the square root of a sixteen-digit one was a doozy. My original work was in pencil, but I've scanned in an inked version for better legibility. (When re-writing the solution in pen, I left out a few minor calculations, such as squaring two-digit numbers.)

**Solving by hand**: look at me. I am the computer now.

**~~Syntax~~ Orthographic Highlight**: the square root symbol, $\sqrt{}$, which my solution makes heavy use of

Day 07: [Kona](https://github.com/kevinlawler/kona)
---------------------------------------------------
Kona is an open-source implementation of the commercial programming language k (related to Q and the KDB+ system). Like APL and J, it's an array language; and like J, it's moved towards ASCII rather than specialised symbols. Some of the symbols used by Kona are the same (like `+` and `/`), but others are very different (like `@` and `>`). Familiarity with the style of language helped, I think, but this was nevertheless a difficult program to write. I kept expecting to be using J's verb-trains, but k operates quite differently to J in that regard. Instead, I created a tonne of explicit functions that operated on arguments. (Only a single function definition was point-free.) I found this language easier to write programs in than Uiua, but J is still my preferred array language.

To solve this puzzle, I took advantage of the fact that the finding the frequency of each rank provides a set of lists that can be sorted by element-wise comparison; so four-of-a-kind (4 1 0 0 0) beats full house (3 2 0 0 0) beats two-pair (2 2 1 0 0) beats high card (1 1 1 1 1), *et cetera*. Append this list to the original hand to get an entirely sortable list of hands. Jokers don't get counted as normal; instead, they always add to the most frequent card rank.

**Kona**: it's point-free free, and it's free!

**Syntax Highlight**: `*` (unary/monadic: get the head of a list; binary/dyadic: multiply)

Day 08: [CoffeeScript](https://coffeescript.org)
------------------------------------------------
CoffeeScript is a syntactically convenient way of writing JavaScript, itself a very flexible language. The language's syntax feels very straightforward and sparse -- no unnecessary brackets, semicolons, or the like. Comprehensions make it easy to grok how lists are being put together, and as always, the higher-order functions like `map`, `filter` and `forEach` make this JavaScript-oriented language feel very easy to deal with.

This is a puzzle that at first looks extraordinarily difficult. However, Eric has very carefully chosen a much easier special case of the problem (viz. each path starts at "XXA", ends at "XXZ", and then loops) that makes it possible to solve by taking the lowest common multiple of each path length.

**CoffeeScript**: for those who like their coffee with sugar.

**Syntax Highlight**: `->` (separates an anonymous function's arguments from its body)

Day 09: [Mercury](https://mercurylang.org)
------------------------------------------
I've seen Mercury described as a mix between Haskell and Prolog. That's not a bad description. The way programs are written in Mercury (i.e. with predicates) feels very, very similar to Prolog; but one of the most noticeable differences is the type annotations of predicates and functions, which is very Haskell-like. It was tough to figure out the exact determinism of each predicate, so I omitted most of the declarations in favour of compiling the program with the `--infer-modes` flag.

|             | 0 possible outputs | 1 possible output | Many possible outputs |
| ----------- | ------------------ | ----------------- | --------------------- |
| Cannot fail | Erroneous          | Deterministic     | Multisolution         |
| May fail    | Failure            | Semideterministic | Nondeterministic      |

My original (hasty) Python solution carried out the algorithm suggested in the problem statement. This program, in contrast, uses binomial coefficients to determine the solution without carrying that algorithm out. I'm not convinced this was a better way to solve it.

**Mercury**: strongly-typed Prolog.

**Syntax Highlight**: `(cond -> A ; B)` (an if-then-else statement that returns `B` if the `cond` predicate fails; this can be used to change a semideterministic predicate into a determistic one)

Day 10: [MIPS Assembly](https://sourceforge.net/projects/spimsimulator)
-----------------------------------------------------------------------
One of these days, I'll have to try writing in a contemporary assembly language. Older assembly languages, like MIPS, have myriad emulators that make it much easier to use (for instance, providing pseudo-instructions or subroutines that call `printf("%d")` or `getchar` or `putchar`). I spent some time figuring out how to push and pull values onto the stack via the `$sp` register, but I ended up not needing to use this trick as much as I imagined I would. The program also uses a tiny lookup table to figure out which symbol the `S` in the input should be replaced by. I suspect my program could be refactored a bit to make more use of lookup tables, for example when determining how to change direction when tracing pipes.

One of the differences between MIPS and older assembly languages (e.g. that of the 6502 processor) is the ability to multiply numbers together in hardware. The product of the two selected 32-bit registers is placed into a *pair* of 32-bit integers, since the result may be up to 64 bits wide. This makes finding products a little finicky, but far less so than implementing it in software.

I was surprised to discover that the leaderboard for this star took 30 minutes to fill up. If I wasn't otherwise occupied on the day it was released, I might have made it there myself. From what I've seen online, a lot of people attempted to find the interior area of the shape with a modified search algorithm; but it took me less than ten minutes to find an alternative solution and write it into my program. (Another, even more efficient way to solve it is using the shoelace formula, which I hadn't heard of at the time. This method involves finding the sum of products derived from the shape's vertices, and has the advantage that it can be carried out during the process of solving the first part of the problem. I didn't use that method here, though, since I wanted to avoid using multiplication in my solution.)

Many thanks to James Larus, the author of *SPIM*, without which I would have had a much harder time.

**MIPS Assembly**: hey, you can multiply numbers together in hardware now!

**Syntax Highlight**: `$0` (a register that always contains the value 0)

Day 11: [Kitten](https://kittenlang.org)
----------------------------------------
It's been a hot minute since I used a con**cat**enative language. Kitten is a bit unusual for such a language. It uses infix arithmetic and supports locally-scoped variables. Its `if-then-else` blocks work in a way that feels quite un-FORTH-like. I made heavy use of local variables in this program, which probably contributes to its slow runtime. I found the presence of Haskell-like algebraic types helpful in some ways (keeping track of number and position of arguments), but the language's lack of integer type casting was very frustrating. Rather than writing functions that could accept any integral type, I hardcoded a lot of `Int64`s.

Speaking of functions, the language's standard library was lacking a handful of common functions that I ended up implementing by hand: `n take` (the first n items of a list), `xs ys f zip_with` (fold a pair of lists together using a given function) and `haystack needle contains` (determine whether an element is present in a list). In addition, I modified some existing functions to work with `Int64` values, rather than the default `Int32` (`map_index_i64`, `replicate_i64`).

The language is also *slow*. The syntax-checking, type-checking and interpreter startup only take a few seconds, but the run time of the program itself is about 75 minutes (to complete both parts of the problem). This seems very high for an algorithm which is, in theory, $O(n^{2})$ with $n<500$. Could it be that the time complexity of the functions I'm using is higher than I thought? Or is it all the overhead incurred from copying local variables around? The latter seems more likely to me, but I'm not sure I want to try to optimise this by using the stack instead of local variable assignments. I feel like this is a language that is almost good, but isn't quite there yet.

My hasty Python solution for this day's task got me to the 31st place on the leaderboard! (My time of 6 minutes and 42 seconds was significantly faster than the runtime of this Kitten solution.)

**Kitten**: it might be good one day.

**Syntax Highlight**: `\f` (syntax sugar for the anonymous function `{ f }`)

Day 12: [Ruby](https://ruby-lang.org)
-------------------------------------
Ruby is a fun little language. Although I haven't used it very much in the past, I feel quite familiar with its syntax because of its similarities with Crystal and Python. (I wonder if I'll regret using it here, instead of on a later day...) One of the reasons I picked Ruby is because I wanted to experiment with the little memoisation pattern that you see in this program. I found a way to use it twice in this solution: once for Part 2 of the problem (for which it's practically mandatory) and one for a little subroutine that creates regular expressions (which can be done without, but cuts the runtime by 30%). I'm not sure that the way I've written and used the Memoised class is very idiomatic of the language -- presumeably it'd be more Ruby-esque to write subclasses that inherit from it, and those subclasses would then solve the problem.

The Ruby interpreter is very slow compared to some of the other languages I've used. My Python implementation of this program takes ~3 seconds, which is just over 10 times faster than my 40-second Ruby solution. Ruby's syntax is nice to work with; but for speed, Crystal would be a much better choice.

**Ruby**: an easy way to solve hard problems (at a gentle pace).

**Syntax Highlight**: `&` (calls the `to_proc` method on the following symbol, effectively letting you create blocks that consist of a single symbol)

Day 13: [Orion](https://github.com/wafelack/orion)
--------------------------------------------------
Orion is a LISP-inspired language developed by a high school student. It compiles to an intermediate representation (ORC) that runs on the Orion VM. I have to say, this is very cool. How does it compare to the other small, hobbyist languages I've tried?

The language has a very small standard library that exists externally to the language itself. Languages like Noulith and Slouch are so specialised that all of their keywords and functions are baked into the language itself; and Uxntal, while it has the ability to import and combine files, doesn't have a standard library used by every project. Languages like Kitten or Factor, on the other hand, are very small languages with very large standard libraries. Some of the functions that would have been nice to see are `filter`, `append`, `zip-with` (or even `zip`), `member?` and `intersection`. The language's syntax and semantics are far enough from most LISPs that moving another language's library over would require a fair bit of work. (For example `(car (Cons x y))` produces `(Just x)`, rather than `x`.)

Some parts of the language don't seem to be working as intended. One particularly egregious example is functions like `>=`, which seem to confuse the compiled representation. Treating functions as data (e.g. passing them as arguments, returning them from functions) also seems a little buggy. The language lacks keywords like `eval` and `apply` that might otherwise make it flexible enough to address these shortcomings. There seems to be no way to write literal negative integers. The language is also pretty sluggish (although this is nothing special for custom interpreted languages like this one). On the plus side, there is a macro system (although I didn't use it in this solution). Its pattern-matching on `Cons` and `Just` are also pretty cool.

I'm glad this language exists -- in fact, I think that every programmer that takes themselves seriously should attempt to create something like this. It was frustrating to work with, though.

**Orion**: proof that you, too, can write your own language.

**Syntax Highlight**: `λ` (or, equivalently, `\`; creates an anonymous function, like the `lambda` keyword in other LISPs)

Day 14: [Pascal](https://freepascal.org)
----------------------------------------
(Specifically, this is the version of *Object Pascal* supported by the Free Pascal compiler.) Pascal certainly feels like an older language. It has pointer types, the ability to allocate or free memory, and a directive to modify the size of an array. Variables used by a procedure or function must be declared before its body. Integer widths can be specified (and indeed gave me a little trouble in Part 2 of this problem). Hash maps are fiddly to set up, unless the types of your keys and values line up with a pre-existing one.

On the other hand, it is definitely not a low-level language, *per se*. Memory management doesn't seem to be as intense as in C, for example; and there are abstractions like iterators for `for` loops. Object Pascal also has classes and methods. I can see why this language was popular for teaching programming in the past.

Since the language is older than C, it doesn't use the conventions that became the *de facto* standard post-C. Pointer dereferencing is written `p^`, not `*p`. Blocks are written `begin end`, not `{ }`. Assignment, equality test and inequality test are `:=`, `=` and `<>` rather than `=`, `==` and `!=` respectively. Pascal was evidently popular enough, though, that amongst languages further away from the C family, many of these syntax conventions have survived to the present day.

My Pascal solution to this problem runs quite a bit faster than my Python one. This is probably because it uses an array to lookup obstacles, and doesn't constantly allocate memory to convert the list of rocks to a set for hashing. (Rather, this version simply moves stuff in place and uses a hash function related to the "stress" used to solve the problem.)

**Pascal**: a high-level language from an age of low-level computing.

**Syntax Highlight**: `^` (declares or dereferences pointers)

Day 15: [x86-64 Assembly](https://www.nasm.us)
----------------------------------------------
This was a tough one. As I mentioned in the Day 10 writeup, one of the conveniences of old, emulated assembly languages is built-in pseudoinstructions that print numbers or characters. In x86, I had no such luxury. I initially tried to use libc's `printf` function. When I couldn't figure out how to get the linker to cooperate, I wrote my own function to print a decimal number instead.

This task, in a nutshell, is "Given *this* specific hash function, implement a hash table with string keys and single-digit positive integer values. It should support insertion, deletion and traversal of its values. Resolve hash collisions by storing each new value at the end of a linked list at each table location." Just about every high-level language seems to have a built-in hash table data structure, so I decided to implement the task in a language that didn't have one. I've implemented hash tables once or twice in C before, and linked lists many more times, but this is my first time doing either one in an assembly language. I also wrote my own memory allocator (since I couldn't use the `malloc` included with libc). I use the `mmap` system call to allocate a pool of 1024 cells to use as hash table entries (each 16 bytes wide). Each cell contains a flag marking whether it is in use or not, and the allocator simply carries out a linear search through the pool (beginning after the last-allocated cell) until it finds an unused cell. "Freeing" the cells is as easy as zeroing the "in-use" flag. The fact that all chunks of memory are the same size makes this process far less complicated than a proper `malloc` implementation.

The x86-64 instruction set has myriad different instructions, and I didn't end up using all that many of them. I'll bet there's a cleverer way to carry out a lot of this program's logic.

**x86-64**: batteries not included.

**Syntax Highlight**: `mov` (move a value between two registers, or from a register to memory, or from memory to a register; and the memory location can be offset by another register)

Day 16: [UF](http://www.call-with-current-continuation.org/uf/uf.html)
----------------------------------------------------------------------
This marks the second time I've written a program to run on the [Varvara](https://wiki.xxiivv.com/site/varvara.html) computer system. (I was worried that later tasks might have trouble fitting in memeory, or would require numbers larger than a 16-bit unsigned integer.) This program is probably the most complicated program I've written in a Forth-like language, but perhaps surprisingly, it feels pretty understandable. I've seen Forth described as "the ultimate assembly language", and this program certainly makes it feel that way. In Forth, defining large amounts of small functions is definitely the way to go. It makes the program very readable.

One of the problems I came up against while writing this solution was the native 256-byte size of the Uxn data stack. My implementation requires at most 60 pairs of 2-byte cells to sit on a stack (for a total of 240 bytes); and when that was added to the bottom values used by the Forth runtime and the top values used by my functions, it overflowed the native data stack. Hence, my solution allocates memory for an extra stack to store these cells. (It's likely 256 bytes would be enough for this, but it allocates 512 bytes).

UF requires manual memory management. Of course, most short-term variables should be kept on the data stack (or stashed on the return stack); but in a situation like this one, where a large grid must be read from a file, allocating memory makes a lot more sense. The process of allocating memory is certainly very different from C-like language. The memory allocated in this program takes up more than 32 kiB of the 64 kiB the Varvara has available.

**UF**: a complete Forth for Varvara.

**Syntax Highlight**: `allot` (allocates an amount of memory for a given variable)

Day 17: [Standard ML](https://www.smlnj.org)
--------------------------------------------
(There are a few different implementations of Standard ML; I used the New Jersey one.) Standard ML, like OCaml and Haskell, is a very functional language. It's my understanding that it's possible to make mutable data structures, in Standard ML, but my solution ended up using only immutable ones (except for TextIO.inStream). The language is very similar to OCaml, of course, and it's a lot less pedantic than Haskell. The type-checking was very helpful for debugging.

I used the New Jersey implementation because it is supposed to have quite a large standard library, but I didn't end up using much of it! I had initially planned to represent the grid as a hash map (of coordinates to values), but I instead used a list of lists; and I implemented my own priority queue when I couldn't figure out how to set up the library one. This was my first time setting up a priority queue that wasn't a binary heap -- it seemed like an easier option in this context.

My first attempt to solve this problem used naive breadth-first and depth-first searches, but after running them for a few hours, I decided to figure out the priority queue option instead. I like to think I learned something in setting up the leftist heap.

**Standard ML**: an elegant functional language (that isn't too fussy).

**Syntax Highlight**: `o` (composes two functions together)

Day 18: [Sacalon](https://sacalon.github.io)
--------------------------------------------
There's nothing offensive about this language's syntax, but it was a little frustrating to use. This is not the first time I've attempted to use the language, but my prior attempt was foiled because I couldn't get pointers to structures to cooperate. I've avoided those in this solution.

I have many gripes with the process of using Sacalon. Let's start with the installation process. The installation instructions refer to a script called `sacalon.sh` that carries out the build/install process, but the actual script is named `install.sh`. It checks and installs dependencies (reporting they "where" installed), runs `make`, then attempts to copy the resulting binary directly into the `/usr/bin` directory. When I denied this request, the script carried on oblivious, reporting "Added Sacalon to path". (Bold of you to assume my `$PATH` contains `/usr/bin`!)

Next, let's look at the libraries and examples. Until late in 2023, the language was known as "Hascal". The name change was a good idea, since the top web search result was usually, "did you mean _Haskell_ programming language?" Unfortunately, many of the examples contained within the distribution still refer to Hascal. The amount of Sacalon in the language's standard library is minimal. Much of it consists of calling functions from C++'s standard library. There's nothing wrong with this, really, except that a lot of the functions I tried to use didn't work. The `abs` function from the `math` module didn't want to convert my `int64`. (Is it because the type signature is float -> float?) The `conv` module didn't want to convert numeric strings to integers. (Is there a difference between Sacalon-style strings and C-style strings?) I wrote my own version of these instead.

Another issue: the documentation was lacking and inconsistent. The documentation provided for the standard library was a link to a 404 error. The `char` datatype was almost completely undocumented. The `append` global function was incorrectly documented to behave like a method. There seems to be no way to initialise an empty list of strings. Many compilation errors were reported as "unknown compiler error", making them very difficult to track down (hence this extra-long post documenting the kinds of thing that cause them!).

Finally, the compiler apparently ignores any output filename directives. No matter what I tried, Sacalon always compiled my program file, `day18.sa`, to a binary called `day1` in the same directory as the source. Neither command line arguments nor invoking the compiler from elsewhere seemed to affect this.

I could go on. Despite everything, though, the language itself is relatively inoffensive. I'm sure that given the chance, it'll eventually improve to the point of useability. It would be nice to have a way to declare empty lists, though.

I solved this problem by carrying out an imperative fold-left-reduction. The language doesn't yet seem mature enough to deal with a more functional approach. I'm glad I didn't have to deal with pointers.

**Sacalon**: it's not there yet.

**Syntax Highlight**: `cuse` (executes inline C++)

Day 19: [Gambit Scheme](https://gambitscheme.org)
-------------------------------------------------
I feel I've become very familiar with Scheme by this point. One of the features of the Gambit Scheme compiler is that it compiles to a binary via a C representation, so programs are pretty portable. (As if that wasn't enough, it can also be transpiled to JavaScript!) Other than that, it's everything you'd expect from a mature, polished implementation of Scheme.

When solving this problem, the foresight of knowing the second part of the problem helped me structure my program to solve it; and this incidently meant my program solved the second part first, since it doesn't involve parsing quite as much input.

**Gambit Scheme**: it's just LISP (but more portable).

**Syntax Highlight**: `string->symbol` (converts a string to a symbol -- this was the first time I found a use for it)

Day 20: [Erlang](https://www.erlang.org)
----------------------------------------
When I learned that Erlang was a very object-oriented language, I expected its syntax to resemble that of C++, or perhaps Smalltalk. Imagine my surprise when it turned out to be much closer to Prolog! (Apparently the language was first implmented in Prolog, which doesn't surprise me given its syntax.)

My method of solving this problem was to instantiate a separate object for each of the ~60 puzzle elements and simulate the scenario with message-passing. While Erlang seems to be a perfect match for this kind of solution, it comes with a few quirks; for one, the main function needs to wait for the messages to propagate throughout the system before it queries the cycle-detecting components of the system. (This means my solution is a little slower than it needs to be, but removing the delay leads to inconsistent results.)

**Erlang**: spin all the plates at once!

**Syntax Highlight**: `receive` (delay execution until a message -- or a particular shape of message -- is received)

Day 21: [Dart](https://dart.dev)
--------------------------------
Compared to some of the other languages I've used, Dart felt very familiar. Its syntax feels very Java or C++-like, and its anonymous functions can be sugared to look like JavaScript's. Its optional types are nice too. When I started writing this solution, I thought I'd have to import `dart:collection` to use some of the data structures I wanted; but as I later discovered, `Map`s are part of Dart's core, and `dart:collection` is only needed if you want a specific implementation of them. The language seems to be very robust and predictable. Apparently it can also be transpiled to JavaScript.

My approach to solving this problem was to try to determine a pattern and extrapolate it into the future. My attempt to do this in Python failed, and I had to use a different (awful) approach. I suspect I had chosen the wrong values to try and extrapolate from, or conflated odd and even parities, or both. In my Dart solution, I've been able to fix the problem. (Note that once again, Eric has controlled properties of the input to ensure the output can be extrapolated this way.)

**Dart**: Google's Java.

**Syntax Highlight**: `as` (unwraps a nullable value)

Day 22: [Wisp](https://www.draketo.de/software/wisp.html)
---------------------------------------------------------
What a weird way to write Lisp. Wisp feels like the mirror-image of Hy; Hy looks like Lisp but feels like Python, and Wisp looks (sort of) like an ALGOL-syntax language, but feels like Scheme. The fact that it is actually just a way of writing Guile Scheme made it difficult to debug, since the line numbers quoted for errors corresponded to the invisible Scheme representation instead of the actual Wisp source code.

The puzzle gave me a little more trouble than I would have liked. Scheme makes it very easy to use lists for everything, but that approach results in a hopelessly slow program. (I think this is the only program I've given an optimising pass to *prior* to the README write-up.) I ended up swapping out a lot of lists for hash-maps instead, and now it runs in an acceptably quick amount of time. I overestimated how useful memoising the `count-falls` function would be, but in the program's current state it's still usefil (since that function is called by both the part-1 and part-2 sections. I'll likely do a little bit more cleanup -- the syntax is a little frustrating to work with at times (e.g. when a line starts with `:`), but can be messed around with a lot.

**Wisp**: I can't believe it's not Scheme!

**Syntax Highlight**: `.` (quotes an atom instead of calling it as a function)

Day 23: [Lisp Flavoured Erlang](https://lfe.io)
-----------------------------------------------
I really enjoyed using the concurrent capabilities of Erlang on Day 20, and finding the longest path in a graph seemed like an embarrassingly parallelisable task; hence, LFE was my language of choice for this task. Not everything went as planned.

Erlang is great at handling concurrent tasks, but I don't think the syntax of Lisp is very well-suited to carry out sequential commands like send and receive. There's a good chance LFE (or Erlang!) has a nicer way to structure this sort of pattern, but if there is, I'm not familiar with it.

I've written many solutions now in Scheme-like (Lisp-1) languages, but LFE is a Lisp-2 language and has different syntax for referring to functions. The only other Lisp-2 I'm very familiar with is Common Lisp, but LFE's syntax is different enough that I still found it tough to deal with.

Finally, my original super-concurrent approach didn't work! The number of concurrent tasks blew past the default limit of 32768 and made the BEAM vm sad. My current version uses only two processes, one for each part. There's a good chance I could speed things up further by setting up a queue of paths to explore, and spawning 4-8 processes to work through it concurrently, but I'm happy with this solution for now.

Writing Hy in the 2021 challenge felt like writing Python, even though the syntax was lisp-like. LFE, on the other hand, feels a lot more lisp-y to me.

**Lisp Flavoured Erlang**: Erlang for people who don't like Prolog.

**Syntax Highlight**: `!` (send a message to another process -- the same as in Erlang)

Day 24: [Octave](https://octave.org)
------------------------------------
Octave is GNU's answer to the better-known Matlab. It's a free and open-source version of the language that is, in theory, completely compatible with Matlab files. (Imagine having a proprietary programming language!)

This puzzle was very tough. My Python solution was basically to throw the whole thing into the z3 equation solver and let it do the work. When coming back to this puzzle, I decided to try and solve it myself. I found myself very stuck until an internet search reminded me that _vector cross products distribute across vector addition_. This was the hint I needed to solve the puzzle:

Let vectors $P$ and $V$ be the rock's initial position and velocity respectively. If the $k$th hailstone has position and velocity  $p_{k}$ and $v_{k}$, and collides with the rock at time $t_{k}$, then:

$$p_{k} + v_{k}t_{k} = P + Vt_{k}$$

$$(p_{k} - P) + (v_{k} - V)t_{k} = 0$$

This means that $(p_{k} - P)$ is a multiple of $(v_{k} - V)$ -- or in other words, the vectors are parallel to each other. (This makes sense, since from the rock's frame of reference, every hailstone's displacement vector must be parallel to that hailstone's velocity vector if it will collide with the rock.) The trick is then to find the cross product of the parallel vectors, and then _expand_ the cross product:

$$0 = (p_{k} - P) \times (v_{k} - V)$$

$$0 = (p_{k} \times v_{k}) - (p_{k} \times V) - (P \times v_{k}) + (P \times V)$$

That $(P \times V)$ term shows up in _every_ hailstone's cross-product-expansion, so it can be cancelled out if we take a pair of hailstone's positions and velocities.

$$(p_{k} \times v_{k}) - (p_{k} \times V) - (P \times v_{k})
    = (p_{k'} \times v_{k'}) - (p_{k'} \times V) - (P \times v_{k'})$$

$$(p_{k} \times v_{k}) - (p_{k'} \times v_{k'})
    = P \times (v_{k} - v_{k'}) + (p_{k} - p_{k'}) \times V$$

Equating $x$, $y$ and $z$ components of the resulting equation gives us a system of three linear equations, and each different pair of hailstones gives us three more. Since we only need to solve for six unknowns, two pairs of hailstones are sufficient. (I used 1-2 and 1-3.)

Once I'd finally figured out the maths, I figured I could use Octave to solve the system of equations (rather than writing a Gaussian elimination algorithm from scratch). Unfortunately, Octave's equation solver only operates on matrices of floating-point values, and the integers in this problem were large enough that not even double-precision could keep up. Rather than figure out how to make Octave deal with arbitrary precision, I brute-forced a small number of integer values near the approximate solution given by Octave.

The language itself is a little annoying, but at least it's understandable. Staying true to its vision of being a free (and backwards-compatible) version of Matlab, it has ported over all of Matlab's quirks: 1-based indexing, printing the value of every statement not terminated by a semicolon, requiring scripts to start with a statement, indexing via `()`, etc.

Matlab/Octave are great at dealing with matrices -- if you can twist your data into the shape of a matrix, they seem to be a good choice. In most other situations, though, I wouldn't recommend them.

**Octave**: proof that trying to lock a programming language behind a paywall is doomed to fail.

**Syntax Highlight**: `\` ("divides" a matrix by a column vector, so that `A\B` solves `Ax = B`)
