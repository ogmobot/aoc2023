Advent of Code 2023
===================
Another year... *another* 25 languages. Let's see how we go this time.

Day 01: Bash (and UNIX text utilities)
--------------------------------------
I've been using shell scripts for years now. Knowing the basics of Bash has let me log debug output, hexdump malfunctioning assembly code, time my Advent of Code solutions, and so on. This is the first time I've actually combined shell utilities together to solve a puzzle. There's so much power in these tiny tools, and I've tapped only a fraction of it here. Perhaps I should try using them to solve problems more often.

My Python solution approaches this problem with regular expressions and a dictionary of english digit names, but I've used a less clever solution here (replacing word names with their matching digits, but not interfering with overlapping words). This seems like a good fit for stream- and text-editing programs. (I also could have piped the whole thing into `awk`, or `python -c`; but that would defeat the purpose, wouldn't it?) I also learned about the `mktemp` program. Originally, I was trying to figure out how to `tee` standard output into two different commands, but this solution seemed cleaner.

**Bash**: shell utilities are greater than the sum of their parts.

**Syntax Highlight**: `|` (pipes the output of one command to the input of another)
