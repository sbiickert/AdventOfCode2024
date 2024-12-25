# AdventOfCode2024

Code solutions for the 2024 Advent of Code

## Let's Gooooo

I've got multiple contenders for the language of the year for 2024. In order of preference:
1. Raku
2. PHP
3. Perl
4. Swift
5. Python

I was toying with the idea of learning F# in the last couple of weeks leading up to the event but yesterday when I tried doing day 1 in F#, I immediately realized I was out of my depth. I will probably come back to it, but for now I am going to be content with exercising a functional pattern in the other languages when possible. Day 1 and Day 2 in Raku both have a strong functional design that is super-clean.

Why these languages? Mostly because I've got some history with them. At this point I've done AOC puzzles with all of them and I have a standard toolkit that I've created and tested. When things get tough later in the month, it's possible that I will switch languages. Swift has the best debugger and I know how to do parallel processing if I need to burn electrons. Raku is at the top of the list mostly because it represents a challenge, both because I'm least familiar with it, and because I've got the nano editor set up for it. I can do this challenge on the Raspberry Pi if I feel like it.

Day 1 I did in four of the languages (not Python) just to get everything warmed up. Raku took the longest, but in the end I liked the solution the best. We'll see how the rest of the month progresses.

## Day 6 - Surprisingly Okay

The Raku experiment is ongoing. There have been issues with the language being obtuse about types in a way that Perl never does, and barfing when I tried doing parallel computing for the first time. But I would say that it's been successful. My favourite part is the feed ==> operator. I am trying to use functional programming patterns when I can, and using the feed operator to chain multiple actions together without intermediate variables really makes for clean code.

Today's part 2 took 180 seconds to run on my MacBook, and I wanted to light up all the cores to trim that, but my experiments with Promises kept giving me a MoarVM panic error. So I think I will re-implement in Swift and use GCD instead. I've already got my stars.

## Day 13 - Starting to Struggle

As is usual, the difficulty in the puzzles is ramping up. Days 11, 12 and 13 have all forced me to work, and Day 11 I had to go to Reddit to look for help. Day 13 part 2 in particular made me sweat because the example didn't actually give any testable numbers. I entered the number and hit submit, but felt that if it didn't work, I didn't know how I would debug it.

My comfort with Raku is growing, but I still think that the collections are the biggest pain in the butt. What good is having Sets, HashSets, Bags, etc. if you spend more time than it's worth fighting compiler errors? I'm sticking to Arrays and Hashes. Early on, I focused on the FP, but the last few days it's been more procedural and OO. Today in particular I leaned on OO to keep my sanity.

Over halfway!

## Day 23 - Nearly There

Today is practically a day off. We had a lot of stuff to get done for Christmas, so I made a point of trying to solve today before bed last night. And considering it's Day 23, the fact that I solved both parts in about an hour was a thrill. I was practically punching the air and had trouble falling asleep.

Day 21 was one that broke me (and a lot of others, it seems). I worked from after the ride through until after supper. I don't think it had anything to do with my choice of language or editor, it was just trying to get the logic optimized. I had it "working" almost immediately, but not optimal pathfinding, which meant my answers were wrong.

As I'm getting deeper and deeper into AoC, I'm leveraging more and more of Raku. Last night I looked up combinations() for the first time and used it extensively for Day 23. I also used the Set (elems) operator. I wouldn't say I'm a master of the language by a long shot, but I'm proficient now. Also proficient with nano, although I still have to think when I want to copy something.

## Day 25 - Done

And Advent of Code is over for another year. It was a good year, I think. Only one day was spent banging my head against the wall. No indication on the website that this is the end (10 years might have been an opportunity for Eric to go out on top).

### Raku Exit Interview

When I was putting together my AoC libraries in Raku earlier this year, I really didn't think that I was going to use the language. There were too many challenges, it seemed. But I took the plunge anyways. 

The main things that were hindrances:
- No debugger/profiler. I can get by without an interactive debugger (I've done AoC in Perl before) but not having a profiler was annoying. Sometimes you want to know definitively where the time is being lost.
- I couldn't get multithreaded processing working. There were a handful of challenges this year that could have been accelerated with multiple threads working in parallel. I did additional solves in Swift for days 6 and 7 for that reason.
- Documentation and community are thin. I think it's fair to say that there isn't a huge community working in Raku. The core docs are good, but there aren't a lot of blogs covering language aspects with examples. Where the documentation doesn't cover it, the second tier is a little empty. [Raku Guide](https://raku.guide)
- Collections seem to get in the way. Raku adds Lists, Bags, Sets, Sequences and others, but for the few times that they helped, they got in the way more. Expecting an Array when you get a List or a Sequence is frustrating. Perl's core set of Scalar, Array and Hash really are good enough.

The positives:
- The core language and standard library of functions are vast. Other than my AoC libraries, I never had to look for an add-in library. GCD, LCM, combinations, print-debug, cloning and others are all in the language. Heck, the Rat type was worth it right there. No issues with floating-point crap at all this month.
- The sigils ($, @, %) were odd at first after using Perl and PHP, but I liked the consistency.
- Having Array.end (the last valid index) and Array.elems (the count of items) was nice.
- I had the option to use Procedural, Functional and Object-Oriented programming throughout the month. I really appreciated that. FP made for very concise code, but I find that OOP is still the way I think.

I don't know that I will use Raku much for other things, but I'm glad to have tried it. It's not like when I finished 2019 in Java and I never wanted to see the language or NetBeans again. My unofficial ranking:
1. Swift. Now that Regex is core language syntax, most of it is so easy it feels like cheating. The only hiccup is String indexing is always complex.
2. Perl. With the new OOP, I wouldn't hesitate to tackle AoC. I've got a list of CPAN modules to use, and it's got a profiler.
3. Raku. If I understood the Collections better, if it had a profiler, and if editor support were broader... it might be #1.
4. Objective-C. Surprisingly powerful, when you've got NSArray, NSSet, NSDictionary. A lot of typing to do though, with the verbosity of the language.
5. PHP. I think I could do well with it. There's good OOP and some FP support. The main issue is that the standard library is a hot mess.
6. Python. Really, this is cheating.
7. Ruby. Like the love child of Pascal and Python. I would like some type support in the language. If only to catch the odd oopsie and save debugging time.
8. Java. Verbose and creaky. I really came to hate NetBeans. I would consider doing Java again but only in a different editor, like BBEdit.
9. C++. I had fun doing some re-solving of 2020 in CodeWarrior on my Powerbook G4. Modern C++ probably would be a lot of fun to try, but a daunting task.
10. Pascal. I really liked doing some re-solving with Pascal, but I don't think I would ever do an initial solve with it. The poor support for Regex and the lack of a definitive Hash/Dictionary are real show-stoppers.