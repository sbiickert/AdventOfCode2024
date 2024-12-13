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