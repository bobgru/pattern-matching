pattern-matching
================

This repository contains code for a project in *harmonic trading* in which certain 
patterns in price charts suggest good trading opportunities.

### The Origin
A friend of a neighbor approached me in the Spring of 2011 with the idea of automating 
currency trades based on "harmonic patterns". He was using a popular trading platform 
as provided by his brokers, which offered graphic pattern matching but required that he
place trades manually. He wanted to automate the whole process.

### The Strategy
While I didn't intend to do any such trading myself, and felt genuine fear
concerning the liability involved with automating his trades,
I enjoy hearing about systems and was looking for an excuse to program
in Haskell. I decided to start by verifying (or debunking) the theory behind
the patterns. I was going to read some historical price data, match the patterns
as specified in the harmonic trading literature, apply his personal decision criteria,
and see whether he would have made money.

### What Happened
I got as far as matching patterns and drawing them on a price chart when the
poor fellow unexpectedly passed away. What I was preparing to tell him was that it's nonsense.
The "math" involves such generous and arbitrary fudge factors, and there are so many cosmically
significant "harmonic" ratios to consider, that you will see whatever you want to see.

### What's Here
The artifacts of this project are a few Haskell programs that analyze price data, none of which got
any review for accuracy beyond my unit tests. I first tried a monadic pipeline that ran so slowly
I abandoned it. Next I did what amounts to a filter that performed much better, starting with a
simple "trend analysis" I read about in a book, then with the harmonic patterns. The trend analysis
simulation also modeled a brokerage account with automated trades.

### What's Next
It was fun. I learned some Haskell basics. At ICFP 2013 I was encouraged to post the code
despite its irrelevance, incompleteness and appearance. I decided to use it to learn more about 
Haskell and participating in the Haskell community, so I'm hoping the fun will continue.
