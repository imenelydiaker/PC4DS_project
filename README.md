# PC4DS Project

## Project Intro/Objective
This project is a R package that contains the solution of two problems Seed Dispersal and Insurance Risk:

* Part 1 : solve case studies chapter 21 section 21.4

Part 1 is about simulating the process by which a plant species colonises a new area.

* Part 2 : solve student projects chapter 22 section 22.4

Part 2 of this project does the simulation for the problem of insurance risk. 
It also contains a test file that you can run in the directory Test.


### Seed Dispersal - PC4DS Project Part 1

The problem is is already solved, the additional task that we gave ourselves, 
consists in reducing the execution time of the solution without modifying it.
We offer two solutions, one with R, avoiding redundancy in the code, 
the other with Cpp which not only deals with redundancy, but is intended to be faster than R solution.


### Insurance Risk - PC4DS Project Part 2

In this part, we simulated the size of a typical claim X that follows pareto distribution. Then we simulated the assets of the company over a period of time in years, Z, two verisons were coded, one that takes into consideration the profits taken by shareholders and another one that doesn't. We also calculated the probability of going bust for the comapny.

Run : *?simu_X* or *?z* or *?z_profit* to see functions documentation

## Getting Started
1. Download this repository and open the project : pkgPC4DS.Rproj
2. Install library "devtools" if not installed *install.packages("devtools")*
3. Make sure to have both packages *EnvStats* and *Rcpp* installed
4. Run : *source("main.R")*
5. Choose an option and follow the instructions

### Technologies
* R
* C++ (Rcpp)

### Authors 
Imene KERBOUA imene.kerboua@univ-lyon2.fr

Mouad LARIBIA mouad.laribia@univ-lyon2.fr

Lawan Dan-Azoumi lawan.dan-azoumi@univ-lyon2.fr
