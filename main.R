## File that runs test_part1 and test_part 2 files

rm(list = ls())
# install.packages("devtools")
library(devtools)
load_all()

library(Rcpp)

## MENU
cat("PC4DS Project\n!! Make sure to have the library \"EnvStats\" installed before running \"Test part 2\"\n\nChoose an option :")

switch(menu(c("Test part 1 Seed Dispersal - R version", "Test part 1 Seed Dispersal - C++ version", "Test part 2 Insurance Risk")) + 1,
       cat("Good bye\n"), source("test/test_part1.R"), sourceCpp("test/testSeedPart1_cpp.cpp"), source("test/test_part2.R"))
