# construct income variable from CHIP dataset.

# read data
library(readr, help, pos = 2, lib.loc = ".packages")
rural88 <- read_tsv("./data/88_rural_individual.tsv.gz")

summary(rural88)