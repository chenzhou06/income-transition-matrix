# construct income variable from CHIP dataset.

# load libraries
library(readr, help, pos = 2, lib.loc = ".packages")
library(dplyr)

# read data
rural88 <- read_tsv("./data/88_rural_individual.tsv.gz")

# construct individual identifier
rural88_id <- rural88 %>% mutate(ind_id = paste0(UCODE, RELATION))

# missing value
rural88_na <- rural88_id %>%
    mutate(ami_na = ifelse(AMI88>9999, NA, AMI88)) %>%
    mutate(tnri88_na = ifelse(TNRI88==99999999, NA, TNRI88))

# calculate total annual income
rural88_inc <- rural88_na %>%   # regular average monthly wage * 12
    mutate(inc = ami_na * 12 + tnri88_na)  # + non-regular income

summary(rural88_inc$ami_na)
summary(rural88_inc$tnri88_na)
summary(rural88_inc$inc)
View(rural88_inc[, c("AMI88", "TNRI88", "ami_na", "tnri88_na", "inc")])
sum(!is.na(rural88_inc$inc))