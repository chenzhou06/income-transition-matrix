# construct income variable from CHIP dataset.

# load libraries
library(readr)
library(dplyr)
library(haven)

# read data
rural88 <- read_sav("./data/88_rural_individual.sav.gz")
rural95 <- read_sav("./data/95_rural_individual.sav.gz")
rural02 <- read_sav("./data/02_rural_individual_income.sav.gz")

# construct individual identifier
rural88_id <- rural88 %>%
    filter(!is.na(RELATION)) %>%
    mutate(relation_pad0 = ifelse(RELATION > 9,
                                  as.character(RELATION),
                                  paste0("0", RELATION))) %>%
    mutate(ind_id = paste0(UCODE, relation_pad0))
rural95_id <- rural95 %>%
    filter(!is.na(B103)) %>%
    mutate(member_pad0 = ifelse(B103 > 9,
                                as.character(B103),
                                paste0("0", B103))) %>%
    mutate(ind_id = paste0(A1, B101, member_pad0))
# View(rural95_id[, c("A1", "B101", "B103", "member_pad0", "ind_id")])
rural02_id <- rural02 %>%
    mutate(hous_pad0 = ifelse(HOUS > 9,
                              as.character(HOUS),
                              paste0("0", HOUS))) %>%
    mutate(member_pad0 = ifelse(P1_2 > 9,
                                as.character(P1_2),
                                paste0("0", P1_2))) %>%
    mutate(house = paste0(VILL, hous_pad0)) %>%
    mutate(ind_id = paste0(COUN, house, member_pad0))
View(rural02_id[, c("COUN", "HOUS", "P1_2", "hous_pad0", "member_pad0", "ind_id")])

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