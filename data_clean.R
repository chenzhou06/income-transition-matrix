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
# View(rural02_id[, c("COUN", "HOUS", "P1_2", "hous_pad0", "member_pad0", "ind_id")])

# calculate total annual income
rural88_inc <- rural88_id %>%
    # remove NAs
    filter(!is.na(AMI88) & !is.na(TNRI88) & !is.na(OCI88)) %>%
    # regular average monthly wage * 12 + non-regular income +
    # other cash income
    mutate(inc = AMI88 * 12 + TNRI88 + OCI88)
rural88_inc %>%
    select(ind_id, AMI88, TNRI88, OCI88, inc) %>%
    View()
rural95_inc <- rural95_id %>%
    # remove NAs by B202 (AMI as above), B203 (TNRI), B204 (other cash)
    filter(!is.na(B202) & !is.na(B203) & !is.na(B204)) %>%
    mutate(inc = B202 * 12 + B203 + B204)
rural95_inc %>%
    select(ind_id, B202, B203, B204, inc) %>%
    View()
rural02_inc <- rural02_id %>%
    # remove NAs
    # P1_43: total wage income
    # P1_57: total non-wage income
    filter(!is.na(P1_43) & !is.na(P1_57)) %>%
    mutate(inc = P1_43 + P1_57)
rural02_inc %>%
    select(ind_id, P1_43, P1_57, inc) %>%
    View()

# construct panel
rural9502 <- rural95_inc %>% # panel 95 to 02 only rural
    inner_join(rural02_inc, by="ind_id", suffix = c("95", "02")) %>%
    select(ind_id, starts_with("inc")) %>%
    filter(inc95 != 0 & inc02 != 0) # drop zeros, improper for real

trans_m <- function(st, st2, tiles = 4) {
    state1 <- ntile(st, tiles)
    state2 <- ntile(st2, tiles)
    count_table <- table(state1, state2)

    count_table / rowSums(count_table)
}

with(rural9502, trans_m(inc95, inc02))