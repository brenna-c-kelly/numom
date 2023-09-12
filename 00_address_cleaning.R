
library(dplyr)
library(haven)

# files  <- list.files(paste0(getwd(), "/address data"), pattern = '\\.sas7bdat')

v1 <- read_sas("/Users/brenna/Downloads/2_SASV9-selected/v1a_addr_site8.sas7bdat")
v2 <- read_sas("/Users/brenna/Downloads/2_SASV9-selected/v2a_addr_site8.sas7bdat")
v3 <- read_sas("/Users/brenna/Downloads/2_SASV9-selected/v3a_addr_site8.sas7bdat")
v4 <- read_sas("/Users/brenna/Downloads/2_SASV9-selected/v4a_addr_site8.sas7bdat")

names(v1) <- tolower(names(v1))
names(v2) <- tolower(names(v2))
names(v3) <- tolower(names(v3))
names(v4) <- tolower(names(v4))

# visit 2
v1$full_address <- paste0(paste(v1$v1aj01a, v1$v1aj01b), " ",
                          ifelse(is.na(v1$v1aj01c), " ", paste0(v1$v1aj01c, ", ")), 
                          v1$v1aj01d, ", ",
                          ifelse(v1$v1aj01e == 45, "UT", "Other"), ", ",
                          v1$v1aj01f)
v1$full_address <- ifelse(grepl("NA", v1$full_address), NA, v1$full_address)

v1$visit <- "v1"
v1 <- v1[, c("studyid", "publicid",
             "full_address", "visit")]

# visit 2
v2 <- v2 |>
  filter(v2ak01 == 1) # only if address changed

v2$full_address <- paste0(paste(v2$v2ak02a, v2$v2ak02b), " ",
                          ifelse(is.na(v2$v2ak02c), " ", paste0(v2$v2ak02c, ", ")), 
                          v2$v2ak02d, ", ",
                          ifelse(v2$v2ak02e == 45, "UT", "Other"), ", ",
                          v2$v2ak02f)
v2$visit <- "v2"
v2 <- v2[, c("studyid", "publicid",
             "full_address", "visit")]

# visit 3
v3 <- v3 |>
  filter(v3ai01 == 1) # only if address changed

v3$full_address <- paste0(paste(v3$v3ai02a, v3$v3ai02b), " ",
                          ifelse(is.na(v3$v3ai02c), " ", paste0(v3$v3ai02c, ", ")), 
                          v3$v3ai02d, ", ",
                          ifelse(v3$v3ai02e == 45, "UT", "Other"), ", ",
                          v3$v3ai02f)
v3$visit <- "v3"
v3 <- v3[, c("studyid", "publicid",
             "full_address", "visit")]

# visit 4
v4 <- v4 |>
  filter(v4ag01 == 1) # only if address changed

v4$full_address <- paste0(paste(v4$v4ag02a, v4$v4ag02b), " ",
                          ifelse(is.na(v4$v4ag02c), " ", paste0(v4$v4ag02c, ", ")), 
                          v4$v4ag02d, ", ",
                          ifelse(v4$v4ag02e == 45, "UT", "Other"), ", ",
                          v4$v4ag02f)
v4$visit <- "v4"
v4 <- v4[, c("studyid", "publicid",
             "full_address", "visit")]

# combining all addresses
addresses <- rbind(v1, v2, v3, v4) %>%
  filter(!grepl("NA", full_address)) %>%
  filter(!is.na(full_address))

write.csv(addresses, "addresses.csv")
