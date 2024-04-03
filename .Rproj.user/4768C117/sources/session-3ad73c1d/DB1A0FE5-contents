library(readr)
library(dplyr)

nmhss_2010 <- read_delim("data-raw/N-MHSS-2010-DS0001-data-excel.tsv",
                         delim = "\t", escape_double = FALSE,
                         trim_ws = TRUE)
nmhss_2010 <- nmhss_2010 %>% mutate(YEAR = 2010) %>%
  rename_with(~toupper(.), .cols = everything()) %>%
  mutate(across(everything(), factor))

nmhss_2012 <- read.csv("data-raw/N-MHSS-2012-DS0001-data-excel.csv")
nmhss_2012 <- nmhss_2012 %>% mutate(YEAR = 2012) %>%
  rename_with(~toupper(.), .cols = everything())%>%
  mutate(across(everything(), factor))

nmhss_2014  <- read.csv("data-raw/N-MHSS-2014-DS0001-data-excel.csv")
nmhss_2014 <- nmhss_2014 %>% mutate(YEAR = 2014) %>%
  rename_with(~toupper(.), .cols = everything()) %>%
  mutate(across(everything(), factor))



nmhss_2015 <- read.csv("data-raw/N-MHSS-2015-DS0001-data-excel.csv")
nmhss_2015 <- nmhss_2015 %>% mutate(YEAR = 2015) %>%
  rename_with(~toupper(.), .cols = everything()) %>%
  mutate(across(everything(), factor))



nmhss_2016 <- read.csv("data-raw/nmhss_puf_2016.csv")
nmhss_2016 <- nmhss_2016 %>% mutate(YEAR = 2016) %>%
  rename_with(~toupper(.), .cols = everything())%>%
  mutate(across(everything(), factor))

nmhss_2017 <- read.csv("data-raw/NMHSS_2017_PUF_CSV.csv")
nmhss_2017 <- nmhss_2017 %>% mutate(YEAR = 2017) %>%
  rename_with(~toupper(.), .cols = everything()) %>%
  mutate(across(everything(), factor))


nmhss_2018 <- read.csv("data-raw/nmhss-puf-2018-csv.csv")
nmhss_2018 <- nmhss_2018 %>% mutate(YEAR = 2018) %>%
  rename_with(~toupper(.), .cols = everything())%>%
  mutate(across(everything(), factor))


nmhss_2019 <- read.csv("data-raw/nmhss-puf-2019-csv.csv")
nmhss_2019 <- nmhss_2019 %>% mutate(YEAR = 2019) %>%
  rename_with(~toupper(.), .cols = everything())%>%
  mutate(across(everything(), factor))


nmhss_2020 <- read.csv("data-raw/nmhss-puf-2020-csv.csv")
nmhss_2020 <- nmhss_2020 %>% mutate(YEAR = 2020) %>%
  rename_with(~toupper(.), .cols = everything())%>%
  mutate(across(everything(), factor))


nsumh_2021 <- read.csv("data-raw/NSUMHSS_2021_PUF_CSV.csv")
nsumh_2021 <- nsumh_2021%>% mutate(YEAR = 2021)%>%
  rename_with(~toupper(.), .cols = everything())%>%
  mutate(across(everything(), factor))%>%
  rename(LST = LOCATIONSTATE)

nsumh_2022 <- read.csv("data-raw/NSUMHSS_2022_PUF_CSV.csv")
nsumh_2022 <- nsumh_2022%>% mutate(YEAR = 2022)%>%
  rename_with(~toupper(.), .cols = everything())%>%
  mutate(across(everything(), factor))%>%
  rename(LST = LOCATIONSTATE)





data_sets_2014 <- list(nmhss_2014,
                       nmhss_2015,
                       nmhss_2016,
                       nmhss_2017,
                       nmhss_2018,
                       nmhss_2019,
                       nmhss_2020)

list_col_names <- lapply(data_sets_2014, colnames)
list_col_names <- lapply(list_col_names,toupper)
list_col_names_common <- Reduce(intersect, list_col_names)

nmhss_2014_filtered <- nmhss_2014[,list_col_names_common]

nmhss_2015_filtered <- nmhss_2015[,list_col_names_common]

nmhss_2016_filtered <- nmhss_2016[,list_col_names_common]

nmhss_2017_filtered <- nmhss_2017[,list_col_names_common]

nmhss_2018_filtered <- nmhss_2018[,list_col_names_common]

nmhss_2019_filtered <- nmhss_2019[,list_col_names_common]

nmhss_2020_filtered <- nmhss_2020[,list_col_names_common]

data_start_2014 <-rbind(
  nmhss_2014_filtered,
  nmhss_2015_filtered,
  nmhss_2016_filtered,
  nmhss_2017_filtered,
  nmhss_2018_filtered,
  nmhss_2019_filtered,
  nmhss_2020_filtered)


remove_cols <- c("PUBLICAGENCY","LANG16","LANG1","LANG2","LANG3","LANG4","LANG5","LANG6","LANG8","LANG9","LANG10","LANG11","LANG22","LANG19","LANG23","LANG12","LANG13","LANG14","LANG15","LANG10","LANG18","LANG17","LANG_B","LANG21","LANG7","LANG20","REVCHK10","FUNDSMHA","FUNDSTATEWELFARE","FUNDSTATEJUV","FUNDSTATEEDUC","FUNDOTHSTATE","FUNDLOCALGOV","FUNDCSBG","FUNDCMHG","REVCHK15","FUNDVA","REVCHK17")

data_start_2014 <- data_start_2014 %>% select(-all_of(remove_cols))



cols <- sapply(data_start_2014,is.factor) & !names(data_start_2014) %in% c("LST")
data_start_2014[cols] <- lapply(data_start_2014[cols],function(x) as.numeric(as.character(x)))
data_start_2014_filter <- data_start_2014 %>%
  filter(!if_any(.cols = where(is.numeric), ~ .x < 0))

data_start_2014_filter[cols] <- lapply(data_start_2014_filter[cols], factor)


data_start_2014_filter <- data_start_2014_filter %>%
  mutate(FACILITYTYPE = as.character(FACILITYTYPE), # Convert to character if it's not already
         FACILITYTYPE = recode(FACILITYTYPE,
                               "1" = 'Psychiatric hospital',
                               "2" = 'Separate unit of general hospital',
                               "3" = 'Residential children',
                               "4" = 'Residential adults',
                               "5" = 'Residential Other',
                               "6" = 'VAMC',
                               "7" = 'CMHC',
                               "8" = 'CCBHC',
                               "9" = 'Day treatment facility',
                               "10" = 'Outpatient facility',
                               "11" = 'Multi-setting facility',
                               "12" = 'Other'),
         FACILITYTYPE = factor(FACILITYTYPE))

usethis::use_data(data_start_2014,overwrite = TRUE)
usethis::use_data(data_start_2014_filter,overwrite = TRUE)




# full_joined_df <- full_join(nmhss_2010, nmhss_2012, by = "CASEID")
# full_joined_df <- full_join(full_joined_df, nmhss_2014, by = "CASEID")

# data_sets <- list(nmhss_2010,
#                   nmhss_2012,
#                   nmhss_2014,
#                   nmhss_2015,
#                   nmhss_2016,
#                   nmhss_2017,
#                   nmhss_2018,
#                   nmhss_2019,
#                   nmhss_2020,
#                   nsumh_2021,
#                   nsumh_2022)
#
# list_col_names <- lapply(data_sets, colnames)
# list_col_names <- lapply(list_col_names,toupper)
# list_col_names_common <- Reduce(intersect, list_col_names)
#
# nmhss_2010_filtered <- nmhss_2010[,list_col_names_common]
#
# nmhss_2012_filtered <- nmhss_2012[,list_col_names_common]
#
# nmhss_2014_filtered <- nmhss_2014[,list_col_names_common]
#
# nmhss_2015_filtered <- nmhss_2015[,list_col_names_common]
#
# nmhss_2016_filtered <- nmhss_2016[,list_col_names_common]
#
# nmhss_2017_filtered <- nmhss_2017[,list_col_names_common]
#
# nmhss_2018_filtered <- nmhss_2018[,list_col_names_common]
#
# nmhss_2019_filtered <- nmhss_2019[,list_col_names_common]
#
# nmhss_2020_filtered <- nmhss_2020[,list_col_names_common]
#
# nsumh_2021_filtered <- nsumh_2021[,list_col_names_common]
#
# nsumh_2022_filtered <- nsumh_2022[,list_col_names_common]
#
# data_all_years <- rbind(nmhss_2010_filtered,
#                         nmhss_2012_filtered,
#                         nmhss_2014_filtered,
#                         nmhss_2015_filtered,
#                         nmhss_2016_filtered,
#                         nmhss_2017_filtered,
#                         nmhss_2018_filtered,
#                         nmhss_2019_filtered,
#                         nmhss_2020_filtered,
#                         nsumh_2021_filtered,
#                         nsumh_2022_filtered)
#
# data_sets_2014_even <- list(#nmhss_2010,
#                             #nmhss_2012,
#                             nmhss_2014,
#                             #nmhss_2015,
#                             nmhss_2016,
#                             #nmhss_2017,
#                             nmhss_2018,
#                             #nmhss_2019,
#                             nmhss_2020)
# list_col_names <- lapply(data_sets_2014_even, colnames)
# list_col_names <- lapply(list_col_names,toupper)
# list_col_names_common <- Reduce(intersect, list_col_names)
#
# nmhss_2014_filtered <- nmhss_2014[,list_col_names_common]
#
# nmhss_2016_filtered <- nmhss_2016[,list_col_names_common]
#
# nmhss_2018_filtered <- nmhss_2018[,list_col_names_common]
#
# nmhss_2020_filtered <- nmhss_2020[,list_col_names_common]
#
# data_start_2014_even <-rbind(#nmhss_2012_filtered,
#                         nmhss_2014_filtered,
#                         #nmhss_2015_filtered,
#                         nmhss_2016_filtered,
#                       #  nmhss_2017_filtered,
#                         nmhss_2018_filtered,
#                        # nmhss_2019_filtered,
#                         nmhss_2020_filtered)



#
# data_sets_2014_through_2022_list <- list(nmhss_2014,
#                        nmhss_2015,
#                        nmhss_2016,
#                        nmhss_2017,
#                        nmhss_2018,
#                        nmhss_2019,
#                        nmhss_2020,nsumh_2021,nsumh_2022)
#
#
#
# list_col_names <- lapply(data_sets_2014_through_2022_list, colnames)
# list_col_names <- lapply(list_col_names,toupper)
# list_col_names_common <- Reduce(intersect, list_col_names)
#
# nmhss_2014_filtered <- nmhss_2014[,list_col_names_common]
#
# nmhss_2015_filtered <- nmhss_2015[,list_col_names_common]
#
# nmhss_2016_filtered <- nmhss_2016[,list_col_names_common]
#
# nmhss_2017_filtered <- nmhss_2017[,list_col_names_common]
#
# nmhss_2018_filtered <- nmhss_2018[,list_col_names_common]
#
# nmhss_2019_filtered <- nmhss_2019[,list_col_names_common]
#
# nmhss_2020_filtered <- nmhss_2020[,list_col_names_common]
#
# nsumh_2021_filtered <- nsumh_2021[,list_col_names_common]
#
# nsumh_2022_filtered <- nsumh_2022[,list_col_names_common]
#
# data_start_2014_through_2022 <-rbind(
#   nmhss_2014_filtered,
#   nmhss_2015_filtered,
#   nmhss_2016_filtered,
#   nmhss_2017_filtered,
#   nmhss_2018_filtered,
#   nmhss_2019_filtered,
#   nmhss_2020_filtered,
#   nsumh_2021_filtered,
#   nsumh_2022_filtered)




# usethis::use_data(nmhss_2010,overwrite = TRUE)
# usethis::use_data(nmhss_2012,overwrite = TRUE)
# usethis::use_data(nmhss_2014,overwrite = TRUE)
# usethis::use_data(nmhss_2015,overwrite = TRUE)
# usethis::use_data(nmhss_2016,overwrite = TRUE)
# usethis::use_data(nmhss_2017,overwrite = TRUE)
# usethis::use_data(nmhss_2018,overwrite = TRUE)
# usethis::use_data(nmhss_2019,overwrite = TRUE)
# usethis::use_data(nmhss_2020,overwrite = TRUE)
# usethis::use_data(nsumh_2021,overwrite = TRUE)
# usethis::use_data(nsumh_2022,overwrite = TRUE)


# usethis::use_data(data_all_years,overwrite = TRUE)
# usethis::use_data(data_start_2014_even,overwrite = TRUE)
# usethis::use_data(data_start_2014_through_2022,overwrite = TRUE)



