# libraries
library(stringr)
library(tidyverse)

print_02 <- read.csv("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/csv/amps-2002-newspaper-magazine-readership-v1.1.csv")
electr_02 <- read.csv("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/csv/amps-2002-electronic-media-v1.1.csv")
internet_02 <- read.csv("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/csv/amps-2002-internet-v1.1.csv")
demogrs_02 <- read.csv("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/csv/amps-2002-demographics-v1.1.csv")
personal_02 <- read.csv("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/csv/amps-2002-personal-v1.1.csv")
lsm_02 <- read.csv("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/csv/amps-2002-lsm-v1.1.csv")
lifestage_02 <- read.csv("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/csv/amps-2002-lifestage-v1.1.csv")
# not got attitudes in 2002
# 
save(print_02, electr_02, internet_02, demogrs_02, personal_02, lsm_02, lifestage_02, file = "input_02.RData")

load("input_02.RData")
# 
print_02_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/csv/metadata/variable_labels/amps-2002-newspaper-magazine-readership-v1.1_variable_labels.txt")
electr_02_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/csv/metadata/variable_labels/amps-2002-electronic-media-v1.1_variable_labels.txt")
internet_02_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/csv/metadata/variable_labels/amps-2002-internet-v1.1_variable_labels.txt")
demogrs_02_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/csv/metadata/variable_labels/amps-2002-demographics-v1.1_variable_labels.txt")
personal_02_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/csv/metadata/variable_labels/amps-2002-personal-v1.1_variable_labels.txt")
lsm_02_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/csv/metadata/variable_labels/amps-2002-lsm-v1.1_variable_labels.txt")
lifestage_02_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/csv/metadata/variable_labels/amps-2002-lifestage-v1.1_variable_labels.txt")
# not got attitudes in 2002
# 
save(print_02_labels, electr_02_labels, internet_02_labels, demogrs_02_labels, personal_02_labels, lsm_02_labels, lifestage_02_labels, file = "labels_02.RData")

load("labels_02.RData")

## 1st Print (newspapers and magazines) Media Set

names_print_02 <- str_subset(print_02_labels, 'Number of different issues usually read or page through') %>%
        str_replace('.+\\s-', '') %>%
        str_trim()
fix(names_print_02)

saveRDS(names_print_02, "names_print_02.rds") # compared and tried to ensure consistency with ''12

names_print_02 <- readRDS("names_print_02.rds")

names_dailies_02 <- names_print_02[1:16]
names_biweeklies_02 <- names_print_02[17]
names_weeklies_02 <- names_print_02[18:39]

# # NBNB: Not community papers in 2012...
names_community_cape_town_02 <- names_print_02[40:51]
names_community_restCape_02 <- names_print_02[52:61]
names_community_FreeState_02 <- names_print_02[62:66]
names_community_NWest_02 <- names_print_02[67:68]
names_community_Jhb_02 <- names_print_02[69]
names_community_ERand_02 <- names_print_02[70:71]
names_community_KZn_02 <- names_print_02[72:74]

names_mags_weekly_02 <- names_print_02[75:89]
names_fortnightly_mags_02 <- names_print_02[90:92]
names_monthly_news_02 <- names_print_02[93]
names_monthly_mags_12 <- names_print_02[94:152]

names_alt_monthly_02 <- names_print_02[153:155]
names_quarterly_mags_02 <- names_print_02[156:158]

# NOT in 2002. From 2012..??? Double check!!
# names_monthly_store_mags_02 <- names_print_02[148:149]
# names_alt_month_store_mags_02 <- names_print_02[162:163]
# names_quarterly_store_mags_02 <- names_print_02[169:172]

# create print dataset (NB since no community newspapers in 2012, have excluded them here also...Double check as I go and also with other periods)
issues_02 <- print_02[,str_detect(names(print_02), 'ca[4567]co\\d{2}')]
issues_02 <- issues_02[,c(1:39,75:158)] # get rid of community newspapers (not in 2012.... check this later again)
names(issues_02) <- names_print_02[c(1:39,75:158)]

saveRDS(issues_02, "issues_02.rds")





# 
thorough_02 <- print_02[,str_detect(names(print_02), 'ca((29)|(30)|(31)|(32)|)co\\d{2}')]
thorough_02 <- thorough_02[,c(1:39,75:158)] # get rid of community newspapers

names(thorough_02) <- names_print_02[c(1:39,75:158)]


# # need to reverse numbering to serve as weights (see value_lables text file):
thorough_02 <- 7 - thorough_02

saveRDS(thorough_02, "thorough_02.rds")
# create single print dataset:

print_engagement_02 <- issues_02 * thorough_02

# replace nas with zero's: # function to replace NA's with 0 in dataframe x. consider also dplyr::replace_na
# # # replace NAs with zeros
print_engagement_02[is.na(print_engagement_02)] <- 0

saveRDS(print_engagement_02, "print_engagement_02.rds")

names(print_engagement_02[39:42])

print_engagement_02 <- readRDS("print_engagement_02.rds")

newspapers_engagement_02 <- print_engagement_02[,c(1:42)]
magazines_engagement_02 <- print_engagement_02[,c(43:123)]

saveRDS(newspapers_engagement_02, "newspapers_engagement_02.rds")
saveRDS(magazines_engagement_02, "magazines_engagement_02.rds")

magazines_engagement_02 <- readRDS("magazines_engagement_02.rds")
newspapers_engagement_02 <- readRDS("newspapers_engagement_02.rds")

## 2nd Electronic Media Set
# RADIO

# creating a names vector
names_radio_02 <- electr_02_labels %>%
        str_subset('.+Listened.+4\\sweeks') %>%
        str_replace('.+Listened\\sto\\s','') %>%
        str_replace('\\sin\\sthe\\spast.+','') 

# get rid of first: "any Radio services via a satellite transmission"
names_radio_02 <- names_radio_02[-1]

# also get rid of the following summaries:
# Total Radio (Any Radio) [22]
# SABC African Language Services [23]
# Total Community [24]
# Other Radio [31]
# Limpopo Corridor [32]
names_radio_02 <- names_radio_02[-c(22,23,24,31,32)]

fix(names_radio_02)

saveRDS(names_radio_02, "names_radio_02.rds")
names_radio_02 <- readRDS('names_radio_02.rds')

# get data...
radio4weeks_02 <- electr_02[,str_detect(names(electr_02), 'ca54co\\d{2}_\\d')]
radio4weeks_02 <- radio4weeks_02[,-c(22,23,24,31,32)] # get rid of list described above

radio7days_02 <- electr_02[,str_detect(names(electr_02), 'ca50co\\d{2}_\\d')]
radio7days_02 <- radio7days_02[,-c(22,23,24,31,32)]  # get rid of list described above

radioYesterday_02 <- electr_02[,str_detect(names(electr_02), 'ca53co\\d{2}_\\d')]
radioYesterday_02 <- radioYesterday_02[,-c(22,23,24,31,32)]  # get rid of "unsure" and "none"


# adding up
radio4weeks_02[,ind_7] <- radio4weeks_02[,ind_7] + radio7days_02
radio4weeks_02[,ind_y] <- radio4weeks_02[,ind_y] + radioYesterday_02

# creating engagement set:
radio_engagement_02 <- radio4weeks_02 + radio7days_02 + radioYesterday_02
names(radio_engagement_02) <- names_radio_02

saveRDS(radio_engagement_02, "radio_engagement_02.rds")
radio_engagement_02 <- readRDS("radio_engagement_02.rds")


names_tv_12

## TV
names_tv_02 <- electr_02_labels %>%
        str_subset('watched.+4\\sweeks') %>%
        str_replace('.+watched\\s','') %>%
        str_replace('in\\sthe\\spast\\s4\\sweeks','') %>%
        str_trim()

# fix(names_tv_02)

# cut total, unsure and no tv
names_tv_02 <- names_tv_02[-c(10:12)]
saveRDS(names_tv_02, "names_tv_02.rds")
names_tv_02 <- readRDS("names_tv_02.rds")

# want to isolate only past 4 weeks and get rid of ("UNSURE", and "no TV")
tv4weeks_02 <- electr_02[,c('ca38co9_1',
                            'ca38co9_2',
                            'ca38co9_3',
                            'ca38co9_4',
                            'ca38co9_5',
                            'ca38co9_6',
                            'ca38co9_7',
                            'ca38co9_5',
                            'ca38co17_5'
)] 

# want to isolate only past 7 days...
tv7days_02 <- electr_02[,c('ca38co19_1',
                           'ca38co19_2',
                           'ca38co19_3',
                           'ca38co19_4',
                           'ca38co19_5',
                           'ca38co19_6',
                           'ca38co19_7',
                           'ca38co27_4',
                           'ca38co27_5'
)] 

# want to isolate only yesterday...(indexes w.r.t 4weeks that are missing here: 7, 10)
tvYesterday_02 <- electr_02[,c('ca38co29_1',
                               'ca38co29_2',
                               'ca38co29_3',
                               'ca38co29_4',
                               'ca38co29_5',
                               'ca38co29_6',
                               'ca38co29_7',
                               'ca38co37_4',
                               'ca38co37_5'
)]

# combining into a tv engagement dataset (using tv4weeks_02 as basis):

tv_engagement_02 <- tv4weeks_02 + tv7days_02+ tvYesterday_02
names(tv_engagement_02) <- names_tv_02

saveRDS(tv_engagement_02, "tv_engagement_02.rds")

tv_engagement_02 <- readRDS("tv_engagement_02.rds")

## 3rd Internet Media Set

## accessed: sum of 4weeks, 7days and yesterday
internet_level1 <- internet_02[,str_detect(names(internet_02), 'ca38co(40)|(45)|(52)')]

#change all 2 = "No" and NA's' to 0
internet_level1[internet_level1 == 2 | is.na(internet_level1)] <- 0

internet_level1 <- rowSums(internet_level1)

# what internet was accessed for...
##  (maybe could use similar to vehicles?? as well as add up and multiply with first eng):

internet_level2 <- internet_02[,str_detect(names(internet_02),
                                           'ca38co(41_3)|(41_4)|(41_6)|(41_7)|(41_8)|(41_9)|(42_0)|(42_1)|(42_2)|(42_3)|(42_4)|(42_5)|(42_6)')]


# want to add "infos together to give int_search

int_search <- transmute(internet_level2[,5:13], rowSums(internet_level2[,5:13]))
internet_level2 <- internet_level2[,1:4] %>%
        mutate(int_search = int_search[,1])
        
names(internet_level2) <- c('int_print',
                            'int_radio',
                            'int_news',
                            'int_social',
                            'int_search')

## create single dataframe for internet multiplying internet_level1 with sum of internet_level2:
internet_engagement_02 <- internet_level2  * internet_level1

saveRDS(internet_engagement_02, "internet_engagement_02.rds")

internet_engagement_02 <- readRDS("internet_engagement_02.rds")

## create single dataframe for media02, including total_engagement columns)

# Level 1: Type
media_type_02 <- data.frame(cbind(qn = print_02$qn,
                                  scale(rowSums(newspapers_engagement_02)),
                                  scale(rowSums(magazines_engagement_02)),
                                  scale(rowSums(radio_engagement_02)),
                                  scale(rowSums(tv_engagement_02)),
                                  scale(rowSums(internet_engagement_02))))
names(media_type_02) <- c("qn",
                          "newspapers",
                          "magazines",
                          "radio",
                          "tv",
                          "internet")
# Level 2: Vehicles
media_vehicles_02 <- data.frame(cbind(qn = print_02$qn,
                                      newspapers_engagement_02,
                                      magazines_engagement_02,
                                      radio_engagement_02,
                                      tv_engagement_02,
                                      internet_engagement_02))

saveRDS(media_type_02, 'media_type_02.rds')
saveRDS(media_vehicles_02, 'media_vehicles_02.rds')

media_type_02 <- readRDS('media_type_02.rds')
media_vehicles_02 <- readRDS('media_vehicles_02.rds')
## 4th Demographics Set (see notes for descriptions)

age <- personal_02[,'ca44co38']
sex <- demogrs_02[,'ca46co51a']
edu <- demogrs_02[,'ca46co48']
for(i in 1: length(edu)) {
        if(edu[i] %in% c(6,7)) {
                edu[i] <- edu[i] + 1
        }
        else if(edu[i] == 8) {
                edu[i] <- 6
        }
}
hh_inc <- demogrs_02[,'ca46co50']
race <- demogrs_02[,'ca46co51b']
# dataset: 1 = white, 2 = black, 3 = coloured, 4 = indian.
# 2012 dataset: 1 = black, 2 = coloured, 3 = indian, 4 = white
# change 2002 to 2012 codes for consistency: 1 to 4; 2 to 1; 3 to 2 and 4 to 3
race <- ifelse(race == 1, 4, race)
race <- ifelse(race == 2, 1, race)
race <- ifelse(race == 3, 2, race)
race <- ifelse(race == 4, 3, race)

province <- demogrs_02[,'ca46co56']
metro1 <- demogrs_02[,'ca46co57']
metro2 <- demogrs_02[,'ca46co58'] + 9
metro <- rowSums(cbind(metro1,
                       metro2), na.rm = TRUE)
#as in '95 and 2012 need to sort out double count of Soweto....
# seems that all the 19s are the sum of 7 & 12s (ie, Soweto)
# # code as such, ie all 19s are actually 12s (this also eliminates double count in the 7s ( so exlude Soweto)) >NB double check this is same in '95!!!
# check

# collect and code into single metro set:
#0 = no metro
#1 Cape Town
#2 Cape Town Fringe Area
#3 Port Elizabeth/Uitenhage
#4 East London
#5 Durban
#6 Bloemfontein
#7 Greater Johannesburg
#8 Reef
#9 Pretoria
#10 Kimberley
##11 Pietermaritzburg
##12 Vaal
##13 Welkom
metro <- ifelse(metro == 19, 7, metro)
metro <- ifelse(metro == 13, 12, metro)
table(metro) # yes, continue

lang <- demogrs_02[,'ca46co75'] + 1 # change 0 to 1, so add one to all
# change NAs to "other"
lang <- ifelse(is.na(lang), 12, lang) # 12 == other
lifestages <- demogrs_02[,'ca46co77'] # nb different categories from 2012

mar_status <- personal_02[,'ca44co09']
# pers_inc1 <- personal_02[,'ca57co61']
# pers_inc2 <- personal_02[,'ca57co62'] + 10
# pers_inc3 <- personal_02[,'ca57co63'] + 20
# pers_inc4 <- personal_02[,'ca57co64'] + 30
# for(i in 1: length(pers_inc4)) {
#         if(!is.na(pers_inc4[i])) {
#                 if(pers_inc4[i] == 31) {
#                         pers_inc4[i] <- 0
#                 }
#                 if(pers_inc4[i] == 32) {
#                         pers_inc4[i] <- 60
#                 }
#         }
# }
# pers_inc <- rowSums(cbind(pers_inc1,
#                           pers_inc2,
#                           pers_inc3,
#                           pers_inc4), na.rm = TRUE)
lsm <- lsm_02[,'ca46co64']
lsm <- ifelse(lsm == 0,10,lsm)

# lifestyle <- lsm_02[,'ca58co39'] + 1 # to get rid of zero

# attitudesA <- lsm_02[,'ca67co10'] + 1 # to get rid of zeros
# attitudesB <- lsm_02[,'ca67co10_lsm']
# attitudesA <- ifelse(is.na(attitudesA), 0, attitudesA)
# attitudesB <- ifelse(is.na(attitudesB), 0, attitudesB)
# attitudes <- attitudesA + attitudesB
# attitudes <- ifelse(attitudes == 8, 4, attitudes)
# attitudes <- ifelse(attitudes == 5 | attitudes == 6, attitudes + 1, attitudes)
# attitudes <- ifelse(attitudes == 9, 5, attitudes)
# table(attitudes) # check


demographics_02 <- data.frame(qn = print_02$qn, # no lifestyle or attitudes yet
                              pwgt = print_02$pwgt,
                              age,
                              sex,
                              edu,
                              hh_inc,
                              race,
                              province,
                              metro,
                              lang,
                              lifestages,
                              mar_status,
                              lsm)


# save as

saveRDS(demographics_02, "demographics_02.rds")
demographics_02 <- readRDS("demographics_02.rds")
