# libraries
library(stringr)
library(tidyverse)
library(caret)

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


# quick chck on actual "age"
ca44co39
summary(personal_02$ca44co39)
sum(personal_02$ca44co39 == 999)

## ISSUES
names_issues_print_02 <- str_subset(print_02_labels, 'Number of different issues usually read or page through') %>%
        str_replace('.+\\s-', '') %>%
        str_trim()
vars_issues_print_02 <- str_subset(print_02_labels, 'Number of different issues usually read or page through') %>%
        str_replace('Number\\sof\\sdifferent.+', '') %>%
        str_trim()

##Newspapers
# # fix names and get rid of some and save
# names_newspapers_02_issues <- names_issues_print_02[c(1:39,77)]
# fix(names_newspapers_02_issues)
# saveRDS(names_newspapers_02_issues, "names_newspapers_02_issues.rds")
names_newspapers_02_issues <- readRDS("names_newspapers_02_issues.rds")

# vector of variables
vars_newspapers_02_issues <- vars_issues_print_02[c(1:39,77)]
issues_newspapers_02 <- print_02[,vars_newspapers_02_issues]

# Magazines
# # fix names and get rid of some (including MNet guides and save
# names_magazines_02_issues <- names_issues_print_02[c(78:84,87:99,103:106,109:129, 131:146, 148:157)]
# fix(names_magazines_02_issues)
# saveRDS(names_magazines_02_issues, "names_magazines_02_issues.rds")
names_magazines_02_issues <- readRDS("names_magazines_02_issues.rds")

# vector of variables
vars_magazines_02_issues <- vars_issues_print_02[c(78:84,87:99,103:106,109:129, 131:146, 148:157)]
issues_magazines_02 <- print_02[,vars_magazines_02_issues]

## THOUROUGHLY
names_thorough_print_02 <- str_subset(print_02_labels, 'How thoroughly respondent usually read') %>%
        str_replace('.+\\s-', '') %>%
        str_replace("\\'",'') %>%
        str_trim()

# # three (161 vs 158) more than in issues need to id them and possibly remove
# which(!names_thorough_print_02 %in% names_issues_print_02)
#154, 156, 158 (GQ , Y Mag and De Kat)
names_thorough_print_02 <- names_thorough_print_02[-c(154,156,158)]
names_thorough_print_02 <- names_issues_print_02 # now they the same

vars_thorough_print_02 <- str_subset(print_02_labels, 'How thoroughly respondent usually read') %>%
        str_replace('How\\sthoroughly.+', '') %>%
        str_trim()
vars_thorough_print_02 <- vars_thorough_print_02[-c(154,156,158)]

##Newspapers
# get names and get rid of some and save (already sorted above)
# names_newspapers_02_thorough <- names_thorough_print_02[c(1:39,77)]
# fix(names_newspapers_02_thorough)
# saveRDS(names_newspapers_02_issues, "names_newspapers_02_issues.rds")

# vector of variables
vars_newspapers_02_thorough <- vars_thorough_print_02[c(1:39,77)]
thorough_newspapers_02 <- print_02[,vars_newspapers_02_thorough]
thorough_newspapers_02 <- 7 - thorough_newspapers_02

# Magazines
# fix names and get rid of some and save
# names_magazines_02_thorough <- names_thorough_print_02[c(77:99,103:107,109:157)]
# fix(names_magazines_02_issues)
# saveRDS(names_magazines_02_issues, "names_magazines_02_issues.rds")

# vector of variables
vars_magazines_02_thorough <- vars_thorough_print_02[c(78:84,87:99,103:106,109:129, 131:146, 148:157)]
thorough_magazines_02 <- print_02[,vars_magazines_02_thorough]

# # need to reverse numbering to serve as weights (see value_lables text file):
thorough_magazines_02 <- 7 - thorough_magazines_02

# create datasets ...for newspapers and magazines:
newspapers_engagement_02_all <- issues_newspapers_02 * thorough_newspapers_02
names(newspapers_engagement_02_all) <- names_newspapers_02_issues
magazines_engagement_02_all <- issues_magazines_02 * thorough_magazines_02
names(magazines_engagement_02_all) <- names_magazines_02_issues

newspapers_engagement_02_simple_all <- issues_newspapers_02
names(newspapers_engagement_02_simple_all) <- names_newspapers_02_issues
magazines_engagement_02_simple_all <- issues_magazines_02
names(magazines_engagement_02_simple_all) <- names_magazines_02_issues

# # # replace NAs with zeros
newspapers_engagement_02_all[is.na(newspapers_engagement_02_all)] <- 0
magazines_engagement_02_all[is.na(magazines_engagement_02_all)] <- 0

newspapers_engagement_02_simple_all[is.na(newspapers_engagement_02_simple_all)] <- 0
magazines_engagement_02_simple_all[is.na(magazines_engagement_02_simple_all)] <- 0

# save (alls)
saveRDS(newspapers_engagement_02_all, "newspapers_engagement_02_all.rds")
saveRDS(magazines_engagement_02_all, "magazines_engagement_02_all.rds")
saveRDS(newspapers_engagement_02_simple_all, "newspapers_engagement_02_simple_all.rds")
saveRDS(magazines_engagement_02_simple_all, "magazines_engagement_02_simple_all.rds")

# CLEAN UP (for now not "simple")
# for newspapers: include "Herald on Sat" as "other.news"
other.news <- newspapers_engagement_02_all[,22]
newspapers_engagement_02 <- newspapers_engagement_02_all %>%
        mutate(other.news = other.news)
newspapers_engagement_02 <- newspapers_engagement_02[,-c(22)]

other.news_simple <- newspapers_engagement_02_simple_all[,22]
newspapers_engagement_02_simple <- newspapers_engagement_02_simple_all %>%
        mutate(other.news = other.news_simple)
newspapers_engagement_02_simple <- newspapers_engagement_02_simple[,-c(22)]

# for magazines - deal with it in vehicle_cleaning project
magazines_engagement_02 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/vehicle_cleaning/magazines_engagement_02.rds")
magazines_engagement_02_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/vehicle_cleaning/magazines_engagement_02_simple.rds")

# save them in this project
saveRDS(newspapers_engagement_02, "newspapers_engagement_02.rds")
saveRDS(magazines_engagement_02, "magazines_engagement_02.rds")
saveRDS(newspapers_engagement_02_simple, "newspapers_engagement_02_simple.rds")
saveRDS(magazines_engagement_02_simple, "magazines_engagement_02_simple.rds")

magazines_engagement_02 <- readRDS("magazines_engagement_02.rds")
newspapers_engagement_02 <- readRDS("newspapers_engagement_02.rds")
magazines_engagement_02_simple <- readRDS("magazines_engagement_02_simple.rds")
newspapers_engagement_02_simple <- readRDS("newspapers_engagement_02_simple.rds")

## 2nd Electronic Media Set
# RADIO

# # creating a names vector
# names_radio_02 <- electr_02_labels %>%
#         str_subset('.+Listened.+4\\sweeks') %>%
#         str_replace('.+Listened\\sto\\s','') %>%
#         str_replace('\\sin\\sthe\\spast.+','') 
# 
# # get rid of first: "any Radio services via a satellite transmission"
# names_radio_02 <- names_radio_02[-1]
# 
# # also get rid of the following summaries:
# # Total Radio (Any Radio) [22]
# # SABC African Language Services [23]
# # Total Community [24]
# # Limpopo Corridor [32]
# names_radio_02 <- names_radio_02[-c(22,23,24,32)]
# 
# # fix(names_radio_02)
# 
# saveRDS(names_radio_02, "names_radio_02.rds")
# fix(names_radio_02)
names_radio_02 <- readRDS('names_radio_02.rds')

# get data...
radio4weeks_02 <- electr_02[,str_detect(names(electr_02), 'ca54co\\d{2}_\\d')]
radio4weeks_02 <- radio4weeks_02[,-c(22,23,24,32)] # get rid of list described above

radio7days_02 <- electr_02[,str_detect(names(electr_02), 'ca50co\\d{2}_\\d')]
radio7days_02 <- radio7days_02[,-c(22,23,24,32)]  # get rid of list described above

radioYesterday_02 <- electr_02[,str_detect(names(electr_02), 'ca53co\\d{2}_\\d')]
radioYesterday_02 <- radioYesterday_02[,-c(22,23,24,32)]  # get rid of "unsure" and "none"

## checking to see if same stations in 3 levels
# first extract all the variable colnames
colnames_4weeks_02 <- names(radio4weeks_02)
colnames_7days_02 <- names(radio7days_02)
colnames_yesterday_02 <- names(radioYesterday_02)
# yes

# creating engagement set:
radio_engagement_02_all <- radio4weeks_02 + radio7days_02 + radioYesterday_02
names(radio_engagement_02_all) <- names_radio_02

saveRDS(radio_engagement_02_all, "radio_engagement_02_all.rds")
radio_engagement_02_all <- readRDS("radio_engagement_02_all.rds")

# AFTER CLEANING (see vehicle cleaning project)
radio_engagement_02 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/vehicle_cleaning/radio_engagement_02.rds")
# save in this space
saveRDS(radio_engagement_02, "radio_engagement_02.rds")

# ## TV
# names_tv_02 <- electr_02_labels %>%
#         str_subset('watched.+4\\sweeks') %>%
#         str_replace('.+watched\\s','') %>%
#         str_replace('in\\sthe\\spast\\s4\\sweeks','') %>%
#         str_trim()
# 
# # cut total tv, unsure, no tv, and mnet stuff
# names_tv_02 <- names_tv_02[-c(3:4,10:12)]
# saveRDS(names_tv_02, "names_tv_02.rds")
names_tv_02 <- readRDS("names_tv_02.rds")
# fix(names_tv_02)
# want to isolate only past 4 weeks
tv4weeks_02 <- electr_02[,c('ca38co9_1',
                            'ca38co9_2',
                            'ca38co9_5',
                            'ca38co9_6',
                            'ca38co9_7',
                            'ca38co17_4',
                            'ca38co17_5'
)]

# combine Bop and Other:
one_4 <- rowSums(tv4weeks_02[,c(1,6)])
two_4 <- ifelse(one_4 == 2, 1, one_4)
tv4weeks_02 <- tv4weeks_02 %>%
        select(-ca38co9_1, -ca38co17_4) %>%
        mutate(other = two_4)

# want to isolate only past 7 days...
tv7days_02 <- electr_02[,c('ca38co19_1',
                           'ca38co19_2',
                           'ca38co19_5',
                           'ca38co19_6',
                           'ca38co19_7',
                           'ca38co27_4',
                           'ca38co27_5'
)] 

# combine Bop and Other:
one_7 <- rowSums(tv7days_02[,c(1,6)])
two_7 <- ifelse(one_7 == 2, 1, one_7)
tv7days_02 <- tv7days_02 %>%
        select(-ca38co19_1, -ca38co27_4) %>%
        mutate(other = two_7)

# want to isolate only yesterday...(indexes w.r.t 4weeks that are missing here: 7, 10)
tvYesterday_02 <- electr_02[,c('ca38co29_1',
                               'ca38co29_2',
                               'ca38co29_5',
                               'ca38co29_6',
                               'ca38co29_7',
                               'ca38co37_4',
                               'ca38co37_5'
)]

# combine Bop and Other:
one_y <- rowSums(tvYesterday_02[,c(1,6)])
two_y <- ifelse(one_y == 2, 1, one_y)
tvYesterday_02 <- tvYesterday_02 %>%
        select(-ca38co29_1, -ca38co37_4) %>%
        mutate(other = two_y)

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
internet_engagement_02_simple <- internet_level1


saveRDS(internet_engagement_02, "internet_engagement_02.rds")
saveRDS(internet_engagement_02_simple, "internet_engagement_02_simple.rds")

internet_engagement_02 <- readRDS("internet_engagement_02.rds")
internet_engagement_02_simple <- readRDS("internet_engagement_02_simple.rds")

## create single dataframe for media02, including total_engagement columns)


# Level 1: Type
media_type_02 <- data.frame(cbind(qn = print_02$qn,
                                  rowSums(scale(newspapers_engagement_02)),
                                  rowSums(scale(magazines_engagement_02)),
                                  rowSums(scale(radio_engagement_02)),
                                  rowSums(scale(tv_engagement_02)),
                                  rowSums(scale(internet_engagement_02))))
names(media_type_02) <- c("qn",
                          "newspapers",
                          "magazines",
                          "radio",
                          "tv",
                          "internet")

media_type_02 <- media_type_02 %>%
        mutate(all = as.vector(newspapers + magazines + radio + tv + internet))


media_type_02_simple <- data.frame(cbind(qn = print_02$qn,
                                         rowSums(scale(newspapers_engagement_02_simple)),
                                         rowSums(scale(magazines_engagement_02_simple)),
                                         rowSums(scale(radio_engagement_02)),
                                         rowSums(scale(tv_engagement_02)),
                                         scale(internet_engagement_02_simple)))

names(media_type_02_simple) <- c("qn",
                          "newspapers",
                          "magazines",
                          "radio",
                          "tv",
                          "internet")
media_type_02_simple <- media_type_02_simple %>%
        mutate(all = as.vector(newspapers + magazines + radio + tv + internet))

# Level 2: Vehicles
media_vehicles_02 <- data.frame(cbind(qn = print_02$qn,
                                      newspapers_engagement_02,
                                      magazines_engagement_02,
                                      radio_engagement_02,
                                      tv_engagement_02,
                                      internet_engagement_02))
media_vehicles_02_simple <- data.frame(cbind(qn = print_02$qn,
                                      newspapers_engagement_02_simple,
                                      magazines_engagement_02_simple,
                                      radio_engagement_02,
                                      tv_engagement_02,
                                      internet_eng = internet_engagement_02_simple))


saveRDS(media_type_02, 'media_type_02.rds')
saveRDS(media_type_02_simple, 'media_type_02_simple.rds')

saveRDS(media_vehicles_02, 'media_vehicles_02.rds')
saveRDS(media_vehicles_02_simple, 'media_vehicles_02_simple.rds')


media_type_02 <- readRDS('media_type_02.rds')
media_type_02_simple <- readRDS('media_type_02_simple.rds')
media_vehicles_02 <- readRDS('media_vehicles_02.rds')
media_vehicles_02_simple <- readRDS('media_vehicles_02_simple.rds')

## 4th Demographics Set (see notes for descriptions)
age <- personal_02[,'ca44co38']
age_actual <- personal_02[,'ca44co39'] # actual age..note some 999 = refusal or dont know

sex <- demogrs_02[,'ca46co51a']

edu_alt <- personal_02[,'ca44co42'] # gives more sensible and additional level
#ca44co42:
# 1 No schooling
# 2 Some primary school
# 3 Primary school completed
# 4 Some high school
# 5 Matric (Grade 12)
# 6 Artisan's certificate obtained
# 7 Technikon diploma
# 8 University degree completed
# 9 Professional

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

# more levels for numeric treatment later on
hh_inc1 <- personal_02[,'ca45co35'] # 1 - 9
hh_inc2 <- personal_02[,'ca45co36'] # 0 - 9 == 10 - 19
hh_inc3 <- personal_02[,'ca45co37'] # 0 - 9 == 20 - 29
hh_inc4 <- personal_02[,'ca45co38'] # 0 - 2 == 30 - 32

hh_bind <- cbind.data.frame(hh_inc1,(10 + hh_inc2), (20 + hh_inc3),(30 + hh_inc4) )
hh_bind[is.na(hh_bind)] <- 0
hh_inc_alt <- rowSums(hh_bind)
# ca45co38:
#         0 R25 000-R29 999
# 1 R30 000-R39 999
# 2 R40 000+
#         3 No personal income
# 4 Refused
# ca45co37:
#         0 R6 000-R6 999
# 1 R7 000-R7 999
# 2 R8 000-R8 999
# 3 R9 000-R9 999
# 4 R10 000-R10 999
# 5 R11 000-R11 999
# 6 R12 000-R13 999
# 7 R14 000-R15 999
# 8 R16 000-R19 999
# 9 R20 000-R24 999
# ca45co36:
#         0 R1 000-R1 099
# 1 R1 100-R1 199
# 2 R1 200-R1 399
# 3 R1 400-R1 599
# 4 R1 600-R1 999
# 5 R2 000-R2 499
# 6 R2 500-R2 999
# 7 R3 000-R3 999
# 8 R4 000-R4 999
# 9 R5 000-R5 999
# ca45co35:
#         1 R1-R199
# 2 R200-R299
# 3 R300-R399
# 4 R400-R499
# 5 R500-R599
# 6 R600-R699
# 7 R700-R799
# 8 R800-R899
# 9 R900-R999

race <- demogrs_02[,'ca46co51b']
# dataset: 1 = white, 2 = black, 3 = coloured, 4 = indian.
# 2012 dataset: 1 = black, 2 = coloured, 3 = indian, 4 = white
# change 2002 to 2012 codes for consistency: 1 to 4; 2 to 1; 3 to 2 and 4 to 3

race <- ifelse(race == 1, 9, race)
race <- ifelse(race == 2, 6, race)
race <- ifelse(race == 3, 7, race)
race <- ifelse(race == 4, 8, race)
race <- race - 5

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

lsm <- lsm_02[,'ca46co64']
lsm <- ifelse(lsm == 0,10,lsm)

# no lifestyle or attitudes yet.

demographics_02 <- data.frame(qn = print_02$qn,
                              pwgt = print_02$pwgt,
                              age,
                              # age_actual,
                              sex,
                              edu,
                              # edu_alt
                              hh_inc,
                              # hh_inc_alt
                              race,
                              province,
                              metro,
                              lang,
                              lifestages,
                              mar_status,
                              lsm)

#reducing levels of categorical variables and setting factor types for demographics:
# age:
demographics_02$age <- ifelse(demographics_02$age %in% c(1,2), 1, demographics_02$age)
demographics_02$age <- ifelse(demographics_02$age %in% c(3,4), 2, demographics_02$age)
demographics_02$age <- ifelse(demographics_02$age %in% c(5,6), 3, demographics_02$age)
demographics_02$age <- ifelse(demographics_02$age %in% c(7,8), 4, demographics_02$age)
demographics_02$age <- factor(demographics_02$age, ordered = TRUE)

# sex:
demographics_02$sex <- factor(demographics_02$sex, ordered = FALSE)

#edu:
demographics_02$edu <- ifelse(demographics_02$edu %in% c(1,2,3,4), 1, demographics_02$edu)
demographics_02$edu <- ifelse(demographics_02$edu %in% c(5), 2, demographics_02$edu)
demographics_02$edu <- ifelse(demographics_02$edu %in% c(6,7,8), 3, demographics_02$edu)
demographics_02$edu <- factor(demographics_02$edu, ordered = TRUE)

#hh_inc
demographics_02$hh_inc <- ifelse(demographics_02$hh_inc %in% c(1,2,3,4), 1, demographics_02$hh_inc)
demographics_02$hh_inc <- ifelse(demographics_02$hh_inc %in% c(5,6), 2, demographics_02$hh_inc)
demographics_02$hh_inc <- ifelse(demographics_02$hh_inc %in% c(7), 3, demographics_02$hh_inc)
demographics_02$hh_inc <- ifelse(demographics_02$hh_inc %in% c(8), 4, demographics_02$hh_inc)
demographics_02$hh_inc <- factor(demographics_02$hh_inc, ordered = TRUE)

demographics_02$race <- factor(demographics_02$race, ordered = FALSE)
demographics_02$province <- factor(demographics_02$province, ordered = FALSE)
demographics_02$metro <- factor(demographics_02$metro, ordered = FALSE)
demographics_02$lang <- factor(demographics_02$lang, ordered = FALSE)
demographics_02$lifestages <- factor(demographics_02$lifestages, ordered = FALSE)
demographics_02$mar_status <- factor(demographics_02$mar_status, ordered = FALSE)
# demographics_02$pers_inc <- factor(demographics_02$pers_inc, ordered = TRUE)

# lsm
demographics_02$lsm <- ifelse(demographics_02$lsm %in% c(1,2), 1, demographics_02$lsm)
demographics_02$lsm <- ifelse(demographics_02$lsm %in% c(3,4), 2, demographics_02$lsm)
demographics_02$lsm <- ifelse(demographics_02$lsm %in% c(5,6), 3, demographics_02$lsm)
demographics_02$lsm <- ifelse(demographics_02$lsm %in% c(7,8), 4, demographics_02$lsm)
demographics_02$lsm <- ifelse(demographics_02$lsm %in% c(9,10), 5, demographics_02$lsm)
demographics_02$lsm <- factor(demographics_02$lsm, ordered = TRUE)

# demographics_02$lifestyle <- factor(demographics_02$lifestyle, ordered = FALSE) # not for 2002 yet
# demographics_02$attitudes <- factor(demographics_02$attitudes, ordered = FALSE) # not for 2002 yet

# save
saveRDS(demographics_02, "demographics_02.rds")
demographics_02 <- readRDS("demographics_02.rds")

# read datafiles if necessary
magazines_engagement_02 <- readRDS("magazines_engagement_02.rds")
magazines_engagement_02_simple <- readRDS("magazines_engagement_02_simple.rds")

newspapers_engagement_02 <- readRDS("newspapers_engagement_02.rds")
newspapers_engagement_02_simple <- readRDS("newspapers_engagement_02_simple.rds")

radio_engagement_02 <- readRDS("radio_engagement_02.rds")

tv_engagement_02 <- readRDS("tv_engagement_02.rds")

internet_engagement_02 <- readRDS("internet_engagement_02.rds")
internet_engagement_02_simple <- readRDS("internet_engagement_02_simple.rds")


media_type_02 <- readRDS("media_type_02.rds")
media_type_02_simple <- readRDS("media_type_02_simple.rds")
media_vehicles_02 <- readRDS("media_vehicles_02.rds")
media_vehicles_02_simple <- readRDS("media_vehicles_02_simple.rds")

demographics_02 <- readRDS("demographics_02.rds")

# #create single dataset minus non metropolitans
set02 <- demographics_02 %>%
        left_join(media_type_02) %>%
        left_join(media_vehicles_02)
#%>%filter(metro != 0)
set02_simple <- demographics_02 %>%
        left_join(media_type_02_simple) %>%
        left_join(media_vehicles_02_simple)
#%>%filter(metro != 0)

# get rid of zero variances:
ind_02 <- nearZeroVar(set02[,14:ncol(set02)], saveMetrics = TRUE)
good_set <- set02[,14:ncol(set02)][,!ind_02$zeroVar]
set02 <- data.frame(cbind(set02[,1:13], good_set))

ind_02_simple <- nearZeroVar(set02_simple[,14:ncol(set02_simple)], saveMetrics = TRUE)
good_set_simple <- set02_simple[,14:ncol(set02_simple)][,!ind_02_simple$zeroVar]
set02_simple <- data.frame(cbind(set02_simple[,1:13], good_set_simple))

# scale media type and media vehicles
set02[,14:ncol(set02)] <- scale(set02[,14:ncol(set02)])
set02_simple[,14:ncol(set02_simple)] <- scale(set02_simple[,14:ncol(set02_simple)])

# save them:
saveRDS(set02, "set02.rds")
saveRDS(set02_simple, "set02_simple.rds")

