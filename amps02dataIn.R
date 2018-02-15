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

load("labels_12.RData")

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

names_radio_12_4w <- electr_02_labels %>%
        str_subset('ca64co\\d{2}_\\d') %>%
        str_replace('.+listened.+4\\sweeks\\s-\\s','')
names_radio_12_4w <- names_radio_12_4w[-c(98,99)] # get rid of "unsure" and "none"

names_radio_12_7 <- electr_02_labels %>%
        str_subset('ca65co\\d{2}_\\d') %>%
        str_replace('.+listened.+7\\sdays\\s-\\s','')
names_radio_12_7 <- names_radio_12_7[-c(81, 87,88)] # get rid of "unsure" and "none" & empty one 

names_radio_12_y <- electr_02_labels %>%
        str_subset('ca66co\\d{2}_\\d') %>%
        str_replace('.+listened\\sto\\syesterday\\s-\\s','')
names_radio_12_y <- names_radio_12_y[-c(64,65)] # get rid of "unsure" and "none"


# # most radio stations in 4 weeks, so use that to create names list
# names_radio_12 <- names_radio_12_4w
# fix(names_radio_12)
saveRDS(names_radio_12, "names_radio_12.rds")
names_radio_12 <- readRDS('names_radio_12.rds')

# get data...
radio4weeks_12 <- electr_02[,str_detect(names(electr_02), 'ca64co\\d{2}_\\d')]
radio4weeks_12 <- radio4weeks_12[,-c(98,99)] # get rid of "unsure" and "none"

radio7days_12 <- electr_02[,str_detect(names(electr_02), 'ca65co\\d{2}_\\d')]
radio7days_12 <- radio7days_12[,-c(81, 87,88)]  # get rid of "unsure" and "none" & empty one 

radioYesterday_12 <- electr_02[,str_detect(names(electr_02), 'ca66co\\d{2}_\\d')]
radioYesterday_12 <- radioYesterday_12[,-c(64,65)]  # get rid of "unsure" and "none"

# identifying missing stations by changing all to "64"
a <- names(radio4weeks_12)
b <- names(radio7days_12)
c <- names(radioYesterday_12)
b_adj <- b %>%
        str_replace("65", "64")
c_adj <- c %>%
        str_replace("66", "64")

names(radio7days_12) <- b_adj
names(radioYesterday_12) <- c_adj

ind_7 <- which(names(radio4weeks_12) %in% names(radio7days_12))
ind_y <- which(names(radio4weeks_12) %in% names(radioYesterday_12))

# adding up
radio4weeks_12[,ind_7] <- radio4weeks_12[,ind_7] + radio7days_12
radio4weeks_12[,ind_y] <- radio4weeks_12[,ind_y] + radioYesterday_12

# creating engagement set:
radio_engagement_12 <- radio4weeks_12
names(radio_engagement_12) <- names_radio_12

saveRDS(radio_engagement_12, "radio_engagement_12.rds")
radio_engagement_12 <- readRDS("radio_engagement_12.rds")

## TV (this year, included specific dstv and toptv channels (will include them))
names_tv_12 <- electr_02_labels %>%
        str_subset('Watched.+4\\sWEEKS') %>%
        str_replace('.+Watched\\s','') %>%
        str_replace('in\\sthe\\sPAST\\s4\\sWEEKS','') %>%
        str_trim()

saveRDS(names_tv_12, "names_tv_12.rds")
names_tv_12 <- readRDS("names_tv_12.rds")

# want to isolate only past 4 weeks and get rid of ("UNSURE", and "no TV")
tv4weeks_12 <- electr_02[,c('ca45co30_1',
                            'ca45co30_2',
                            'ca45co30_3',
                            'ca45co30_4',
                            'ca45co30_5',
                            'ca45co30_6',
                            'ca45co30_7',
                            'ca45co30_8',
                            'ca45co30_9',
                            'ca45co31_0',
                            'ca45co72_3',
                            'ca45co72_8'
)] 

# want to isolate only past 7 days...
tv7days_12 <- electr_02[,c('ca45co32_1',
                           'ca45co32_2',
                           'ca45co32_3',
                           'ca45co32_4',
                           'ca45co32_5',
                           'ca45co32_6',
                           'ca45co32_7',
                           'ca45co32_8',
                           'ca45co32_9',
                           'ca45co33_0',
                           'ca45co74_3',
                           'ca45co74_8'
)] 

# want to isolate only yesterday...(indexes w.r.t 4weeks that are missing here: 7, 10)
tvYesterday_12 <- electr_02[,c('ca45co34_1',
                               'ca45co34_2',
                               'ca45co34_3',
                               'ca45co34_4',
                               'ca45co34_5',
                               'ca45co34_6',
                               'ca45co34_8',
                               'ca45co34_9',
                               'ca45co76_3',
                               'ca45co76_8'
)]

# combining into a tv engagement dataset (using tv4weeks_12 as basis):

tv_engagement_12 <- tv4weeks_12 + tv7days_12
tv_engagement_12[,-c(7,10)] <- tv_engagement_12[,-c(7,10)] + tvYesterday_12
names(tv_engagement_12) <- names_tv_12

saveRDS(tv_engagement_12, "tv_engagement_12.rds")

tv_engagement_12 <- readRDS("tv_engagement_12.rds")

## 3rd Internet Media Set

## accessed: sum of 12 months, 4weeks, 7days and yesterday
internet_level1 <- internet_02[,str_detect(names(internet_02), 'ca49co(45)|(46)|(47)|(48)')]

#change all 2 = "No" and NA's' to 0
for(i in 1: nrow(internet_level1)) {
        for(j in 1: ncol(internet_level1)) {
                if(is.na(internet_level1[i,j]) | internet_level1[i,j] == 2){
                        internet_level1[i,j] <- 0
                }
        }
}

internet_level1 <- rowSums(internet_level1)

# what internet was accessed for...
##  (maybe could use similar to vehicles?? as well as add up and multiply with first eng):

internet_level2 <- internet_02[,str_detect(names(internet_02), 'ca49co(55)|(58)|(63)|(64)|(69)|(71)')]

# change NA and 3 = 0; 1,2,4 = 1
for(i in 1: nrow(internet_level2)) {
        for(j in 1: ncol(internet_level2)) {
                if(is.na(internet_level2[i,j]) | internet_level2[i,j] == 3){
                        internet_level2[i,j] <- 0
                }
                else {
                        internet_level2[i,j] <- 1
                }
        }
}

names(internet_level2) <- c('int_search',
                            'int_social',
                            'int_print',
                            'int_news',
                            'int_tv',
                            'int_radio')

## create single dataframe for internet multiplying internet_level1 with sum of internet_level2:
internet_engagement_12 <- internet_level2  * internet_level1

saveRDS(internet_engagement_12, "internet_engagement_12.rds")

internet_engagement_12 <- readRDS("internet_engagement_12.rds")

## create single dataframe for media12, including total_engagement columns (consider using media groupings .. follow up on this!)

# Level 1: Type
media_type_12 <- data.frame(cbind(qn = print_02$qn,
                                  scale(rowSums(newspapers_engagement_02)),
                                  scale(rowSums(magazines_engagement_02)),
                                  scale(rowSums(radio_engagement_12)),
                                  scale(rowSums(tv_engagement_12)),
                                  scale(rowSums(internet_engagement_12))))
names(media_type_12) <- c("qn",
                          "newspapers",
                          "magazines",
                          "radio",
                          "tv",
                          "internet")
# Level 2: Vehicles
media_vehicles_12 <- data.frame(cbind(qn = print_02$qn,
                                      newspapers_engagement_02,
                                      magazines_engagement_02,
                                      radio_engagement_12,
                                      tv_engagement_12,
                                      internet_engagement_12))

saveRDS(media_type_12, 'media_type_12.rds')
saveRDS(media_vehicles_12, 'media_vehicles_12.rds')

## 4th Demographics Set (see notes for descriptions)

age <- personal_02[,'ca56co34']
sex <- demogrs_02[,'ca91co51a']
edu <- demogrs_02[,'ca91co48']
for(i in 1: length(edu)) {
        if(edu[i] %in% c(6,7)) {
                edu[i] <- edu[i] + 1
        }
        else if(edu[i] == 8) {
                edu[i] <- 6
        }
}
hh_inc <- demogrs_02[,'ca91co50']
race <- demogrs_02[,'ca91co51b']
province <- demogrs_02[,'ca91co56']
metro1 <- demogrs_02[,'ca91co57']
metro2 <- demogrs_02[,'ca91co58'] + 9
metro <- rowSums(cbind(metro1,
                       metro2), na.rm = TRUE)
#as in '95 need to sort out double count of Soweto....
# seems that all the 19s are the sum of 7 & 12s (ie, Soweto)
# # code as such, ie all 19s are actually 12s (this also eliminates double count in the 7s ( so exlude Soweto)) >NB double check this is same in '95!!!
metro <- ifelse(metro == 19, 12, metro)
lang <- demogrs_02[,'ca91co75'] + 1 # change 0 to 1, so add one to all
lifestages <- demogrs_02[,'ca91co77']
mar_status <- personal_02[,'ca56co09']
pers_inc1 <- personal_02[,'ca57co61']
pers_inc2 <- personal_02[,'ca57co62'] + 10
pers_inc3 <- personal_02[,'ca57co63'] + 20
pers_inc4 <- personal_02[,'ca57co64'] + 30
for(i in 1: length(pers_inc4)) {
        if(!is.na(pers_inc4[i])) {
                if(pers_inc4[i] == 31) {
                        pers_inc4[i] <- 0
                }
                if(pers_inc4[i] == 32) {
                        pers_inc4[i] <- 60
                }
        }
}
pers_inc <- rowSums(cbind(pers_inc1,
                          pers_inc2,
                          pers_inc3,
                          pers_inc4), na.rm = TRUE)
lsm <- lsm_02[,'ca91co64']
lsm <- ifelse(lsm == 0,10,lsm)

lifestyle <- lsm_02[,'ca58co39'] + 1 # to get rid of zero

attitudesA <- lsm_02[,'ca67co10'] + 1 # to get rid of zeros
attitudesB <- lsm_02[,'ca67co10_lsm']
attitudesA <- ifelse(is.na(attitudesA), 0, attitudesA)
attitudesB <- ifelse(is.na(attitudesB), 0, attitudesB)
attitudes <- attitudesA + attitudesB
attitudes <- ifelse(attitudes == 8, 4, attitudes)
attitudes <- ifelse(attitudes == 5 | attitudes == 6, attitudes + 1, attitudes)
attitudes <- ifelse(attitudes == 9, 5, attitudes)
table(attitudes) # check


demographics_12 <- data.frame(qn = print_02$qn,
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
                              pers_inc,
                              lsm,
                              lifestyle,
                              attitudes)


# save as

saveRDS(demographics_12, "demographics_12.rds")
demographics_12 <- readRDS("demographics_12.rds")
