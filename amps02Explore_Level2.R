# libraries
library(stringr)
library(tidyverse)
library(corrplot)
library(rpart)
library(rpart.plot)
library(scatterplot3d)
library(rgl)
library(kohonen)
library(caret)
library(randomForest)
library(MASS)
library(CCA)
library(nFactors)
library(FactoMineR)
library(factoextra)
library(gridExtra)
library(ggplot2)

# load datafiles 
set02c <- readRDS("set02c.rds")
# set02c_simple <- readRDS("set02c_simple.rds")

# LEVEL 2

# subset by metropoles
# focus on media vehicles cape town and jhb
# cape town: metro == 1 | 2
# greater jhb: metro == 7

# isolate cape town
set02_CT <- set02c %>% filter(metro == 1) 


 # try with nationals
set02_CT <- set02_nat


# isolate johannesburg
set02_JHB <- set02c %>% filter(metro == 7)

# get rid of near zero variances:
ind_ct <- nearZeroVar(set02_CT[,21:ncol(set02_CT)], saveMetrics = TRUE)
ind_jhb <- nearZeroVar(set02_JHB[,21:ncol(set02_JHB)], saveMetrics = TRUE)

good_ct <- set02_CT[,21:ncol(set02_CT)][,!ind_ct$zeroVar]
good_jhb <- set02_JHB[,21:ncol(set02_JHB)][,!ind_jhb$nzv]

catSet02_CT <- data.frame(cbind(set02_CT[,1:20], good_ct))
catSet02_JHB <- data.frame(cbind(set02_JHB[,1:20], good_jhb))

saveRDS(catSet02_CT, "catSet02_CT.rds")
saveRDS(catSet02_JHB, "catSet02_JHB.rds")

nuSet02_CT <- data.frame(cbind(set02_CT[,1:20], good_ct))
nuSet02_JHB <- data.frame(cbind(set02_JHB[,1:20], good_jhb))

#setting the ordered variables as scaled numerical:
nuSet02_CT$age <- scale(as.numeric(nuSet02_CT$age))
nuSet02_CT$edu <- scale(as.numeric(nuSet02_CT$edu))
nuSet02_CT$hh_inc <- scale(as.numeric(nuSet02_CT$hh_inc))
nuSet02_CT$lsm <- scale(as.numeric(nuSet02_CT$lsm))

nuSet02_JHB$age <- scale(as.numeric(nuSet02_JHB$age))
nuSet02_JHB$edu <- scale(as.numeric(nuSet02_JHB$edu))
nuSet02_JHB$hh_inc <- scale(as.numeric(nuSet02_JHB$hh_inc))
nuSet02_JHB$lsm <- scale(as.numeric(nuSet02_JHB$lsm))

# naming the factors

# Cape Town
nuSet02_CT$cluster <- factor(nuSet02_CT$cluster,
                             levels = c(1,2,3,4),
                             labels = c("cluster1", "cluster2", "cluster3", "cluster4"))
nuSet02_CT$sex <- factor(nuSet02_CT$sex,
                         levels = c(1,2),
                         labels = c("male", "female"))
nuSet02_CT$race <- factor(nuSet02_CT$race,
                          levels = c(1,2,3,4),
                          labels = c("black", "coloured", "indian", "white"))
nuSet02_CT$lifestages <- factor(nuSet02_CT$lifestages,
                                levels = c(1,2,3,4,5,6,7,8),
                                labels = c("at home singles", "young independent singles", "mature singles", "young couples", "mature couples", "young family", "single parent family", "mature family"))
nuSet02_CT$mar_status <- factor(nuSet02_CT$mar_status,
                                levels = c(1,2,3,4,5),
                                labels = c("single", "married or living together", "widowed", "divorced", "separated"))
# nuSet02_CT$lifestyle <- factor(nuSet02_CT$lifestyle,
#                                levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
#                                labels = c("none", "cell sophisticates", "sports", "gamers", "outdoors", "good living", "avid readers", "traditionalists","cell fundamentals", "homebodies", "studious", "showgoers"))
# nuSet02_CT$attitudes <- factor(nuSet02_CT$attitudes,
#                                levels = c(1,2,3,4,5,6,7),
#                                labels = c("none", "now generation", "nation builders", "distants survivors", "distants established", "rooted", "global citizens"))

# Johannesburg
# 
nuSet02_JHB$cluster <- factor(nuSet02_JHB$cluster,
                              levels = c(1,2,3,4),
                              labels = c("cluster1", "cluster2", "cluster3", "cluster4"))
nuSet02_JHB$sex <- factor(nuSet02_JHB$sex,
                          levels = c(1,2),
                          labels = c("male", "female"))
nuSet02_JHB$race <- factor(nuSet02_JHB$race,
                           levels = c(1,2,3,4),
                           labels = c("black", "coloured", "indian", "white"))
nuSet02_JHB$lifestages <- factor(nuSet02_JHB$lifestages,
                                 levels = c(1,2,3,4,5,6,7,8),
                                 labels = c("at home singles", "young independent singles", "mature singles", "young couples", "mature couples", "young family", "single parent family", "mature family"))
nuSet02_JHB$mar_status <- factor(nuSet02_JHB$mar_status,
                                 levels = c(1,2,3,4,5),
                                 labels = c("single", "married or living together", "widowed", "divorced", "separated"))
# nuSet02_JHB$lifestyle <- factor(nuSet02_JHB$lifestyle,
#                                 levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
#                                 labels = c("none", "cell sophisticates", "sports", "gamers", "outdoors", "good living", "avid readers", "traditionalists","cell fundamentals", "homebodies", "studious", "showgoers"))
# nuSet02_JHB$attitudes <- factor(nuSet02_JHB$attitudes,
#                                 levels = c(1,2,3,4,5,6,7),
#                                 labels = c("none", "now generation", "nation builders", "distants survivors", "distants established", "rooted", "global citizens"))

# focussing only on the variable I intend to use in this section:
nuSet02_CT <- nuSet02_CT[,-c(1:2,9:13,15:20)]
nuSet02_JHB <- nuSet02_JHB[,-c(1:2,9:13,15:20)]

# saving these objects:
saveRDS(nuSet02_CT, "nuSet02_CT.rds")
saveRDS(nuSet02_JHB, "nuSet02_JHB.rds")

nuSet02_CT <- readRDS("nuSet02_CT.rds")
nuSet02_JHB <- readRDS("nuSet02_JHB.rds")
# 
# ## Determine Number of Factors to Extract
# ev_ct <- eigen(cor(nuSet02_CT[,8:ncol(nuSet02_CT)]))
# ap_ct <- parallel(subject=nrow(nuSet02_CT[,8:ncol(nuSet02_CT)]),var=ncol(nuSet02_CT[,8:ncol(nuSet02_CT)]),
#                rep=100,cent=.02)
# nS_ct <- nScree(x=ev_ct$values, aparallel=ap_ct$eigen$qevpea)
# jpeg("nScree_02_ct")
# plotnScree(nS_ct, main = "Cape Town") # optimal = 7
# dev.off()
# 
# ev_jhb <- eigen(cor(nuSet02_JHB[,8:ncol(nuSet02_JHB)]))
# ap_jhb <- parallel(subject=nrow(nuSet02_JHB[,8:ncol(nuSet02_JHB)]),var=ncol(nuSet02_JHB[,8:ncol(nuSet02_JHB)]),
#                rep=100,cent=.02)
# nS_jhb <- nScree(x=ev_jhb$values, aparallel=ap_jhb$eigen$qevpea)
# jpeg("nScree_02_jhb")
# plotnScree(nS_jhb, main = "Johannesburg") #
# dev.off()

# npc_ct <- nS_ct$Components$noc
# npc_jhb <- nS_jhb$Components$noc

# will set them at six for both Jhb and CT for now
npc_ct <- 6
npc_jhb <- 6

# creating objects with supplementary variables (qualitative and quantitative) and active one defined:
set.seed(56)
pca_02_ct <- PCA(nuSet02_CT,
                 quanti.sup = c(2,4,5,7),
                 quali.sup = c(1,3,6),
                 ncp = npc_ct,
                 graph = FALSE)
set.seed(56)
pca_02_jhb <- PCA(nuSet02_JHB,
                  quanti.sup = c(2,4,5,7),
                  quali.sup = c(1,3,6),
                  ncp = npc_jhb,
                  graph = FALSE)
# save for later use:

saveRDS(pca_02_ct, "pca_02_ct.rds")
saveRDS(pca_02_jhb, "pca_02_jhb.rds")


# # try FactoInvestigate
# library(FactoInvestigate)
# Investigate(pca_02_ct)
# Investigate(pca_02_jhb)

# cape town contributions plots
jpeg("contributions02_ct_1n2.jpeg")
fviz_pca_var(pca_02_ct,
             axes = c(1,2),
             col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "Cape Town",
             repel = TRUE # Avoid text overlapping
)
dev.off()

jpeg("contributions02_ct_3n4.jpeg")
fviz_pca_var(pca_02_ct,
             axes = c(3,4),
             col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "Cape Town",
             repel = TRUE # Avoid text overlapping
)
dev.off()

jpeg("contributions02_ct_5n6.jpeg")
fviz_pca_var(pca_02_ct,
             axes = c(5,6),
             col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "Cape Town",
             repel = TRUE # Avoid text overlapping
)
dev.off()

# jpeg("contributions02_ct_6n7.jpeg")
# fviz_pca_var(pca_02_ct,
#              axes = c(6,7),
#              col.var="contrib",
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              title = "Cape Town",
#              repel = TRUE # Avoid text overlapping
# )
# dev.off()

# for the six jhb dimensions
jpeg("contributions02_jhb_1m2.jpeg")
fviz_pca_var(pca_02_jhb,
             axes = c(1,2),
             col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "Johannesburg",
             repel = TRUE # Avoid text overlapping
)
dev.off()

jpeg("contributions02_jhb_3m4.jpeg")
fviz_pca_var(pca_02_jhb,
             axes = c(3,4),
             col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "Johannesburg",
             repel = TRUE # Avoid text overlapping
)
dev.off()

jpeg("contributions02_jhb_5m6.jpeg")
fviz_pca_var(pca_02_jhb,
             axes = c(5,6),
             col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "Johannesburg",
             repel = TRUE # Avoid text overlapping
)
dev.off()

# can identify most important
# # high absolute values describe variables that best describe a particular dimension.
# and a negatively correlated value (eg Kickoff) means that individuals who have high coordinates on the given dimension would have a low value for engagement with kickoff... (see FactoMineR document p 8)

# create dataframe based on correlations > plus or minus 0.3
# create dataframe of values:

# for cape town
vehicle_ct <- rownames(pca_02_ct$var$cor)
corr_ct <- as.data.frame(pca_02_ct$var$cor)
contrib_ct <- as.data.frame(pca_02_ct$var$contrib)
cos2_ct <- as.data.frame(pca_02_ct$var$cos2)

dims_ct <- list()
for(i in 1:npc_ct) {
        temp <- data.frame(vehicle_ct, corr = corr_ct[,i], contrib = contrib_ct[,i], cos2 = cos2_ct[,i]) %>%
                filter(corr > 0.3 | corr < -0.3) %>%
                arrange(desc(corr))
        rownames(temp) <- temp$vehicle
        dims_ct[[i]] <- temp[,-1]
}

# for johannesburg
vehicle_jhb <- rownames(pca_02_jhb$var$cor)
corr_jhb <- as.data.frame(pca_02_jhb$var$cor, row.names = '')
contrib_jhb <- as.data.frame(pca_02_jhb$var$contrib, row.names = '')
cos2_jhb <- as.data.frame(pca_02_jhb$var$cos2, row.names = '')

dims_jhb <- list()
for(i in 1:npc_jhb) {
        temp <- data.frame(vehicle_jhb, corr = corr_jhb[,i], contrib = contrib_jhb[,i], cos2 = cos2_jhb[,i]) %>%
                filter(corr > 0.3 | corr < -0.3) %>%
                arrange(desc(corr))
        rownames(temp) <- temp$vehicle
        dims_jhb[[i]] <- temp[,-1]
}

# Dimension Tables Information Cape Town 1 & 2:
# for dimension 1
tab_ct_1 <- tableGrob(round(dims_ct[[1]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_ct_1 <- grobHeight(tab_ct_1)
w_ct_1 <- grobWidth(tab_ct_1)
title_ct_1 <- textGrob("Dimension 1", y=unit(0.5,"npc") + 0.5*h_ct_1, 
                       vjust=-6, hjust = 0.2, gp=gpar(fontsize=14)) # title
gt_ct_1 <- gTree(children = gList(tab_ct_1, title_ct_1)) #,footnote
#grid.draw(gt_ct_1) # check

# for dimension 2
tab_ct_2 <- tableGrob(round(dims_ct[[2]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_ct_2 <- grobHeight(tab_ct_1)
w_ct_2 <- grobWidth(tab_ct_1)
title_ct_2 <- textGrob("Dimension 2", y=unit(0.5,"npc") + 0.5*h_ct_2, 
                       vjust=-6, hjust = 0.2, gp=gpar(fontsize=14)) # title
gt_ct_2 <- gTree(children = gList(tab_ct_2, title_ct_2)) #,footnote
# grid.draw(gt_ct_2) # check

# arrange two Dimensions on one plot and print to interactive graphic with latex
ml_ct_1n2 <- marrangeGrob(list(gt_ct_1,gt_ct_2), nrow=1, ncol=2, top = '\n\n\n\nCape Town')

# print to graphic
jpeg("dims02_ct_1n2.jpeg")
ml_ct_1n2
dev.off()

# JHB 1 & 2:
# for dimension 1
# given so many, will cut it off at 0.4:

tab_jhb_1 <- tableGrob(round(dims_jhb[[1]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_jhb_1 <- grobHeight(tab_jhb_1)
w_jhb_1 <- grobWidth(tab_jhb_1)
title_jhb_1 <- textGrob("Dimension 1", y=unit(0.5,"npc") + 0.5*h_jhb_1, 
                        vjust=-8, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_jhb_1 <- gTree(children = gList(tab_jhb_1, title_jhb_1))
# grid.draw(gt_jhb_1) # check

# for dimension 2
tab_jhb_2 <- tableGrob(round(dims_jhb[[2]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_jhb_2 <- grobHeight(tab_jhb_2)
w_jhb_2 <- grobWidth(tab_jhb_2)
title_jhb_2 <- textGrob("Dimension 2", y=unit(0.5,"npc") + 0.5*h_jhb_2, 
                        vjust=-6, hjust = 0.2, gp=gpar(fontsize=14)) # title
gt_jhb_2 <- gTree(children = gList(tab_jhb_2, title_jhb_2))
# grid.draw(gt_jhb_2) # check

# arrange two Dimensions
ml_jhb_1n2 <- marrangeGrob(list(gt_jhb_1,gt_jhb_2), nrow=1, ncol=2, top = '\n\nJohannesburg')

# print to graphic
jpeg("dims02_jhb_1n2.jpeg")
ml_jhb_1n2
dev.off()

# CAPE TOWN 3 & 4:
# for dimension 3
tab_ct_3 <- tableGrob(round(dims_ct[[3]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_ct_3 <- grobHeight(tab_ct_3)
w_ct_3 <- grobWidth(tab_ct_3)
title_ct_3 <- textGrob("Dimension 3", y=unit(0.5,"npc") + 0.5*h_ct_3, 
                       vjust=-6, hjust = 0.2, gp=gpar(fontsize=14)) # title
gt_ct_3 <- gTree(children = gList(tab_ct_3, title_ct_3)) #,footnote
# grid.draw(gt_ct_3) # check

# for dimension 4
tab_ct_4 <- tableGrob(round(dims_ct[[4]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_ct_4 <- grobHeight(tab_ct_4)
w_ct_4 <- grobWidth(tab_ct_4)
title_ct_4 <- textGrob("Dimension 4", y=unit(0.5,"npc") + 0.5*h_ct_4, 
                       vjust=-5, hjust = 0.2, gp=gpar(fontsize=14)) # title
gt_ct_4 <- gTree(children = gList(tab_ct_4, title_ct_4)) #,footnote
# grid.draw(gt_ct_2) # check

# arrange two Dimensions on one plot and print to interactive graphic with latex
ml_ct_3n4 <- marrangeGrob(list(gt_ct_3,gt_ct_4), nrow=1, ncol=2, top = '\n\n\n\nCape Town')

# print to graphic
jpeg("dims02_ct_3n4.jpeg")
ml_ct_3n4
dev.off()

# JHB 3 & 4:

tab_jhb_3 <- tableGrob(round(dims_jhb[[3]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_jhb_3 <- grobHeight(tab_jhb_3)
w_jhb_3 <- grobWidth(tab_jhb_3)
title_jhb_3 <- textGrob("Dimension 3", y=unit(0.5,"npc") + 0.5*h_jhb_3, 
                        vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_jhb_3 <- gTree(children = gList(tab_jhb_3, title_jhb_3)) #,footnote
# grid.draw(gt_jhb_3) # check

# for dimension 4
tab_jhb_4 <- tableGrob(round(dims_jhb[[4]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_jhb_4 <- grobHeight(tab_jhb_4)
w_jhb_4 <- grobWidth(tab_jhb_4)
title_jhb_4 <- textGrob("Dimension 4", y=unit(0.5,"npc") + 0.5*h_jhb_4, 
                        vjust=-6.5, hjust = 0.2, gp=gpar(fontsize=14)) # title
gt_jhb_4 <- gTree(children = gList(tab_jhb_4, title_jhb_4)) #,footnote
# grid.draw(gt_jhb_4) # check

# arrange two Dimensions
ml_jhb_3n4 <- marrangeGrob(list(gt_jhb_3,gt_jhb_4), nrow=1, ncol=2, top = '\n\nJohannesburg')

# print to graphic
jpeg("dims02_jhb_3n4.jpeg")
ml_jhb_3n4
dev.off()



# CAPE TOWN 5 & 6 & 7:
# for dimension 5
tab_ct_5 <- tableGrob(round(dims_ct[[5]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_ct_5 <- grobHeight(tab_ct_5)
w_ct_5 <- grobWidth(tab_ct_5)
title_ct_5 <- textGrob("Dimension 5", y=unit(0.5,"npc") + 0.5*h_ct_5, 
                       vjust=-6, hjust = 0.2, gp=gpar(fontsize=14)) # title
gt_ct_5 <- gTree(children = gList(tab_ct_5, title_ct_5)) #,footnote
# grid.draw(gt_ct_5) # check

# for dimension 6
tab_ct_6 <- tableGrob(round(dims_ct[[6]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_ct_6 <- grobHeight(tab_ct_6)
w_ct_6 <- grobWidth(tab_ct_6)
title_ct_6 <- textGrob("Dimension 6", y=unit(0.5,"npc") + 0.5*h_ct_6, 
                       vjust=-5, hjust = 0.2, gp=gpar(fontsize=14)) # title
gt_ct_6 <- gTree(children = gList(tab_ct_6, title_ct_6)) #,footnote
# grid.draw(gt_ct_6) # check

# # for dimension 7
# tab_ct_7 <- tableGrob(round(dims_ct[[7]], 2), theme = ttheme_minimal(base_size = 10)) # table
# 
# grid.newpage()
# h_ct_7 <- grobHeight(tab_ct_7)
# w_ct_7 <- grobWidth(tab_ct_7)
# title_ct_7 <- textGrob("Dimension 7", y=unit(0.5,"npc") + 0.5*h_ct_7, 
#                        vjust=-5, hjust = 0.2, gp=gpar(fontsize=14)) # title
# gt_ct_7 <- gTree(children = gList(tab_ct_7, title_ct_7)) #,footnote
# # grid.draw(gt_ct_7) # check

# arrange two Dimensions on one plot and print to interactive graphic with latex
ml_ct_5n6 <- marrangeGrob(list(gt_ct_5,gt_ct_6), nrow=1, ncol=2, top = '\nCape Town')

# print to graphic
jpeg("dims02_ct_5n6.jpeg")
ml_ct_5n6
dev.off()

# JHB 5 and 6:
# for dimension 5

tab_jhb_5 <- tableGrob(round(dims_jhb[[5]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_jhb_5 <- grobHeight(tab_jhb_5)
w_jhb_5 <- grobWidth(tab_jhb_5)
title_jhb_5 <- textGrob("Dimension 5", y=unit(0.5,"npc") + 0.5*h_jhb_3, 
                        vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_jhb_5 <- gTree(children = gList(tab_jhb_5, title_jhb_5)) #,footnote
# grid.draw(gt_jhb_5) # check

# for dimension 6

tab_jhb_6 <- tableGrob(round(dims_jhb[[6]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_jhb_6 <- grobHeight(tab_jhb_6)
w_jhb_6 <- grobWidth(tab_jhb_6)
title_jhb_6 <- textGrob("Dimension 6", y=unit(0.5,"npc") + 0.5*h_jhb_3, 
                        vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_jhb_6 <- gTree(children = gList(tab_jhb_6, title_jhb_6)) #,footnote
# grid.draw(gt_jhb_6) # check

# arrange two Dimensions
ml_jhb_5n65 <- marrangeGrob(list(gt_jhb_5,gt_jhb_6), nrow=1, ncol=2,
                            top = '\n\n\n\nJohannesburg')

# print to graphic
jpeg("dims02_jhb_5n6.jpeg")
ml_jhb_5n65
dev.off()


## CAPE TOWN
# getting all the dimension descriptions
dimdesc_02_ct <- dimdesc(pca_02_ct, c(1:npc_ct), proba = 1)

# categorical supplementaries per dimension ... need to explain and interpret "Estimate"
cat_coord_02_ct <- list()
for(i in 1:npc_ct) {
        temp1 <- dimdesc_02_ct[[i]]$category[order(dimdesc_02_ct[[i]]$category[,1], decreasing = TRUE),]
        temp2 <- temp1[c(1:5, (nrow(temp1) - 4): nrow(temp1)),1]
        cat_coord_02_ct[[i]] <- data.frame(Est = round(temp2, 2))
        # write.table(cat_coord_02_ct[[i]], file = paste0("cat_dim", i, "_ct.csv")) # if needed
        
}

# getting tables 
# Dim 1 CT
tab_ct_cat1 <- tableGrob(cat_coord_02_ct[[1]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_ct1 <- grobHeight(tab_ct_cat1)
w_cat_ct1 <- grobWidth(tab_ct_cat1)
title_tab_cat_ct1 <- textGrob('Dimension 1', y=unit(0.5,"npc") + 0.5*h_cat_ct1, 
                              vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_ct1 <- gTree(children = gList(tab_ct_cat1, title_tab_cat_ct1))

# grid.draw(gt_cat_ct1) # check

# Dim 2 CT
tab_ct_cat2 <- tableGrob(cat_coord_02_ct[[2]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_ct2 <- grobHeight(tab_ct_cat2)
w_cat_ct2 <- grobWidth(tab_ct_cat2)
title_tab_cat_ct2 <- textGrob('Dimension 2', y=unit(0.5,"npc") + 0.5*h_cat_ct2, 
                              vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_ct2 <- gTree(children = gList(tab_ct_cat2, title_tab_cat_ct2))

# grid.draw(gt_cat_ct2) # check

# Dim 3 CT
tab_ct_cat3 <- tableGrob(cat_coord_02_ct[[3]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_ct3 <- grobHeight(tab_ct_cat3)
w_cat_ct3 <- grobWidth(tab_ct_cat3)
title_tab_cat_ct3 <- textGrob('Dimension 3', y=unit(0.5,"npc") + 0.5*h_cat_ct3, 
                              vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_ct3 <- gTree(children = gList(tab_ct_cat3, title_tab_cat_ct3))

# grid.draw(gt_cat_ct3) # check        

# Dim 4 CT
tab_ct_cat4 <- tableGrob(cat_coord_02_ct[[4]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_ct4 <- grobHeight(tab_ct_cat4)
w_cat_ct4 <- grobWidth(tab_ct_cat4)
title_tab_cat_ct4 <- textGrob('Dimension 4', y=unit(0.5,"npc") + 0.5*h_cat_ct4, 
                              vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_ct4 <- gTree(children = gList(tab_ct_cat4, title_tab_cat_ct4))

# grid.draw(gt_cat_ct4) # check

# Dim 5 CT
tab_ct_cat5 <- tableGrob(cat_coord_02_ct[[5]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_ct5 <- grobHeight(tab_ct_cat5)
w_cat_ct5 <- grobWidth(tab_ct_cat5)
title_tab_cat_ct5 <- textGrob('Dimension 5', y=unit(0.5,"npc") + 0.5*h_cat_ct5, 
                              vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_ct5 <- gTree(children = gList(tab_ct_cat5, title_tab_cat_ct5))

# grid.draw(gt_cat_ct5) # check  

# Dim 6 CT
tab_ct_cat6 <- tableGrob(cat_coord_02_ct[[6]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_ct6 <- grobHeight(tab_ct_cat6)
w_cat_ct6 <- grobWidth(tab_ct_cat6)
title_tab_cat_ct6 <- textGrob('Dimension 6', y=unit(0.5,"npc") + 0.5*h_cat_ct6, 
                              vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_ct6 <- gTree(children = gList(tab_ct_cat6, title_tab_cat_ct6))

# grid.draw(gt_cat_ct6) # check  

# # Dim 7 CT
# tab_ct_cat7 <- tableGrob(cat_coord_02_ct[[7]], theme = ttheme_minimal(base_size = 12)) # table
# 
# grid.newpage()
# h_cat_ct7 <- grobHeight(tab_ct_cat7)
# w_cat_ct7 <- grobWidth(tab_ct_cat7)
# title_tab_cat_ct7 <- textGrob('Dimension 7', y=unit(0.5,"npc") + 0.5*h_cat_ct7, 
#                               vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
# gt_cat_ct7 <- gTree(children = gList(tab_ct_cat7, title_tab_cat_ct7))
# 
# # grid.draw(gt_cat_ct7) # check

# creating two files for these 1st 4 and then 3 more
# arrange first four
ml_ct_1n2 <- marrangeGrob(list(gt_cat_ct1,
                               gt_cat_ct2), nrow=1, ncol=2,
                          top = '\n\n\n\nCape Town')

# print to graphic
jpeg("cats02_ct_1n2.jpeg")
ml_ct_1n2
dev.off()

ml_ct_3n4 <- marrangeGrob(list(gt_cat_ct3,
                               gt_cat_ct4), nrow=1, ncol=2,
                          top = '\n\n\n\nCape Town')

# print to graphic
jpeg("cats02_ct_3n4.jpeg")
ml_ct_3n4
dev.off()

ml_ct_5n6 <- marrangeGrob(list(gt_cat_ct5,
                               gt_cat_ct6), nrow=1, ncol=2,
                          top = '\n\n\n\nCape Town')

# print to graphic
jpeg("cats02_ct_5n6.jpeg")
ml_ct_5n6
dev.off()

# ml_ct_7 <- marrangeGrob(list(gt_cat_ct7), nrow=1, ncol=1,
#                           top = '\n\n\n\nCape Town')
# 
# # print to graphic
# jpeg("cats02_ct_7.jpeg")
# ml_ct_7
# dev.off()

## JOHANNESBURG
# getting all the dimension descriptions
dimdesc_02_jhb <- dimdesc(pca_02_jhb, c(1:npc_jhb), proba = 1)

# categorical supplementaries per dimension ... need to explain and interpret "Estimate"
cat_coord_02_jhb <- list()
for(i in 1:npc_jhb) {
        temp1 <- dimdesc_02_jhb[[i]]$category[order(dimdesc_02_jhb[[i]]$category[,1], decreasing = TRUE),]
        temp2 <- temp1[c(1:5, (nrow(temp1) - 4): nrow(temp1)),1]
        cat_coord_02_jhb[[i]] <- data.frame(Est = round(temp2, 2))
        # write.table(cat_coord_02_ct[[i]], file = paste0("cat_dim", i, "_ct.csv")) # if needed
        
}

# getting tables 
# Dim 1 JHB
tab_jhb_cat1 <- tableGrob(cat_coord_02_jhb[[1]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_jhb1 <- grobHeight(tab_jhb_cat1)
w_cat_jhb1 <- grobWidth(tab_jhb_cat1)
title_tab_cat_jhb1 <- textGrob('Dimension 1', y=unit(0.5,"npc") + 0.5*h_cat_jhb1, 
                               vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_jhb1 <- gTree(children = gList(tab_jhb_cat1, title_tab_cat_jhb1))

grid.draw(gt_cat_jhb1) # check

# Dim 2 jhb
tab_jhb_cat2 <- tableGrob(cat_coord_02_jhb[[2]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_jhb2 <- grobHeight(tab_jhb_cat2)
w_cat_jhb2 <- grobWidth(tab_jhb_cat2)
title_tab_cat_jhb2 <- textGrob('Dimension 2', y=unit(0.5,"npc") + 0.5*h_cat_jhb2, 
                               vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_jhb2 <- gTree(children = gList(tab_jhb_cat2, title_tab_cat_jhb2))

# grid.draw(gt_cat_jhb2) # check

# Dim 3 jhb
tab_jhb_cat3 <- tableGrob(cat_coord_02_jhb[[3]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_jhb3 <- grobHeight(tab_jhb_cat3)
w_cat_jhb3 <- grobWidth(tab_jhb_cat3)
title_tab_cat_jhb3 <- textGrob('Dimension 3', y=unit(0.5,"npc") + 0.5*h_cat_jhb3, 
                               vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_jhb3 <- gTree(children = gList(tab_jhb_cat3, title_tab_cat_jhb3))

# grid.draw(gt_cat_jhb3) # check        

# Dim 4 jhb
tab_jhb_cat4 <- tableGrob(cat_coord_02_jhb[[4]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_jhb4 <- grobHeight(tab_jhb_cat4)
w_cat_jhb4 <- grobWidth(tab_jhb_cat4)
title_tab_cat_jhb4 <- textGrob('Dimension 4', y=unit(0.5,"npc") + 0.5*h_cat_jhb4, 
                               vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_jhb4 <- gTree(children = gList(tab_jhb_cat4, title_tab_cat_jhb4))

# grid.draw(gt_cat_jhb4) # check

# Dim 5 jhb
tab_jhb_cat5 <- tableGrob(cat_coord_02_jhb[[5]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_jhb5 <- grobHeight(tab_jhb_cat5)
w_cat_jhb5 <- grobWidth(tab_jhb_cat5)
title_tab_cat_jhb5 <- textGrob('Dimension 5', y=unit(0.5,"npc") + 0.5*h_cat_jhb5, 
                               vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_jhb5 <- gTree(children = gList(tab_jhb_cat5, title_tab_cat_jhb5))

# grid.draw(gt_cat_jhb5) # check  

# Dim 6 jhb
tab_jhb_cat6 <- tableGrob(cat_coord_02_jhb[[6]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_jhb6 <- grobHeight(tab_jhb_cat6)
w_cat_jhb6 <- grobWidth(tab_jhb_cat6)
title_tab_cat_jhb6 <- textGrob('Dimension 6', y=unit(0.5,"npc") + 0.5*h_cat_jhb6, 
                               vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_jhb6 <- gTree(children = gList(tab_jhb_cat6, title_tab_cat_jhb6))

# grid.draw(gt_cat_jhb6) # check  


# creating two files for these 1st 4 and then 3 more
# arrange first four
ml_jhb_1n2 <- marrangeGrob(list(gt_cat_jhb1,
                                gt_cat_jhb2), nrow=1, ncol=2,
                           top = '\n\n\n\nJohannesburg')

# print to graphic
jpeg("cats02_jhb_1n2.jpeg")
ml_jhb_1n2
dev.off()

ml_jhb_3n4 <- marrangeGrob(list(gt_cat_jhb3,
                                gt_cat_jhb4), nrow=1, ncol=2,
                           top = '\n\n\n\nJohannesburg')

# print to graphic
jpeg("cats02_jhb_3n4.jpeg")
ml_jhb_3n4
dev.off()

ml_jhb_5n6 <- marrangeGrob(list(gt_cat_jhb5,
                                gt_cat_jhb6), nrow=1, ncol=2,
                           top = '\n\n\n\nJohannesburg')

# print to graphic
jpeg("cats02_jhb_5n6.jpeg")
ml_jhb_5n6
dev.off()



# continuous supplementaries per dimension...explain difference... blah blah
# cape town:

cont_corrs_02_ct <- matrix(0, nrow = 4, ncol = 0)
for(i in 1:npc_ct) {
        temp1 <- as.matrix(dimdesc_02_ct[[i]]$quanti)
        temp2 <- temp1[which(rownames(temp1) %in% c("age", "edu", "hh_inc", "lsm")),]
        cont_corrs_02_ct <- as.data.frame(round(cbind(cont_corrs_02_ct, temp2), 2))
}
names(cont_corrs_02_ct) <- c("Dim1", "pVal","Dim2", "pVal", "Dim3", "pVal", "Dim4", "pVal", "Dim5", "pVal", "Dim6", "pVal" )

# print to file for graphic:

tab_ct_cont <- tableGrob(cont_corrs_02_ct, theme = ttheme_minimal(base_size = 8)) # table

grid.newpage()
h_cont_ct <- grobHeight(tab_ct_cont)
w_cont_ct <- grobWidth(tab_ct_cont)
title_tab_cont_ct <- textGrob('Cape Town', y=unit(0.5,"npc") + 0.3*h_cont_ct, 
                              vjust=-4, hjust = 0.5, gp=gpar(fontsize=14)) # title
gt_cont_ct<- gTree(children = gList(tab_ct_cont, title_tab_cont_ct)) #
# grid.draw(gt_cont_ct) # check

# Johannesburg:
dimdesc_02_jhb <- dimdesc(pca_02_jhb, c(1:npc_jhb), proba = 1)
cont_corrs_02_jhb <- matrix(0, nrow = 4, ncol = 0)
for(i in 1:npc_jhb) {
        temp1 <- as.matrix(dimdesc_02_jhb[[i]]$quanti)
        temp2 <- temp1[which(rownames(temp1) %in% c("age", "edu", "hh_inc", "lsm")),]
        cont_corrs_02_jhb <- as.data.frame(round(cbind(cont_corrs_02_jhb, temp2), 2))
}
names(cont_corrs_02_jhb) <- c("Dim1", "pVal","Dim2", "pVal", "Dim3", "pVal", "Dim4", "pVal", "Dim5", "pVal", "Dim6", "pVal" )

# print to file for graphic:
tab_jhb_cont <- tableGrob(cont_corrs_02_jhb, theme = ttheme_minimal(base_size = 8)) # table

grid.newpage()
h_cont_jhb <- grobHeight(tab_jhb_cont)
w_cont_jhb <- grobWidth(tab_jhb_cont)
title_tab_cont_jhb <- textGrob('Johannesburg', y=unit(0.5,"npc") + 0.5*h_cont_jhb, 
                               vjust=-4, hjust = 0.5, gp=gpar(fontsize=14)) # title
gt_cont_jhb  <- gTree(children = gList(tab_jhb_cont, title_tab_cont_jhb))
# grid.draw(gt_cont_jhb) # check

# arrange single output
ml_cont_ct_jhb <- marrangeGrob(list(gt_cont_ct,gt_cont_jhb), nrow=2, ncol=1,
                               top = '')

# print to graphic
jpeg("cont_02_ct_jhb.jpeg")
ml_cont_ct_jhb
dev.off()

