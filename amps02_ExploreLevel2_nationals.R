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
set02_nat <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set02_nat.rds")

# LEVEL 2

# # get rid of near zero variances:
# ind_ct <- nearZeroVar(set02_CT[,21:ncol(set02_CT)], saveMetrics = TRUE)
# ind_jhb <- nearZeroVar(set02_JHB[,21:ncol(set02_JHB)], saveMetrics = TRUE)
# 
# good_ct <- set02_CT[,21:ncol(set02_CT)][,!ind_ct$zeroVar]
# good_jhb <- set02_JHB[,21:ncol(set02_JHB)][,!ind_jhb$nzv]
# 
# catSet02_CT <- data.frame(cbind(set02_CT[,1:20], good_ct))
# catSet02_JHB <- data.frame(cbind(set02_JHB[,1:20], good_jhb))
# 
# saveRDS(catSet02_CT, "catSet02_CT.rds")
# saveRDS(catSet02_JHB, "catSet02_JHB.rds")
# 
# nuSet02_CT <- data.frame(cbind(set02_CT[,1:20], good_ct))
# nuSet02_JHB <- data.frame(cbind(set02_JHB[,1:20], good_jhb))

#setting the ordered variables as scaled numerical:
set02_nat$age <- scale(as.numeric(set02_nat$age))
set02_nat$edu <- scale(as.numeric(set02_nat$edu))
set02_nat$hh_inc <- scale(as.numeric(set02_nat$hh_inc))
set02_nat$lsm <- scale(as.numeric(set02_nat$lsm))

# naming the factors

# National
# nuSet02_CT$cluster <- factor(nuSet02_CT$cluster,
#                              levels = c(1,2,3,4),
#                              labels = c("cluster1", "cluster2", "cluster3", "cluster4"))
set02_nat$sex <- factor(set02_nat$sex,
                         levels = c(1,2),
                         labels = c("male", "female"))
set02_nat$race <- factor(set02_nat$race,
                          levels = c(1,2,3,4),
                          labels = c("black", "coloured", "indian", "white"))
set02_nat$lifestages <- factor(set02_nat$lifestages,
                                levels = c(1,2,3,4,5,6,7,8),
                                labels = c("at home singles", "young independent singles", "mature singles", "young couples", "mature couples", "young family", "single parent family", "mature family"))
set02_nat$mar_status <- factor(set02_nat$mar_status,
                                levels = c(1,2,3,4,5),
                                labels = c("single", "married or living together", "widowed", "divorced", "separated"))
# set02_nat$lifestyle <- factor(set02_nat$lifestyle,
#                                levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
#                                labels = c("none", "cell sophisticates", "sports", "gamers", "outdoors", "good living", "avid readers", "traditionalists","cell fundamentals", "homebodies", "studious", "showgoers"))
# set02_nat$attitudes <- factor(set02_nat$attitudes,
#                                levels = c(1,2,3,4,5,6,7),
#                                labels = c("none", "now generation", "nation builders", "distants survivors", "distants established", "rooted", "global citizens"))

# focussing only on the variable I intend to use in this section:
set02_nat <- set02_nat[,-c(1:2,8:12,14:19)]

# # saving these objects:
# saveRDS(set02_nat, "set02_nat.rds")
# 
# set02_nat <- readRDS("set02_nat.rds")

## Determine Number of Factors to Extract
ev <- eigen(cor(set02_nat[,7:ncol(set02_nat)]))
ap <- parallel(subject=nrow(set02_nat[,7:ncol(set02_nat)]),var=ncol(set02_nat[,7:ncol(set02_nat)]),
               rep=100,cent=.02)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
jpeg("nScree_02_nat")
plotnScree(nS, main = "National") # optimal = 6
dev.off()

# will set them at six for both Jhb and CT for now
npc <- 6

# creating objects with supplementary variables (qualitative and quantitative) and active one defined:
set.seed(56)
pca_02_nat <- PCA(set02_nat,
                 quanti.sup = c(1,3,4,6),
                 quali.sup = c(2,5),
                 ncp = npc,
                 graph = FALSE)

# # try FactoInvestigate
# library(FactoInvestigate)
# Investigate(pca_02_nat, "factoInvestigate_02_nat.Rmd", c("pdf_document"))


# contributions plots
jpeg("contributions02_nat_1n2.jpeg")
fviz_pca_var(pca_02_nat,
             axes = c(1,2),
             col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "National",
             repel = TRUE # Avoid text overlapping
)
dev.off()

jpeg("contributions02_nat_3n4.jpeg")
fviz_pca_var(pca_02_nat,
             axes = c(3,4),
             col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "National",
             repel = TRUE # Avoid text overlapping
)
dev.off()

jpeg("contributions02_nat_5n6.jpeg")
fviz_pca_var(pca_02_nat,
             axes = c(5,6),
             col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "National",
             repel = TRUE # Avoid text overlapping
)
dev.off()

# can identify most important
# # high absolute values describe variables that best describe a particular dimension.
# and a negatively correlated value (eg Kickoff) means that individuals who have high coordinates on the given dimension would have a low value for engagement with kickoff... (see FactoMineR document p 8)

# create dataframe based on correlations > plus or minus 0.3
# create dataframe of values:

# for National
vehicle_nat <- rownames(pca_02_nat$var$cor)
corr_nat <- as.data.frame(pca_02_nat$var$cor)
contrib_nat <- as.data.frame(pca_02_nat$var$contrib)
cos2_nat <- as.data.frame(pca_02_nat$var$cos2)

dims_nat <- list()
for(i in 1:npc) {
        temp <- data.frame(vehicle_nat, corr = corr_nat[,i], contrib = contrib_nat[,i], cos2 = cos2_nat[,i]) %>%
                filter(corr > 0.3 | corr < -0.3) %>%
                arrange(desc(corr))
        rownames(temp) <- temp$vehicle
        dims_nat[[i]] <- temp[,-1]
}

# Dimension Tables Information National 1 & 2:
# for dimension 1
tab_nat_1 <- tableGrob(round(dims_nat[[1]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_nat_1 <- grobHeight(tab_nat_1)
w_nat_1 <- grobWidth(tab_nat_1)
title_nat_1 <- textGrob("Dimension 1", y=unit(0.5,"npc") + 0.5*h_nat_1, 
                       vjust=-6, hjust = 0.2, gp=gpar(fontsize=14)) # title
gt_nat_1 <- gTree(children = gList(tab_nat_1, title_nat_1)) #,footnote
# grid.draw(gt_nat_1) # check

# for dimension 2
tab_nat_2 <- tableGrob(round(dims_nat[[2]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_nat_2 <- grobHeight(tab_nat_1)
w_nat_2 <- grobWidth(tab_nat_1)
title_nat_2 <- textGrob("Dimension 2", y=unit(0.5,"npc") + 0.5*h_nat_2, 
                       vjust=-6, hjust = 0.2, gp=gpar(fontsize=14)) # title
gt_nat_2 <- gTree(children = gList(tab_nat_2, title_nat_2)) #,footnote
# grid.draw(gt_nat_2) # check

# arrange two Dimensions on one plot and print to interactive graphic with latex
ml_nat_1n2 <- marrangeGrob(list(gt_nat_1,gt_nat_2), nrow=1, ncol=2, top = '\n\n\n\nNational')

# print to graphic
jpeg("dims02_nat_1n2.jpeg")
ml_nat_1n2
dev.off()



# National 3 & 4:
# for dimension 3
tab_nat_3 <- tableGrob(round(dims_nat[[3]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_nat_3 <- grobHeight(tab_nat_3)
w_nat_3 <- grobWidth(tab_nat_3)
title_nat_3 <- textGrob("Dimension 3", y=unit(0.5,"npc") + 0.5*h_nat_3, 
                       vjust=-6, hjust = 0.2, gp=gpar(fontsize=14)) # title
gt_nat_3 <- gTree(children = gList(tab_nat_3, title_nat_3)) #,footnote
# grid.draw(gt_nat_3) # check

# for dimension 4
tab_nat_4 <- tableGrob(round(dims_nat[[4]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_nat_4 <- grobHeight(tab_nat_4)
w_nat_4 <- grobWidth(tab_nat_4)
title_nat_4 <- textGrob("Dimension 4", y=unit(0.5,"npc") + 0.5*h_nat_4, 
                       vjust=-5, hjust = 0.2, gp=gpar(fontsize=14)) # title
gt_nat_4 <- gTree(children = gList(tab_nat_4, title_nat_4)) #,footnote
# grid.draw(gt_nat_2) # check

# arrange two Dimensions on one plot and print to interactive graphic with latex
ml_nat_3n4 <- marrangeGrob(list(gt_nat_3,gt_nat_4), nrow=1, ncol=2, top = '\n\n\n\nNational')

# print to graphic
jpeg("dims02_nat_3n4.jpeg")
ml_nat_3n4
dev.off()



# National 5 & 6 & 7:
# for dimension 5
tab_nat_5 <- tableGrob(round(dims_nat[[5]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_nat_5 <- grobHeight(tab_nat_5)
w_nat_5 <- grobWidth(tab_nat_5)
title_nat_5 <- textGrob("Dimension 5", y=unit(0.5,"npc") + 0.5*h_nat_5, 
                       vjust=-6, hjust = 0.2, gp=gpar(fontsize=14)) # title
gt_nat_5 <- gTree(children = gList(tab_nat_5, title_nat_5)) #,footnote
# grid.draw(gt_nat_5) # check

# for dimension 6
tab_nat_6 <- tableGrob(round(dims_nat[[6]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_nat_6 <- grobHeight(tab_nat_6)
w_nat_6 <- grobWidth(tab_nat_6)
title_nat_6 <- textGrob("Dimension 6", y=unit(0.5,"npc") + 0.5*h_nat_6, 
                       vjust=-5, hjust = 0.2, gp=gpar(fontsize=14)) # title
gt_nat_6 <- gTree(children = gList(tab_nat_6, title_nat_6)) #,footnote
# grid.draw(gt_nat_6) # check


# arrange two Dimensions on one plot and print to interactive graphic with latex
ml_nat_5n6 <- marrangeGrob(list(gt_nat_5,gt_nat_6), nrow=1, ncol=2, top = '\nNational')

# print to graphic
jpeg("dims02_nat_5n6.jpeg")
ml_nat_5n6
dev.off()


## Nationals
# getting all the dimension descriptions
dimdesc_02_nat <- dimdesc(pca_02_nat, c(1:npc), proba = 1)

# categorical supplementaries per dimension ... need to explain and interpret "Estimate"
cat_coord_02_nat <- list()
for(i in 1:npc) {
        temp1 <- dimdesc_02_nat[[i]]$category[order(dimdesc_02_nat[[i]]$category[,1], decreasing = TRUE),]
        temp2 <- temp1[c(1:5, (nrow(temp1) - 4): nrow(temp1)),1]
        cat_coord_02_nat[[i]] <- data.frame(Est = round(temp2, 2))
        # write.table(cat_coord_02_nat[[i]], file = paste0("cat_dim", i, "_nat.csv")) # if needed
        
}

# getting tables 
# Dim 1 CT
tab_nat_cat1 <- tableGrob(cat_coord_02_nat[[1]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_nat1 <- grobHeight(tab_nat_cat1)
w_cat_nat1 <- grobWidth(tab_nat_cat1)
title_tab_cat_nat1 <- textGrob('Dimension 1', y=unit(0.5,"npc") + 0.5*h_cat_nat1, 
                              vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_nat1 <- gTree(children = gList(tab_nat_cat1, title_tab_cat_nat1))

# grid.draw(gt_cat_nat1) # check

# Dim 2 CT
tab_nat_cat2 <- tableGrob(cat_coord_02_nat[[2]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_nat2 <- grobHeight(tab_nat_cat2)
w_cat_nat2 <- grobWidth(tab_nat_cat2)
title_tab_cat_nat2 <- textGrob('Dimension 2', y=unit(0.5,"npc") + 0.5*h_cat_nat2, 
                              vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_nat2 <- gTree(children = gList(tab_nat_cat2, title_tab_cat_nat2))

# grid.draw(gt_cat_nat2) # check

# Dim 3 CT
tab_nat_cat3 <- tableGrob(cat_coord_02_nat[[3]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_nat3 <- grobHeight(tab_nat_cat3)
w_cat_nat3 <- grobWidth(tab_nat_cat3)
title_tab_cat_nat3 <- textGrob('Dimension 3', y=unit(0.5,"npc") + 0.5*h_cat_nat3, 
                              vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_nat3 <- gTree(children = gList(tab_nat_cat3, title_tab_cat_nat3))

# grid.draw(gt_cat_nat3) # check        

# Dim 4 CT
tab_nat_cat4 <- tableGrob(cat_coord_02_nat[[4]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_nat4 <- grobHeight(tab_nat_cat4)
w_cat_nat4 <- grobWidth(tab_nat_cat4)
title_tab_cat_nat4 <- textGrob('Dimension 4', y=unit(0.5,"npc") + 0.5*h_cat_nat4, 
                              vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_nat4 <- gTree(children = gList(tab_nat_cat4, title_tab_cat_nat4))

# grid.draw(gt_cat_nat4) # check

# Dim 5 CT
tab_nat_cat5 <- tableGrob(cat_coord_02_nat[[5]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_nat5 <- grobHeight(tab_nat_cat5)
w_cat_nat5 <- grobWidth(tab_nat_cat5)
title_tab_cat_nat5 <- textGrob('Dimension 5', y=unit(0.5,"npc") + 0.5*h_cat_nat5, 
                              vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_nat5 <- gTree(children = gList(tab_nat_cat5, title_tab_cat_nat5))

# grid.draw(gt_cat_nat5) # check  

# Dim 6 CT
tab_nat_cat6 <- tableGrob(cat_coord_02_nat[[6]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_nat6 <- grobHeight(tab_nat_cat6)
w_cat_nat6 <- grobWidth(tab_nat_cat6)
title_tab_cat_nat6 <- textGrob('Dimension 6', y=unit(0.5,"npc") + 0.5*h_cat_nat6, 
                              vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_nat6 <- gTree(children = gList(tab_nat_cat6, title_tab_cat_nat6))

# grid.draw(gt_cat_nat6) # check  



# creating two files for these 1st 4 and then 3 more
# arrange first four
ml_nat_1n2 <- marrangeGrob(list(gt_cat_nat1,
                               gt_cat_nat2), nrow=1, ncol=2,
                          top = '\n\n\n\nNational')

# print to graphic
jpeg("cats02_nat_1n2.jpeg")
ml_nat_1n2
dev.off()

ml_nat_3n4 <- marrangeGrob(list(gt_cat_nat3,
                               gt_cat_nat4), nrow=1, ncol=2,
                          top = '\n\n\n\nNational')

# print to graphic
jpeg("cats02_nat_3n4.jpeg")
ml_nat_3n4
dev.off()

ml_nat_5n6 <- marrangeGrob(list(gt_cat_nat5,
                               gt_cat_nat6), nrow=1, ncol=2,
                          top = '\n\n\n\nNational')

# print to graphic
jpeg("cats02_nat_5n6.jpeg")
ml_nat_5n6
dev.off()

# continuous supplementaries per dimension...explain difference... blah blah
# National:

cont_corrs_02_nat <- matrix(0, nrow = 4, ncol = 0)
for(i in 1:npc) {
        temp1 <- as.matrix(dimdesc_02_nat[[i]]$quanti)
        temp2 <- temp1[which(rownames(temp1) %in% c("age", "edu", "hh_inc", "lsm")),]
        cont_corrs_02_nat <- as.data.frame(round(cbind(cont_corrs_02_nat, temp2), 2))
}
names(cont_corrs_02_nat) <- c("Dim1", "pVal","Dim2", "pVal", "Dim3", "pVal", "Dim4", "pVal", "Dim5", "pVal", "Dim6", "pVal" )

# print to file for graphic:

tab_nat_cont <- tableGrob(cont_corrs_02_nat, theme = ttheme_minimal(base_size = 8)) # table

grid.newpage()
h_cont_nat <- grobHeight(tab_nat_cont)
w_cont_nat <- grobWidth(tab_nat_cont)
title_tab_cont_nat <- textGrob('National', y=unit(0.5,"npc") + 0.3*h_cont_nat, 
                              vjust=-4, hjust = 0.5, gp=gpar(fontsize=14)) # title
gt_cont_nat<- gTree(children = gList(tab_nat_cont, title_tab_cont_nat)) #
# grid.draw(gt_cont_nat) # check
