# # loading packages
library(tidyverse)
library(corrplot)
library(rpart)
library(rpart.plot)
# library(scatterplot3d)
# library(rgl)
library(caret)
library(randomForest)
library(MASS)
library(gridExtra)
library(ggplot2)

#  read in datasets
set02_simple <- readRDS("set02_simple.rds")

# consider some correlations
jpeg('corTypePlot2002_simple.jpeg')
corrplot(cor(set02_simple[,c("newspapers","magazines","radio", "tv", "internet")]),
         method = "pie",
         order = "hclust",
         hclust.method = "complete",
         tl.col = 'black',
         mar = c(1,1,1,1),
         addCoefasPercent = TRUE,
         tl.pos = TRUE)
dev.off()

## consider kmeans
wss_simple <- vector()
set.seed(123)
for(k in c(1,2,3,4,5,6)) {
        temp <- kmeans(set02_simple[,c("newspapers","magazines","radio", "tv", "internet", "all")],
                       centers = k,
                       nstart = 5,
                       iter.max = 30)
        wss_simple <- append(wss_simple,temp$tot.withinss)
}

jpeg('kmeansTypePlot2002_simple.jpeg')
plot(c(1,2,3,4,5,6), wss_simple, type = "b", xlab = "k-values", ylab = "total within sum of squares", main = "_simple" )
dev.off()

set.seed(123)
kmeans02_simple <- kmeans(set02_simple[,c("newspapers","magazines","radio", "tv", "internet", "all")],
                          centers = 4,
                          nstart = 5,
                          iter.max = 100)
table(kmeans02_simple$cluster)

# align with interpretation of 2012 basic (ie, not simple)....
# green to lilac:  2 to 4
# lilac to green: 4 to 2
# blue to blue: 3 to 3
# red to red: 1 to 1
kmeans02_simple$cluster <- ifelse(kmeans02_simple$cluster == 1, 6, kmeans02_simple$cluster)
kmeans02_simple$cluster <- ifelse(kmeans02_simple$cluster == 2, 9, kmeans02_simple$cluster)
kmeans02_simple$cluster <- ifelse(kmeans02_simple$cluster == 3, 8, kmeans02_simple$cluster)
kmeans02_simple$cluster <- ifelse(kmeans02_simple$cluster == 4, 7, kmeans02_simple$cluster)
kmeans02_simple$cluster <- kmeans02_simple$cluster - 5


# add cluster labels to the dataset
set02c_simple <- set02_simple %>%
        mutate(cluster = factor(kmeans02_simple$cluster)) %>%
        dplyr::select(qn, pwgt, cluster, everything())

# save
saveRDS(set02c_simple, "set02c_simple.rds")
# read back
set02c_simple <- readRDS("set02c_simple.rds")

## some plots for simple version to use in longitudinal stuff later...
# boxplots of clusters and media types
boxplot <- function(set,type) {
        ggplot(set, aes_string("cluster", type, fill = "cluster")) +
                geom_boxplot() +
                guides(fill = FALSE) +
                labs(title = type)
}

jpeg('typeBoxPlots_02_simple.jpeg', quality = 100, type = "cairo")
grid.arrange(boxplot(set02c_simple, type = "all"),
             boxplot(set02c_simple, type = "newspapers"),
             boxplot(set02c_simple, type = "magazines"),
             boxplot(set02c_simple, type = "radio"),
             boxplot(set02c_simple, type = "tv"),
             boxplot(set02c_simple, type = "internet"),
             ncol=3, nrow = 2)
dev.off()


# try to make sense of demographics

# size of each cluster
ggplot(data = set02c_simple, aes(x = cluster, fill = cluster)) +
        geom_bar(stat = "count") +
        guides(fill = FALSE)

# demographics by cluster
bars_by_cluster <- function(set, category) { # category:one of race, edu, age, lsm, sex, hh_inc
        if(category == "race") {
                level = c("black", "coloured", "indian", "white")
                title = "Population Group 2002"
        }
        if(category == "edu") {
                level = c(c("<matric", "matric",">matric"))
                title = "Education Level 2002"
        }
        if(category == "age") {
                level = c(c("15-24","25-44", "45-54","55+"))
                title = "Age Group 2002"
        }
        if(category == "lsm") {
                level = c("1-2", "3-4", "5-6", "7-8", "9-10")
                title = "LSM 2002"
        }
        if(category == "sex") {
                level = c("male", "female")
                title = "Gender 2002"
        }
        if(category == "hh_inc") {
                level = c("<5000","5000-10999","11000-19999",">=20000")
                title = "Household Income 2002"
        }
        
        ggplot(data = set, aes_string(x = "cluster", fill = category)) +
                geom_bar(stat = "count", position = position_dodge()) +
                scale_fill_discrete(labels=level) +
                labs(title = title) +
                guides(fill=guide_legend(title=NULL)) 
}

jpeg('typeDemogPlots_02_simple.jpeg', quality = 100, type = "cairo")
grid.arrange(bars_by_cluster(set02c_simple, "sex"),
             bars_by_cluster(set02c_simple, "age"),
             bars_by_cluster(set02c_simple, "race"),
             bars_by_cluster(set02c_simple, "edu"),
             bars_by_cluster(set02c_simple, "hh_inc"),
             bars_by_cluster(set02c_simple, "lsm"),
             ncol=2, nrow = 3)
dev.off()


# consider multidimensional scaling and self organising maps on the clusters :

# 1st create a subset to ensure easier running
set.seed(56)
sub02_simple <- set02c_simple[sample(nrow(set02c_simple), size = 1000),]

# distance matrix and MDS
sub02_simple_dist <- dist(sub02_simple[,c("newspapers","magazines","radio", "tv", "internet", "all")])
mds02_simple <- cmdscale(sub02_simple_dist)
plot(mds02_simple, col = as.numeric(sub02_simple$cluster) + 1, pch = 19, ylab = "", xlab = "")

# 3D scaling
mds3_simple <- cmdscale(dist(sub02_simple[,c("newspapers", "magazines", "radio", "tv", "internet", "all")]), k = 3)
mds3_simple <- as.data.frame(mds3_simple)

# 2D Scatterplots of 4 cente

# setting colours
cols <- as.numeric(sub02_simple$cluster) + 1
cols <- ifelse(cols == 5, 6, cols)

jpeg('kmeans2DPlot2002.jpeg')
plot(mds02_simple, col = cols, ylab = "", xlab = "", pch = 19)
dev.off()
# 

# consider for some predictions:
# create training and test sets:

set.seed(56)
ind_train_simple <- createDataPartition(set02c_simple$cluster, p = 0.7, list = FALSE)
training_simple <- set02c_simple[ind_train_simple,]
testing_simple <- set02c_simple[-ind_train_simple,]

# # using random forest:
forest02_type_simple <- randomForest(cluster ~ newspapers
                                     + tv
                                     + radio
                                     + magazines
                                     + internet,
                                     data = training_simple )

pred_forest02_type_simple <- predict(forest02_type_simple, newdata = testing_simple)

confusionMatrix(pred_forest02_type_simple, testing_simple$cluster) 

# with lda. Although given accuracy of forest,  no real need.
set.seed(56)
lda02_simple <- lda(cluster ~ newspapers
                    + tv
                    + radio
                    + magazines
                    + internet,
                    data = training_simple)
summary(lda02_simple)

pred_lda02_simple <- predict(lda02_simple, newdata = testing_simple)
confusionMatrix(pred_lda02_simple$class, testing_simple$cluster) # collinearity meant took out 

# using only demographic information
forest02_demogr_simple <- randomForest(cluster ~ age
                                       + sex
                                       + edu
                                       + hh_inc
                                       + race
                                       + lsm,
                                       data = training_simple)

pred_forest02_demogr_simple <- predict(forest02_demogr_simple, newdata = testing_simple)

confusionMatrix(pred_forest02_demogr_simple, testing_simple$cluster)

# with lda
set.seed(56)
lda02_demogr_simple <- lda(cluster ~ age
                           + sex
                           + edu
                           + hh_inc
                           + race
                           + lsm,
                           data = training_simple)

pred_lda02_demogr_simple <- predict(lda02_demogr_simple, newdata = testing_simple)
confusionMatrix(pred_lda02_demogr_simple$class, testing_simple$cluster)

##  some qualitative consideration of the four types:

# consider a single tree partitioning to try to add meaning to the four clusters
control_simple <- rpart.control(maxdepth = 3, cp = 0.001)
tree02_simple <- rpart(cluster ~ newspapers + tv + radio + magazines + internet, 
                       data = set02c_simple,
                       control = control_simple) # weights = pwgt
par(mfrow = c(1,1))
plot(tree02_simple, uniform = TRUE, margin = 0.2)
text(tree02_simple, pretty = 0, cex = 0.8)

# for more detail
rpart.plot(tree02_simple, type = 4, extra = 1, cex = 0.5)