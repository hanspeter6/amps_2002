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
# 

# read datafiles
magazines_engagement_02 <- readRDS("magazines_engagement_02.rds")
newspapers_engagement_02 <- readRDS("newspapers_engagement_02.rds")
radio_engagement_02 <- readRDS("radio_engagement_02.rds")
tv_engagement_02 <- readRDS("tv_engagement_02.rds")
internet_engagement_02 <- readRDS("internet_engagement_02.rds")

media_type_02 <- readRDS("media_type_02.rds")
media_type_02_simple <- readRDS("media_type_02_simple.rds")
media_vehicles_02 <- readRDS("media_vehicles_02.rds")

demographics_02 <- readRDS("demographics_02.rds")

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

# #create single dataset minus non metropolitans
set02 <- demographics_02 %>%
        left_join(media_type_02) %>%
        left_join(media_vehicles_02) %>%
        filter(metro != 0)
set02_simple <- demographics_02 %>%
        left_join(media_type_02_simple) %>%
        left_join(media_vehicles_02) %>%
        filter(metro != 0)

# consider some correlations

png('corTypePlot2002.png')
corrplot(cor(set02[,c("newspapers","magazines","radio", "tv", "internet")]),
         method = "pie",
         order = "hclust",
         hclust.method = "complete",
         tl.col = 'black',
         mar = c(1,1,1,1),
         addCoefasPercent = TRUE,
         tl.pos = TRUE)
dev.off()

# # consider some clustering
# # construct distance matrix for newspapers, magazines, radio, tv and internet engagement:
# 
# dist02 <- dist(set02[,c("newspapers","magazines","radio", "tv", "internet")])
# clust02 <- hclust(dist02, method = "complete")
# plot(clust02) # messy, unhelpful

## consider kmeans
wss <- vector()
for(k in c(3,4,5,6,7,8,9,02,11,02)) {
        temp <- kmeans(set02[,c("newspapers","magazines","radio", "tv", "internet")],
                       centers = k,
                       nstart = 3,
                       iter.max = 20)
        wss <- append(wss,temp$tot.withinss)
}

png('kmeansTypePlot2002.png')
plot(c(3,4,5,6,7,8,9,02,11,02), wss, type = "b", xlab = "k-values", ylab = "total within sum of squares" )
dev.off()

set.seed(12)
kmeans02 <- kmeans(set02[,c("newspapers","magazines","radio", "tv", "internet")],
                   centers = 5,
                   nstart = 20)
set.seed(12)
kmeans02_simple <- kmeans(set02_simple[,c("newspapers","magazines","radio", "tv", "internet")],
                   centers = 5,
                   nstart = 20)


# add cluster labels to the dataset
set02 <- set02 %>%
        mutate(cluster = factor(kmeans02$cluster))
set02_simple <- set02_simple %>%
        mutate(cluster = factor(kmeans02_simple$cluster))

# trying out idea of first pc scores as measure of media type mix...kinda engagement...think about this

pc_type <- princomp(set02[,c('newspapers', 'magazines', 'tv', 'radio', 'internet')])
screeplot(pc_type, type = "lines")
pc_type_simple <- princomp(set02_simple[,c('newspapers', 'magazines', 'tv', 'radio', 'internet')])
screeplot(pc_type_simple, type = "lines")

set02 <- set02 %>%
        mutate(typePC = scale(pc_type$scores[,1]))
set02_simple <- set02 %>%
        mutate(typePC = scale(pc_type_simple$scores[,1]))
        

saveRDS(set02, "set02.rds")
saveRDS(set02_simple, "set02_simple.rds")
set02 <- readRDS("set02.rds")
set02_simple <- readRDS("set02_simple.rds")

# consider multidimensional scaling and self organising maps on the clusters :

# 1st create a subset to ensure easier running
set.seed(56)
sub02 <- set02[sample(nrow(set02), size = 0200),]

# distance matrix and MDS
sub02_dist <- dist(sub02[,c("newspapers","magazines","radio", "tv", "internet")])
mds02 <- cmdscale(sub02_dist)
plot(mds02, col = as.numeric(sub02$cluster) + 1, pch = 19, ylab = "", xlab = "")

# 3D scaling
mds3 <- cmdscale(dist(sub02[,c("newspapers", "magazines", "radio", "tv", "internet")]), k = 3)
mds3 <- as.data.frame(mds3)

# 2D & 3D Scatterplots of 5 centers
jpeg('kmeans2DPlot2002.jpeg')
plot(mds02, col = as.numeric(sub02$cluster) + 1, ylab = "", xlab = "", pch = 19)
dev.off()

jpeg('kmeans3DPlot2002.jpeg')
scatterplot3d(mds3, color = as.numeric(sub02$cluster) + 1, xlab = '', ylab = '', zlab = '')
dev.off()

# Spinning 3D for 5 classes
jpeg('kmeansSpinningPlot2002.png')
plot3d(jitter(mds3$V1), jitter(mds3$V2), jitter(mds3$V3), col= as.numeric(sub02$cluster) + 1, size=5, xlab = '', ylab = '', zlab = '', pch = 19)
dev.off()

# try some Self Organising Maps.... try to explain the differences....

# set up somgrid
grid <- somgrid(xdim = 02, ydim = 02, topo = "hexagonal")

# run som
# set up as data matrix
mat_sub <- as.matrix(sub02[,c('newspapers', 'magazines', 'radio', 'tv','internet')])
som_sub <- som(mat_sub, grid = grid, rlen = 02000) 

par(mfrow = c(1,1))
plot(som_sub, type = "codes")
plot(som_sub, type = "changes")
plot(som_sub, type = "counts")
plot(som_sub, type = "dist.neighbours")
plot(som_sub, type = "quality")

par(mfrow = c(3,2))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,1], main = names(sub02['newspapers']))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,2], main = names(sub02['magazines']))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,3], main = names(sub02['radio']))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,4], main = names(sub02['tv']))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,5], main = names(sub02['internet']))

par(mfrow = c(1,1))
plot(som_sub, type = "mapping", bgcol = sub02$cluster ) # not very good organising??

# Try pca to get sense of relative use of media type... not very helpful since in most cases require many components to reflect variation in the data.

mags_pca <- princomp(scale(magazines_engagement_02))
screeplot(mags_pca, type = "lines")
newsp_pca <- princomp(scale(newspapers_engagement_02))
screeplot(newsp_pca, type = "lines")
tv_pca <- princomp(scale(tv_engagement_02))
screeplot(tv_pca, type = "lines")
rad_pca <- princomp(scale(radio_engagement_02[,-60])) # cant divide by zero
screeplot(rad_pca, type = "lines")
int_pca <- princomp(scale(internet_engagement_02))
screeplot(int_pca, type = "lines")

all_pca <- princomp(set02[,c('newspapers','magazines', 'tv', 'radio', 'internet')])
screeplot(all_pca, type = "lines")
summary(all_pca) # first component could be useful (@~40% of variation) to give relative multimedia scores

# try kmeans on the first pca and compare with cluster values...
test <- kmeans(all_pca$scores[,1], centers = 6)
test$cluster
set02$cluster
cor(test$cluster, as.numeric(set02$cluster))

# consider for some predictions:
# create training and test sets:

set.seed(56)
ind_train <- createDataPartition(set02$cluster, p = 0.7, list = FALSE)
training <- set02[ind_train,]
testing <- set02[-ind_train,]

# # using random forest:
forest02_type <- randomForest(cluster ~ newspapers
                              + tv
                              + radio
                              + magazines
                              + internet,
                              data = training )

pred_forest02_type <- predict(forest02_type, newdata = testing)

confusionMatrix(pred_forest02_type, testing$cluster) 

# with lda. Although given accuracy of forest,  no real need.
set.seed(56)
lda02 <- lda(cluster ~ newspapers
             + tv
             + radio
             + magazines
             + internet,
             data = training)
summary(lda02)

pred_lda02 <- predict(lda02, newdata = testing)
confusionMatrix(pred_lda02$class, testing$cluster) # 

# using only demographic information
forest02_demogr <- randomForest(cluster ~ age
                                + sex
                                + edu
                                + hh_inc
                                + race
                                + lang
                                + lifestages
                                + mar_status
                                + lsm
                                + lifestyle
                                + attitudes,
                                data = training)

pred_forest02_demogr <- predict(forest02_demogr, newdata = testing)

confusionMatrix(pred_forest02_demogr, testing$cluster)

# with lda
set.seed(56)
lda02_demogr <- lda(cluster ~ age
                    + sex
                    + edu
                    + hh_inc
                    + race
                    + lang
                    + lifestages
                    + mar_status
                    + lsm
                    + lifestyle
                    + attitudes,
                    data = training)

pred_lda02_demogr <- predict(lda02_demogr, newdata = testing)
confusionMatrix(pred_lda02_demogr$class, testing$cluster)

##  some qualitative consideration of the four types:

# consider a single tree partitioning to try to add meaning to the six clusters
control <- rpart.control(maxdepth = 4, cp = 0.001)
tree02 <- rpart(cluster ~ newspapers + tv + radio + magazines + internet, 
                data = set02,
                control = control) # weights = pwgt
par(mfrow = c(1,1))
plot(tree02, uniform = TRUE, margin = 0.2)
text(tree02, pretty = 0, cex = 0.8)

# for more detail
rpart.plot(tree02, type = 4, extra = 1, cex = 0.5)

percentile <- ecdf(set02$internet)
percentile(1.4)

# some plots
jpeg('typeBoxPlots_02.jpeg', quality = 020, type = "cairo")
par(mfrow = c(2,3))
plot(set02$radio ~ set02$cluster, col = c(2,3,4,5,6), main = "radio", xlab = "cluster", ylab = '')
plot(set02$tv ~ set02$cluster, col = c(2,3,4,5,6), main = "tv", xlab = "cluster", ylab = '')
plot(set02$newspapers ~ set02$cluster, col = c(2,3,4,5,6), main = "newspapers", xlab = "cluster", ylab = '')
plot(set02$magazines ~ set02$cluster, col = c(2,3,4,5,6), main = "magazines", xlab = "cluster", ylab = '')
plot(set02$internet ~ set02$cluster, col = c(2,3,4,5,6), main = "internet", xlab = "cluster", ylab = '')
dev.off()

# try to make sense of demographics
jpeg('typeDemogPlots1_02.jpeg', quality = 020, type = "cairo")
par(mfrow = c(2,2))
plot(set02$cluster ~ factor(set02$race,labels = c("black", "coloured", "indian", "white")), col = c(2,3,4,5,6), main = "race", xlab = "", ylab = "")
plot(set02$cluster ~ factor(set02$edu, labels = c("<matric", "matric",">matric" )), col = c(2,3,4,5,6), main = "education", xlab = "", ylab = "")
plot(set02$cluster ~ factor(set02$age, labels = c("15-24","25-44", "45-54","55+")), col = c(2,3,4,5,6), main = "age", xlab = "", ylab = "")
plot(set02$cluster ~ factor(set02$lsm, labels = c("1-2", "3-4", "5-6", "7-8", "9-02")), col = c(2,3,4,5,6), main = "LSM", xlab = "", ylab = "")
dev.off()

jpeg('typeDemogPlots2_02.jpeg', quality = 020, type = "cairo")
par(mfrow = c(2,2))
plot(set02$cluster ~ factor(set02$sex, labels = c("male", "female")), col = c(2,3,4,5,6), main = "sex", xlab = "", ylab = "")
plot(set02$cluster ~ factor(set02$hh_inc, labels = c("<2500","2500-6999","7000-11999",">=02000")), col = c(2,3,4,5,6), main = "hh_inc", xlab = "", ylab = "")
plot(set02$cluster ~ set02$lifestages, col = c(2,3,4,5,6), main = "lifestages", xlab = "", ylab = "")
# plot(set02$cluster ~ set02$lifestyle, col = c(2,3,4,5,6), main = "lifestyle", xlab = "", ylab = "")
dev.off()
