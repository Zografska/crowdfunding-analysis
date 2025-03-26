library(PCAmixdata)
library(FactoMineR)
library(factoextra)
library(ade4)
library(dplyr) 
library(ISLR) 
library(cluster) 
library(Rtsne) 
library(ggplot2) 

par(mfrow=c(1,1))
# recode the categorical variables to be shown in FAMD
variables <- dataset %>% dplyr::select(success_status, is_donation, is_all_or_nothing,project_duration_days,
                                       promo_video_length,frequently_asked_questions_numb,has_website, has_social_media, have_followers, 
                                        description_word_count,is_fongogo, in_big_city, has_female_owner,
                                       is_cultural, is_technology,
                                       log_social_media_followers, log_funding_target,)
clean_split <- splitmix(variables)
# join categorical and quantitative variables
variables_clean <- cbind(clean_split$X.quali, clean_split$X.quanti)
quali_range <- ncol(clean_split$X.quali)

# recode the categorical variables so they would be visible in the graph for famd
variables$success_status <- ifelse(variables$success_status == 1, "Success", "Failure")
variables$success_status <- as.factor(variables$success_status)
variables$is_donation <- ifelse(variables$is_donation == 1, "Donation", "Not Donation")
variables$is_donation <- as.factor(variables$is_donation)
variables$is_all_or_nothing <- ifelse(variables$is_all_or_nothing == 1, "All or Nothing", "Flexible")
variables$is_all_or_nothing <- as.factor(variables$is_all_or_nothing)
variables$has_website <- ifelse(variables$has_website == 1, "Has Website", "No Website")
variables$has_website <- as.factor(variables$has_website)
variables$has_social_media <- ifelse(variables$has_social_media == 1, "Has Social Media", "No Social Media")
variables$has_social_media <- as.factor(variables$has_social_media)
variables$have_followers <- ifelse(variables$have_followers == 1, "Has Followers", "No Followers")
variables$have_followers <- as.factor(variables$have_followers)
variables$is_fongogo <- ifelse(variables$is_fongogo == 1, "Fongogo", "Not Fongogo")
variables$is_fongogo <- as.factor(variables$is_fongogo)
variables$in_big_city <- ifelse(variables$in_big_city == 1, "Big City", "Small City")
variables$in_big_city <- as.factor(variables$in_big_city)
variables$has_female_owner <- ifelse(variables$has_female_owner == 1, "FemaleOwner", "MaleOrUnknownOwner")
variables$has_female_owner <- as.factor(variables$has_female_owner)
variables$is_cultural <- ifelse(variables$is_cultural == 1, "Cultural", "NotCultural")
variables$is_cultural <- as.factor(variables$is_cultural)
variables$is_technology <- ifelse(variables$is_technology == 1, "Technology", "NotTechnology")
variables$is_technology <- as.factor(variables$is_technology)

split <- splitmix(variables)

# consists of normalizing quantitative and categorical variables and then applying PCA on transformed data
famd_result_initial <- FAMD(variables, graph = FALSE)

famd_result_initial$eig

scree1<-fviz_screeplot(famd_result_initial, addlabels = TRUE, ylim = c(0, 28), main = "Scree Plot Before Dim Red")

# show contribution of the lst 5 variables to the first two dimensions
dim(variables)
contrib1 <- fviz_contrib(famd_result_initial, choice = "var", axes = 1, bottom=10)
contrib2 <- fviz_contrib(famd_result_initial, choice = "var", axes = 2,bottom=10)
grid.arrange(contrib1, contrib2, ncol=2, nrow =1)

# Dim 1 low contributions are 
#is_donation, is_all_or_nothing, has_website,
#description_word_count, promo_video_length, frequently_asked_questions_numb,
#log_funding_target
# is_lifestyle, is_other, has_female_owner

# Dim 2 low contributions are
# success_status, description_word_count, promo_video_length, 
# is_all_or_nothing, has_website, has_social_media, have_followers, has_female_owner

reduced_variables <- variables %>% dplyr::select(-c(description_word_count,promo_video_length,is_all_or_nothing,has_website, has_social_media, have_followers, has_female_owner))
famd_result <- FAMD(reduced_variables, graph = FALSE)
famd_result$eig
scree2<-fviz_screeplot(famd_result, addlabels = TRUE, ylim = c(0, 28), main = "Scree Plot After Dim Red")
grid.arrange(scree1, scree2, ncol=2, nrow =1)

ind <- plot(famd_result, choix="ind", cex=0.5, lab.ind=FALSE, lab.var = FALSE)
quanti <- plot(famd_result, choix = "quanti",  cex = 0.8)
quali <- plot(famd_result, choix = "quali", cex = 0.8)
grid.arrange(ind, quali, ncol = 2)
quanti
summary(reduced_variables)

# the first and second dimension only account for 42% of the variation
# more dimensions are needed to explain the variation
# if we go up to the 5th dimension we get 78% of variance explained

# the gower index is used for mixed data
# to compute the distance between two data points
# different metric for each feature and aggregate them to get the overal distance
# dgower(x1,x2) = (sum(similarity(x1,x2)))/p
# p-numb of features
# how to compute the similarity between two data points
# if it's categorical distance is 0 if the values are the same, 1 otherwise
# for numerical abs(x1j-x2j)/range(j)

# we need to watch out to specify column types

gower_dist <- daisy(variables_clean,
                    metric = "gower", type = list(sym = 1:quali_range,
                                                  num = (quali_range + 1):(ncol(variables_clean)-2),
                                                  logratio = (ncol(variables_clean)-1):ncol(variables_clean) ))
summary(gower_dist)
summary(variables_clean)

gower_mat <- as.matrix(gower_dist)


# most similar pair
variables_clean[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
# most dissimilar pair

variables_clean[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# calculate silhouette width for many k using PAM
# PAM - Partitioning (clustering) of the data into k clusters “around medoids”, a more robust version of K-means.

sil_width <- c()
fits <- c()

for(i in 2:10){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  fits[i] <- pam_fit
}

# plot silhouette width (higher is better)

plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)
sil_width[3]-sil_width[2]

# silutette score measures how close the points are to their own cluster
# 2 clusters is better since it has a bigger sillhouette widht

pam_fit <- pam(gower_dist, diss = TRUE, k = 2)

pam_results <- variables_clean %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(cluster_summary = summary(.))


pam_results$cluster_summary

# first cluster: most of them were donations, most of them have social media, followers, 
# and mostly in big cities, majority have a female owner
# project_duration_days mean: 48.11
# faq 0.2236
# log_social_media_followers 4.994
# log_funding_target: 9.476
# description_word_count: 416.5, high max 3473.0
# promo video length: 86.9
# 0.28% inner cluster success rate
# 0.73% of the successful observations are in this cluster

# second cluster: mostly not donations, no social media, low followers,
# mostly no website, mostly in small cities, by female owners
# project_duration_days mean: 57.55
# faq 0.26
# log_social_media_followers: 0.4761 - low social media folowers
# log_funding_target: 9.745
# description_word_count: 265.4
# promo video length: 40.21
# 0.14% success rate
# 0.27% of the successful observations are in this cluster


# takes a high dimensional dataset and reduces it into a lower dimentional graph
# finds a way to project data into a lower dimension while preserving the clustering of the data
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))

ggplot(aes(x = X, y = Y, color = cluster), data = tsne_data) +
  geom_point()

# Dimensionality reduction with CMDS
# classical multidimensional scaling aims at finding low-dimensional structure by preserving pairwise distances of data
cmds <- cmdscale(gower_mat, eig = TRUE)

par(mfrow = c(2, 3))
plot(cmds$points, asp = 1, col = variables_clean$success_status, main = "Success Status", xlab="", ylab="")
plot(cmds$points, asp = 1, col = variables_clean$in_big_city, main = "Big City",xlab="", ylab="")
plot(cmds$points, asp = 1, col = variables_clean$has_female_owner, main = "Female Owner",xlab="", ylab="")
plot(cmds$points, asp = 1, col = variables_clean$has_social_media, main = "Has Social Media",xlab="", ylab="")
plot(cmds$points, asp = 1, col = variables_clean$is_cultural, main = "Is Cultural",xlab="", ylab="")
plot(cmds$points, asp = 1, col = variables_clean$is_technology, main = "Is Technological",xlab="", ylab="")
# cmds axes don't have intrinsic meaning - loss of interpretability
# but we can color code the categorical values to get some interpretation

