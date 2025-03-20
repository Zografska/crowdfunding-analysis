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
                                        description_word_count,is_fongogo, in_big_city, has_female_owner,log_social_media_followers, log_funding_target)
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

split <- splitmix(variables)

# consists of normalizing quantitative and categorical variables and then applying PCA on transformed data
famd_result <- FAMD(variables, graph = FALSE)

famd_result$eig

fviz_screeplot(famd_result, addlabels = TRUE, ylim = c(0, 50))

# show contribution of the lst 5 variables to the first two dimensions

fviz_contrib(famd_result, choice = "var", axes = 1, bottom=10)
fviz_contrib(famd_result, choice = "var", axes = 2,bottom=10)

# Dim 1 low contributions are 
#is_donation, is_all_or_nothing, has_website,
#description_word_count, promo_video_length, frequently_asked_questions_numb,
#log_funding_target

# Dim 2 low contributions are
# success_status, description_word_count, promo_video_length, 
# is_all_or_nothing, has_website, has_social_media, have_followers, has_female_owner

reduced_variables <- variables %>% dplyr::select(-c(description_word_count,promo_video_length,is_all_or_nothing,has_website, has_social_media, have_followers,has_female_owner))
famd_result <- FAMD(reduced_variables, graph = FALSE)
famd_result$eig

# arrange one plot in two rows and two plots in one row

plot(famd_result, choix="ind", cex=0.8, lab.ind=FALSE, lab.var = TRUE)
plot(famd_result, choix = "quanti",  cex = 0.8)
plot(famd_result, choix = "quali", cex = 0.8)
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

sil_width <- c(NA)

for(i in 2:10){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# plot silhouette width (higher is better)

plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

# even though 8 clusters gives us bigger silluette score
# silutette score measures how close the points are to their own cluster
# 3 clusters it's more interpretable


pam_fit <- pam(gower_dist, diss = TRUE, k = 3)

pam_results <- variables_clean %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(cluster_summary = summary(.))


pam_results$cluster_summary
# first cluster: most of them were donations, and all have social media, followers, and mostly in big cities
# project_duration_days mean: 48.7
# faq 0.1652
# log_social_media_followers 6.171
# log_funding_target: 9.547
# description_word_count: 437.1, high max 3473.0
# promo video length: 91.77
# 0.31% success rate
# 0.49% of the samples

# second cluster: mostly not donations, no social media, no followers, but mostly in big cities, by female owners
# project_duration_days mean: 48.98
# faq 0.26
# log_social_media_followers: 0.02208 - low social media folowers
# log_funding_target: 9.397
# description_word_count: 378.2
# promo video length: 65.85
# 0.17% success rate
# 0.21% of the samples

# second cluster: not donations, is all or nothing, no social media and no web site, mostly no followers,  mostly in small cities, 1/4 by female owners
# project_duration_days mean: 59.76
# faq: 0.1279
# log_social_media_followers: 0.3227 - lower social media folowers than c1
# log_funding_target: 9.774
# description_word_count: 197.6 - shorter description
# promo video length: 28.59 - shorter promo video than the others
# 0.14% success rate - low success rate
# 0.29% of the samples - big part of the dataset


# takes a high dimensional dataset and reduces it into a lower dimentional graph
# finds a way to project data into a lower dimension while preserving the clustering of the data
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))

ggplot(aes(x = X, y = Y, color = cluster), data = tsne_data) +
  geom_point()
           
# c1 and c2 suggest more intercluster variability since they are spread out  


# with increasing k to 8 we loose some interpretability
pam_fit_k8 <- pam(gower_dist, diss = TRUE, k = 8)
tsne_data_k8 <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit_k8$clustering))

ggplot(aes(x = X, y = Y, color = cluster), data = tsne_data_k8) +
  geom_point()
# difference in siluette scores for k=3 and k=8 
sil_width[3] - sil_width[8]
# -0.0077 which suggests a small improvement
# we can take k=3 to perserve the interpretability

# Dimensionality reduction with CMDS
# classical multidimensional scaling aims at finding low-dimensional structure by preserving pairwise distances of data
cmds <- cmdscale(gower_mat, eig = TRUE)

par(mfrow = c(2, 2))
plot(cmds$points, asp = 1, col = variables_clean$success_status, main = "Success Status", xlab="", ylab="")
plot(cmds$points, asp = 1, col = variables_clean$in_big_city, main = "Big City",xlab="", ylab="")
plot(cmds$points, asp = 1, col = variables_clean$has_female_owner, main = "Female Owner",xlab="", ylab="")
plot(cmds$points, asp = 1, col = variables_clean$has_social_media, main = "Has Social Media",xlab="", ylab="")
# cmds axes don't have intrinsic meaning - loss of interpretability
# but we can color code the categorical values to get some interpretation
