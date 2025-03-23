# https://archive.ics.uci.edu/dataset/1025/turkish+crowdfunding+startups
library(dplyr)
library(ggplot2)
library(caret)
library(corrplot)
library(car)
library(foreign)
library(epitools)

dataset <- read.csv("data/turkishCF.csv", sep=";")
dataset <- dataset[,-1]
colnames(dataset) <- c(
  "platform",
  "crowdfunding_type",
  "category",
  "funding_method",
  "project_name",
  "project_owner",
  "project_owner_gender",
  "num_projects_backed",
  "num_projects_subscribed",
  "num_projects_owned",
  "num_teams_part_of",
  "location",
  "region",
  "year",
  "project_start_date",
  "project_end_date",
  "project_duration_days",
  "has_promo_video",
  "promo_video_length",
  "num_images",
  "has_faq",
  "num_updates",
  "num_comments",
  "num_backers",
  "num_rewards",
  "team_size",
  "has_website",
  "has_social_media",
  "num_social_media_accounts",
  "num_social_followers",
  "num_tags",
  "description_word_count",
  "project_description",
  "funding_target",
  "funding_collected",
  "funding_percentage",
  "success_status"
)

summary(dataset)

# translate the dataset to English
head(dataset)
dataset$success_status <- as.factor(dataset$success_status)
dataset$category <- as.factor(dataset$category)

colnames(dataset)[colnames(dataset) == "has_faq"] <- "frequently_asked_questions_numb"

dataset$category <- factor(dataset$category, 
                               levels = c("çevre", "dans-performans", "diğer", "eğitim", "film-video-fotoğraf",
                                          "gıda-yeme-içme", "hayvanlar", "kültür-sanat", "moda", "müzik", 
                                          "sağlık-güzellik", "sosyal sorumluluk", "spor", "tasarım", 
                                          "teknoloji", "turizm", "yayıncılık"),
                               labels = c("environment", "dance-performance", "other", "education", "film-video-photography",
                                          "food-drink", "animals", "culture-art", "fashion", "music", 
                                          "health-beauty", "social-responsibility", "sports", "design", 
                                          "technology", "tourism", "publishing"))

# squash categories in bigger groups
# culture_and_education, lifestyle, technology
dataset$generalized_category <- factor(dataset$category, 
                          levels = c("environment", "dance-performance", "other", "education", "film-video-photography",
                                     "food-drink", "animals", "culture-art", "fashion", "music", 
                                     "health-beauty", "social-responsibility", "sports", "design", 
                                     "technology", "tourism", "publishing"),
                          labels = c("other", "culture_and_education", "other", "culture_and_education", "culture_and_education",
                                     "lifestyle", "lifestyle", "culture_and_education", "lifestyle", "culture_and_education", 
                                     "lifestyle", "culture_and_education", "lifestyle", "lifestyle", 
                                     "technology", "lifestyle", "culture_and_education"))

# make dummies for all the categories
dataset$is_cultural <- ifelse(dataset$generalized_category == 'culture_and_education', 1, 0)
dataset$is_cultural <- as.factor(dataset$is_cultural)
dataset$is_technology <- ifelse(dataset$generalized_category == 'technology', 1, 0)
dataset$is_technology <- as.factor(dataset$is_technology)
dataset$is_lifestyle <- ifelse(dataset$generalized_category == 'lifestyle', 1, 0)
dataset$is_lifestyle <- as.factor(dataset$is_lifestyle)
dataset$is_other <- ifelse(dataset$generalized_category == 'other', 1, 0)
dataset$is_other <- as.factor(dataset$is_other)

summary(dataset)


dataset$crowdfunding_type <- factor(dataset$crowdfunding_type, levels = c("bağış", "ödül"), labels = c("Donation", "Reward"))
dataset$is_donation <- ifelse(dataset$crowdfunding_type == "Donation", 1, 0)
dataset$is_donation <- as.factor(dataset$is_donation)
# başarılı is successful, başarısız is unsuccessful
dataset$success_status <- factor(dataset$success_status, levels = c("başarılı", "başarısız"), labels = c(1, 0))
#dataset$success_status <- as.numeric(dataset$success_status)

dataset$funding_method <- factor(dataset$funding_method, levels = c("hepsi kalsın", "ya hep ya hiç"), labels = c("Flexible", "All Or Nothing"))
dataset$is_all_or_nothing <- ifelse(dataset$funding_method == "All Or Nothing", 1, 0)
dataset$is_all_or_nothing <-factor(dataset$is_all_or_nothing)

dataset$funding_method <- as.factor(dataset$funding_method)
summary(dataset$funding_method)
dataset$crowdfunding_type <- as.factor(dataset$crowdfunding_type)
dataset$description_word_count <- as.numeric(dataset$description_word_count)

dataset$platform <- as.factor(dataset$platform)
summary(dataset$platform)
dataset$is_fongogo <- ifelse(dataset$platform == "fongogo", 1, 0)
dataset$is_fongogo <- as.factor(dataset$is_fongogo)

dataset$location <- as.factor(dataset$location)
summary(dataset$location)
# replace the Turkish values with English

# remove percent mark
# Percentage of the target amount achieved.
dataset$funding_percentage <- as.numeric(gsub("%", "", dataset$funding_percentage))
summary(dataset$funding_percentage)

# https://en.wikipedia.org/wiki/List_of_largest_cities_and_towns_in_Turkey
# list aggregated by taking in consideration Population Size, Economic Importance, Cultural and Administrative Role 
big_cities <- c("adana", "ankara", "antalya", "bursa", "diyarbakır", 
                "eskişehir", "gaziantep", "istanbul", "izmir", "kayseri", 
                "kocaeli", "konya", "mersin", "muğla", "samsun", "sakarya", 
                "trabzon", "şanlıurfa", "tekirdağ")  
dataset$in_big_city = ifelse(dataset$location %in% big_cities, 1, 0)
dataset$in_big_city <- as.factor(dataset$in_big_city)
summary(dataset$in_big_city)

summary(dataset$project_owner_gender)

dataset$project_owner_gender <- factor(dataset$project_owner_gender, levels = c("kadın", "erkek", "belirsiz"), labels = c("male", "female", NA))
dataset$has_promo_video <- as.factor(dataset$has_promo_video)
dataset$has_promo_video <- factor(dataset$has_promo_video, levels = c("var", "yok"), labels = c(1, 0))
summary(dataset$has_promo_video)

dataset$has_website <- factor(dataset$has_website, levels = c("var", "yok"), labels = c(1, 0))
dataset$has_social_media <- factor(dataset$has_social_media, levels = c("var", "yok"), labels = c(1, 0))

dataset$has_female_owner <- ifelse(dataset$project_owner_gender == 'female', 1, 0)
dataset$has_female_owner <- as.factor(dataset$has_female_owner)

# replace 0 with 1 to fix the scaling with the logarithm
dataset$num_social_followers[dataset$num_social_followers == 0] <- 1
dataset$have_followers <- dataset$num_social_followers != 1
dataset$log_social_media_followers <- log(dataset$num_social_followers)


dataset$log_funding_target <- log(dataset$funding_target)

dataset$have_followers <- as.factor(dataset$have_followers)
dataset$have_followers <- factor(dataset$have_followers, levels = c("TRUE", "FALSE"), labels = c(1, 0))


