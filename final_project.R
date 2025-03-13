# https://archive.ics.uci.edu/dataset/1025/turkish+crowdfunding+startups

big_dataset <- read.csv("data/turkishCF.csv", sep=";")
big_dataset <- big_dataset[,-1]
colnames(big_dataset) <- c(
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

summary(big_dataset)

# translate the dataset to English
head(big_dataset)
big_dataset$success_status <- as.factor(big_dataset$success_status)
big_dataset$category <- as.factor(big_dataset$category)

big_dataset$category <- factor(big_dataset$category, 
                      levels = c("çevre", "dans-performans", "diğer", "eğitim", "film-video-fotoğraf",
                                 "gıda-yeme-içme", "hayvanlar", "kültür-sanat", "moda", "müzik", 
                                 "sağlık-güzellik", "sosyal sorumluluk", "spor", "tasarım", 
                                 "teknoloji", "turizm", "yayıncılık"),
                      labels = c("environment", "dance-performance", "other", "education", "film-video-photography",
                                 "food-drink", "animals", "culture-art", "fashion", "music", 
                                 "health-beauty", "social-responsibility", "sports", "design", 
                                 "technology", "tourism", "publishing"))

big_dataset$crowdfunding_type <- factor(big_dataset$crowdfunding_type, levels = c("bağış", "ödül"), labels = c("Donation", "Reward"))
big_dataset$is_donation <- ifelse(big_dataset$crowdfunding_type == "Donation", 1, 0)
big_dataset$success_status <- factor(big_dataset$success_status, levels = c("başarılı", "başarısız"), labels = c(1, 0))
big_dataset$funding_method <- factor(big_dataset$funding_method, levels = c("hepsi kalsın", "ya hep ya hiç"), labels = c("Flexible", "All Or Nothing"))
big_dataset$is_all_or_nothing <- ifelse(big_dataset$funding_method == "All Or Nothing", 1, 0)

big_dataset$funding_method <- as.factor(big_dataset$funding_method)
summary(big_dataset$funding_method)
big_dataset$crowdfunding_type <- as.factor(big_dataset$crowdfunding_type)



big_dataset$platform <- as.factor(big_dataset$platform)
summary(big_dataset$platform)
big_dataset$is_fongogo <- ifelse(big_dataset$platform == "fongogo", 1, 0)

big_dataset$location <- as.factor(big_dataset$location)
summary(big_dataset$location)
# replace the Turkish values with English

# remove percent mark
# Percentage of the target amount achieved.
big_dataset$funding_percentage <- as.numeric(gsub("%", "", big_dataset$funding_percentage))
summary(big_dataset$funding_percentage)

# https://en.wikipedia.org/wiki/List_of_largest_cities_and_towns_in_Turkey
# list aggregated by taking in consideration Population Size, Economic Importance, Cultural and Administrative Role 
big_cities <- c("adana", "ankara", "antalya", "bursa", "diyarbakır", 
                "eskişehir", "gaziantep", "istanbul", "izmir", "kayseri", 
                "kocaeli", "konya", "mersin", "muğla", "samsun", "sakarya", 
                "trabzon", "şanlıurfa", "tekirdağ")  
big_dataset$in_big_city = ifelse(big_dataset$location %in% big_cities, 1, 0)
summary(big_dataset$in_big_city)

big_dataset$is_cultural <- ifelse(big_dataset$category %in% c("film-video-photography", "culture-art"), 1, 0)

summary(big_dataset$project_owner_gender)

big_dataset$project_owner_gender <- factor(big_dataset$project_owner_gender, levels = c("kadın", "erkek", "belirsiz"), labels = c("male", "female", NA))
big_dataset$has_promo_video <- as.factor(big_dataset$has_promo_video)
big_dataset$has_promo_video <- factor(big_dataset$has_promo_video, levels = c("var", "yok"), labels = c(1, 0))
summary(big_dataset$has_promo_video)

big_dataset$has_website <- factor(big_dataset$has_website, levels = c("var", "yok"), labels = c(1, 0))
big_dataset$has_social_media <- factor(big_dataset$has_social_media, levels = c("var", "yok"), labels = c(1, 0))

big_dataset$has_female_owner <- ifelse(big_dataset$project_owner_gender == 'female', 1, 0)
