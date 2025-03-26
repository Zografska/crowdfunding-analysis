library(gridExtra)
summary(dataset)

summary(dataset$success_status)

# compute the correlation matrix between numeric variables
numerical <- dataset %>% dplyr::select(project_duration_days, num_comments, team_size, log_social_media_followers, num_tags, description_word_count, log_funding_target, funding_percentage)
correlation_matrix <- cor(numerical)
heatmap(x = correlation_matrix, margins = c(15, 15))

par(mfrow = c(1, 2))
qqnorm(dataset$funding_target)
# let's try the log transformation
qqnorm(dataset$log_funding_target, main = "Normal QQ Plot on Log Transformation")

shapiro.test(dataset$log_funding_target)
# p-value is really low, doens't pass the shapiro test


plot(dataset$funding_target, col = dataset$success_status, pch = 19, xlab = "Funding Target", ylab = "Frequency", main = "Funding Target by Success Status")
# success status can be 0 or zero regardless of the funding target

initial <- dataset %>% ggplot(aes(x = success_status, y = log_funding_target)) + geom_boxplot() + geom_jitter(width = 0.2)

# we look at the distribution of the variable
summary(dataset$funding_target)

# remove error in data with funding target 1
dataset <- dataset[dataset$funding_target != 1,]

# the min looks realistic now
summary(dataset$funding_target)

# we plot again
clean <- dataset %>% ggplot(aes(x = success_status, y = log_funding_target)) + geom_boxplot() + geom_jitter(width = 0.3)

grid.arrange(initial, clean, ncol=2, nrow =1)

# is the difference in means statistically significant?
t.test(log_funding_target ~ success_status, data = dataset)
# p-value = 0.05063 we reject the null hypothesis that the means are equal at the 5% significance level
# this would suggest that the success status is associated with the funding target

summary(dataset$log_social_media_followers)


# take a look of the distribution of the log_social_media_followers
# it has a fairly normal distribution, except the big bar at 0
## ggplot histogram with y limit
hist <- dataset %>% ggplot(aes(x = log_social_media_followers)) + geom_histogram(bins = 40, fill = "lightblue") + ylim(0, 75)

box_plot <- dataset %>% ggplot(aes(x = success_status, y = log_social_media_followers)) + geom_boxplot() + geom_jitter(width = 0.3)
# there is a visual jump in the mean of log_funding_percentage between the success statuses
# we can do a test to check if the difference is significant
grid.arrange(hist, box_plot, ncol = 2, nrow = 1)

# is the difference in means statistically significant?
t.test(log_social_media_followers ~ success_status, data = dataset)
# p-value = 2.2e-16 we reject the null hypothesis that the means are equal at the 5% significance level


# can 0 followers be explained by not having social media accounts?
# how many projects don't have social media accounts?
# do they correspond with those who have 0 followers?
table(dataset$have_followers, dataset$has_social_media)

# fix entry error for project that have followers but no social media account (followers implies account)
dataset$has_social_media[dataset$have_followers == 1 & dataset$has_social_media == 0] <- 1

table(dataset$have_followers, dataset$has_social_media)
# 10 of them have social media but no followers

# is success_status and has_social_media associated
mytab <- xtabs(~success_status+has_social_media, data=dataset)
mytab

# make it fancy by adding the row and col percentages
mosaicplot(mytab, col=c("pink","purple"), main="Success Status by Having Social Media", xlab="Success Status", ylab="Social Media")

# chi-square test
chisq.test(mytab)
# significant association between the variables at the 5% significance level

# plot funding_target vs num_social_followers
dataset %>% ggplot(aes(x = funding_target, y = num_social_followers, colour = success_status)) + geom_point() + xlim(0, 50000) + ylim(0, 36000)

# let's check description word count
summary(dataset$description_word_count)

hist <- dataset %>% ggplot(aes(x = description_word_count)) + geom_histogram(bins = 40, fill = "lightblue")
box_plot <- dataset %>% ggplot(aes(x = success_status, y = description_word_count)) + geom_boxplot() + geom_jitter(width = 0.3)

grid.arrange(hist, box_plot, ncol = 2, nrow = 1)
# is the difference in means statistically significant?
t.test(description_word_count ~ success_status, data = dataset)
# yes

# is success_status and in_big_city associated
mytab <- xtabs(~success_status+in_big_city, data=dataset)
mytab
mosaicplot(mytab, col=c("pink","violet"), main="Success Status by Big City", xlab="Success Status", ylab="Big City")

# chi-square test
chisq.test(mytab)
# there is a statistically significant association between the variables in at the 5% significance level

# plot success against category
box_plot <- dataset %>% ggplot(aes(x = success_status, fill = generalized_category)) + geom_bar(position = "fill") + scale_fill_brewer(palette = "Paired")

success <- factor(dataset$success_status, c(0,1))
reverse <- dataset %>% ggplot(aes(x = generalized_category, fill = success)) + geom_bar() + geom_text(stat = "count", aes(label = ..count..), vjust = 1) + theme(axis.text.x = element_text(angle = 35, hjust = 1)) + scale_fill_brewer(palette = "Paired")
grid.arrange(box_plot, reverse, ncol=2, nrow =1)
# calculate success by category
table(dataset$generalized_category, dataset$success_status)
# chi-square test for categories
chisq.test(table(dataset$generalized_category, dataset$success_status))
# the association is significant p-value = 3.831e-08

dataset %>% ggplot(aes(x = project_owner_gender, fill = )) + geom_bar(position = "fill") + geom_bar() + geom_text(stat = "count", aes(label = ..count..), vjust = 1)

