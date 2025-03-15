# explore
#   plot
# model
#  model diagnostics
# improve/compare
# conclusion

# do k nearest neighbours

# we say that it's successful if the funding percentage is greater than 100%
# explore data

summary(dataset)

summary(dataset$success_status)

# compute the correlation matrix between numeric variables
numerical <- dataset %>% dplyr::select(project_duration_days, has_faq, num_comments, team_size, num_social_followers, num_tags, description_word_count, funding_target, funding_percentage, in_big_city)
correlation_matrix <- cor(numerical)
correlation_matrix

symnum(correlation_matrix, abbr.colnames = FALSE)

heatmap(x = correlation_matrix, symm = TRUE, Colv = NA, Rowv = NA)

qqnorm(dataset$funding_target)
plot(dataset$funding_target, col = dataset$success_status, pch = 19, xlab = "Funding Target", ylab = "Frequency", main = "Funding Target by Success Status")
# success status can be 0 or zero regardless of the funding target

dataset$log_funding_target <- log(dataset$funding_target)
dataset %>% ggplot(aes(x = success_status, y = log_funding_target)) + geom_boxplot() + geom_jitter(width = 0.2)

# we look at the distribution of the variable
summary(dataset$funding_target)

# remove error in data with funding target 1
dataset <- dataset[dataset$funding_target != 1,]

# the min looks realistic now
summary(dataset$funding_target)

# we plot again
dataset %>% ggplot(aes(x = success_status, y = log_funding_target)) + geom_boxplot() + geom_jitter(width = 0.3)

# is the difference in means statistically significant?
t.test(log_funding_target ~ success_status, data = dataset)
# p-value = 0.05063 we reject the null hypothesis that the means are equal at the 5% significance level
# this would suggest that the success status is associated with the funding target

# replace 0 with 1 to fix the scaling with the logarithm
dataset$num_social_followers[dataset$num_social_followers == 0] <- 1
dataset$have_followers <- dataset$num_social_followers != 1
dataset$log_social_media_followers <- log(dataset$num_social_followers)
summary(dataset$log_social_media_followers)

# take a look of the distribution of the log_social_media_followers
hist(dataset$log_social_media_followers, breaks = 50, ylim = c(0, 45), col = 'lightblue', main = "Log Social Media Followers", xlab = "Log Social Media Followers")
# it has a fairly normal distribution, except the big bar at 0

dataset %>% ggplot(aes(x = success_status, y = log_social_media_followers)) + geom_boxplot() + geom_jitter(width = 0.3)
# there is a visual jump in the mean of log_funding_percentage between the success statuses
# we can do a test to check if the difference is significant

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
dataset %>% ggplot(aes(x = success_status, y = description_word_count)) + geom_boxplot()
# plot description_word_count

dataset %>% ggplot(aes(x = success_status, y = description_word_count)) + geom_boxplot() + geom_jitter(width = 0.3)
# there is a visual jump in the mean of log_funding_percentage between the success statuses
# we can do a test to check if the difference is significant

# is the difference in means statistically significant?
t.test(description_word_count ~ success_status, data = dataset)
# yes

# is success_status and in_big_city associated

mytab <- xtabs(~success_status+in_big_city, data=dataset)
mytab

# make it fancy by adding the row and col percentages
mosaicplot(mytab, col=c("red","blue"), main="Success Status by Big City", xlab="Success Status", ylab="Big City")

# chi-square test
chisq.test(mytab)
# there is a statistically significant association between the variables in at the 5% significance level

# plot success against category
dataset %>% ggplot(aes(x = success_status, fill = generalized_category)) + geom_bar(position = "fill") + scale_fill_brewer(palette = "Paired")
# plot each category against success status and put in the number of projects

factor_order <- c(0,1)
dataset %>% ggplot(aes(x = generalized_category, fill = factor(success_status, factor_order))) + geom_bar() + geom_text(stat = "count", aes(label = ..count..), vjust = 1) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_brewer(palette = "Paired")


dataset %>% ggplot(aes(x = project_owner_gender, fill = factor(success_status, factor_order))) + geom_bar(position = "fill") + geom_bar() + geom_text(stat = "count", aes(label = ..count..), vjust = 1) + scale_fill_brewer(palette = "Paired")




########## Feature Selection ##########

