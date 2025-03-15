library("rpart")
library("rpart.plot")
library("party") 
library("partykit")
# splitting
set.seed(101)
for_tree <- dataset %>% dplyr::select(success_status, project_duration_days, has_faq, num_comments,team_size, num_social_followers, num_tags, description_word_count, funding_collected, in_big_city)


train.idx <- sample(nrow(for_tree), 2/3 * nrow(for_tree)) 
data.train <- for_tree[train.idx, ]
data.test <- for_tree[-train.idx, ]

pptree<-ctree(success_status ~ ., data=data.train, control = ctree_control(mincriterion=0.5, minsplit=0, minbucket=100))
pptree
plot(pptree)

dim(data.train)
rtree<-rpart(success_status ~ ., data=data.train, cp=0.005)
printcp(rtree)
plotcp(rtree)
summary(rtree)