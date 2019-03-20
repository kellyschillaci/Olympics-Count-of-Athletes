library(readr)
setwd("C:\\Users\\kdoyl\\OneDrive\\Documents\\IST 707")
data <- read_csv("athlete_events.csv")
noc <- read_csv("noc_regions.csv")


library("plotly")
library("tidyverse")
library("data.table")
library("gridExtra")
library("knitr")

counts <- data %>% filter(Sport != "Art Competitions") %>%
  group_by(Year, Season) %>%
  summarize(
    Athletes = length(unique(ID)),
    Nations = length(unique(NOC)),
    Events = length(unique(Event))
  )
#Athletes by Season
ggplot(counts, aes(x=Year, y=Athletes, group=Season, color=Season)) +
  geom_point(size=2) +
  geom_line() +
  scale_color_manual(values=c("dark blue","dodgerblue"))

#Nations by Season
ggplot(counts, aes(x=Year, y=Nations, group=Season, color=Season)) +
  geom_point(size=2) +
  geom_line() +
  scale_color_manual(values=c("dark blue","dodgerblue"))

#Events by Season
ggplot(counts, aes(x=Year, y=Events, group=Season, color=Season)) +
  geom_point(size=2) +
  geom_line() +
  scale_color_manual(values=c("dark blue","dodgerblue"))


data <- data %>% filter(Sport != "Art Competitions")

# Recode year of Winter Games after 1992 to match the next Summer Games

original <- c(1994,1998,2002,2006,2010,2014)
new <- c(1996,2000,2004,2008,2012,2016)
for (i in 1:length(original)) {
  data$Year <- gsub(original[i], new[i], data$Year)
}
data$Year <- as.integer(data$Year)

# Table counting number of athletes by Year and Sex
counts_sex <- data %>% group_by(Year, Sex) %>%
  summarize(Athletes = length(unique(ID)))
counts_sex$Year <- as.integer(counts_sex$Year)
ggplot(counts_sex, aes(x=Year, y=Athletes, group=Sex, color=Sex)) +
  geom_point(size=2) +
  geom_line()  +
  scale_color_manual(values=c("magenta","darkblue")) +
  labs(title = "Number of male and female Olympians over time") +
  theme(plot.title = element_text(hjust = 0.5))
counts_NOC <- data %>% filter(Year %in% c(1936,1956,1976,1996,2016)) %>%
  group_by(Year, NOC, Sex) %>%
  summarize(Count = length(unique(ID))) %>%
  spread(Sex, Count) %>%
  mutate(Total = sum(M,F,na.rm=T)) %>%
  filter(Total > 49)
names(counts_NOC)[3:4] <- c("Male","Female")
counts_NOC$Male[is.na(counts_NOC$Male)] <- 0
counts_NOC$Female[is.na(counts_NOC$Female)] <- 0
counts_NOC$Year <- as.factor(counts_NOC$Year)


# Count number at 1936 Olympics
counts_1936 <- data %>% filter(Year==1936, !is.na(Medal)) %>%
  group_by(NOC, Medal) %>%
  summarize(Count=length(Medal)) 


# Order NOC by total medal count
levs_1936 <- counts_1936 %>%
  group_by(NOC) %>%
  summarize(Total=sum(Count)) %>%
  arrange(Total) %>%
  select(NOC)
counts_1936$NOC <- factor(counts_1936$NOC, levels=levs_1936$NOC)

# Plot 1936
ggplot(counts_1936, aes(x=NOC, y=Count, fill=Medal)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values=c("navyblue","gray","lightblue")) +
  ggtitle("Medals 1936") +
  theme(plot.title = element_text(hjust = 0.5))
# Count number of medals awarded to each NOC at 1976 Olympics
counts_1976 <- data %>% filter(Year==1976, !is.na(Medal)) %>%
  group_by(NOC, Medal) %>%
  summarize(Count=length(Medal)) 

# Order NOC by total medal count
levs_1976 <- counts_1976 %>%
  group_by(NOC) %>%
  summarize(Total=sum(Count)) %>%
  arrange(Total) %>%
  select(NOC)
counts_1976$NOC <- factor(counts_1976$NOC, levels=levs_1976$NOC)
ggplot(counts_1976, aes(x=NOC, y=Count, fill=Medal)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values=c("navyblue","gray","lightblue")) +
  ggtitle("Medals 1976 Olympics") +
  theme(plot.title = element_text(hjust = 0.5))


# Count number of medals awarded to each NOC at 2014/2016 Olympics
counts_2016 <- data %>% filter(Year==2016, !is.na(Medal)) %>%
  group_by(NOC, Medal) %>%
  summarize(Count=length(Medal)) 

# Order NOC by total medal count

levs_2016 <- counts_2016 %>%
  group_by(NOC) %>%
  summarize(Total=sum(Count)) %>%
  arrange(Total) %>%
  select(NOC) 
 
counts_2016$NOC <- factor(counts_2016$NOC, levels=levs_2016$NOC)
ggplot(counts_2016, aes(x=NOC, y=Count, fill=Medal)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values=c("navyblue","gray","lightblue")) +
  ggtitle("Medals 2014/2016 Olympics") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size=6))


# Add regions to data and remove missing points
data_regions <- data %>% 
  left_join(noc,by="NOC") %>%
  filter(!is.na(region))

Amsterdam <- data_regions %>% 
  filter(Games == "1936 Summer") %>%
  group_by(region) %>%
  summarize(amsterdam = length(unique(ID)))
Munich <- data_regions %>% 
  filter(Games == "1976 Summer") %>%
  group_by(region) %>%
  summarize(munich = length(unique(ID)))
Rio <- data_regions %>% 
  filter(Games == "2016 Summer") %>%
  group_by(region) %>%
  summarize(rio = length(unique(ID)))
world <- map_data("world")
mapdat <- tibble(region=unique(world$region))
mapdat <- mapdat %>% 
  left_join(amsterdam, by="region") %>%
  left_join(munich, by="region") %>%
  left_join(rio, by="region")
mapdat$Amsterdam[is.na(mapdat$Amsterdam)] <- 0
mapdat$Munich[is.na(mapdat$Munich)] <- 0
mapdat$Rio[is.na(mapdat$Rio)] <- 0
world <- left_join(world, mapdat, by="region")
ggplot(world, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Amsterdam)) +
  labs(title = "Amsterdam 1928",
       x = NULL, y=NULL) +
  guides(fill=guide_colourbar(title="Athletes")) +
  scale_fill_gradient(low="lightblue",high="navyblue")
# Plot: Munich 1972
ggplot(world, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Munich)) +
  labs(title = "Munich 1972",
       x = NULL, y = NULL) +
  guides(fill=guide_colourbar(title="Athletes")) +
  scale_fill_gradient2(low = "lightblue", high = "navyblue")
ggplot(world, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Rio)) +
  labs(title = "Rio 2016",
       x = NULL, y = NULL) +
  guides(fill=guide_colourbar(title="Athletes")) +
  scale_fill_gradient2(low="lightblue",high = "navyblue")

data$Medal[is.na(data$Medal)] <- 0
head(data)

data$Medal1 <- gsub("Gold", "1", data$Medal)
data$Medal1 <- gsub("Silver", "1", data$Medal1)
data$Medal1 <- gsub("Bronze", "1", data$Medal1)

MedalData <- cbind(data$Age, data$Height, data$Weight, data$Team, data$Medal,data$Medal1)
colnames(MedalData) <- c("Age", "Height", "Weight", "Team", "Medal", "Medal1")

head(MedalData)

MD <- as.data.frame(MedalData[complete.cases(MedalData),])
MD$Medal=factor(MD$Medal)
MD$Team=factor(MD$Team)

MD1 <- MD[,-6]
(n <- round(nrow(MD1)/5))
(s <- sample(1:nrow(MD1), n))
## The test set is the sample
MD1Train <- MD1[s,]
## The training set is the not sample
MD1Test <- MD1[-s,]

MD1Test_no_labels <- MD1Test[,-5]
MD1Test_labels <- MD1Test[,5]
MD1Train_no_labels <- MD1Train[,-5]
MD1Train_labels <- MD1Train[,5]

MD2 <- MD[,-5]
(n2 <- round(nrow(MD2)/5))
(s2 <- sample(1:nrow(MD2), n2))
## The test set is the sample
MD2Train <- MD2[s2,]
## The trainng set is the not sample
MD2Test <- MD2[-s2,]

MD2Test_no_labels <- MD2Test[,-5]
MD2Test_labels <- MD2Test[,5]
MD2Train_no_labels <- MD2Train[,-5]
MD2Train_labels <- MD2Train[,5]
str(MD)


###Rules Done
library(arules)
library(arulesViz)
rules=apriori(data=MD2, parameter = list(supp=.001, conf=.005, minlen=2),
              appearance = list(default="rhs", lhs=c("Medal1=1")),
              control=list(verbose=FALSE))
inspect(rules)
inspect(MD1)

plot(rules[1:20],method="graph", control=list(type="items", engine="interactive"))

SortedRulesC <- sort(rules, by="confidence", decreasing=TRUE)
inspect(SortedRulesC[1:10])
plot(SortedRulesC[1:10],method="graph", control=list(type="items", engine="interactive"))
SortedRulesL <- sort(rules, by="lift", decreasing=TRUE)
inspect(SortedRulesL[1:10])
plot(SortedRulesL[1:10],method="graph", control=list(type="items", engine="interactive"))
SortedRulesS <- sort(rules, by="support", decreasing=TRUE)
inspect(SortedRulesS[1:10])
plot(SortedRulesS[1:10],method="graph", control=list(type="items", engine="interactive"))

library(cluster)
NoMedals <- MD1[,-5]
NoMedals$Age<-as.character(NoMedals$Age)
NoMedals$Age<-as.numeric(NoMedals$Age)
NoMedals$Height<-as.character(NoMedals$Height)
NoMedals$Height<-as.numeric(NoMedals$Height)
NoMedals$Weight<- as.character(NoMedals$Weight)
NoMedals$Weight<- as.numeric(NoMedals$Weight)
x<-as.data.frame(NoMedals)
x<- x[,-4]
str(x)

str(NoMedals)

boxplot(x$Age)
boxplot(x$Height)
boxplot(x$Weight)
x$WeightNorm <-(x$Weight-mean(x$Weight))/sd(x$Weight)
x$HeightNorm <-(x$Height-mean(x$Height))/sd(x$Height)
mean(x$WeightNorm)
mean(x$HeightNorm)

xkm <- kmeans(x[4:5], 
              centers= 4)

xkm3 <-kmeans(x[4:5], centers = 3)
xkm5 <-kmeans(x[4:5], centers = 5)

## Find cluster size
clus.size <-xkm$size
clus <- data.frame(table(xkm$cluster))
clus3 <- data.frame(table(xkm3$cluster))
clus5 <- data.frame(table(xkm5$cluster))
## Plot a Bar chart for cluster size
names(clus) <- c("Cluster","Size")

cp <-barplot(height=clus$Size ,
             names.arg=clus$Cluster,
             xlab="Cluster",
             ylab="Object Counts",
             main="Cluster Size",
             col=c("darkorchid1","firebrick2","darkseagreen2","goldenrod1"),
             ylim= c(0,max(clus$Size)+500 ) ,
             border=NA 
)

plot(clus, what = "classification")

(table(MD1$Medal,xkm$cluster))
(table(MD1$Medal,xkm3$cluster))
(table(MD1$Medal,xkm5$cluster))

library(mclust)
Clust_EM <- Mclust(x,G=4)

#plot(Clust_EM, what = "classification")


library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(Cairo)

fit <-rpart(MD2$Medal1 ~ ., data = MD2, method="class")
fancyRpartPlot(fit)

library(randomForest)

rf <- randomForest(Medal1 ~.
                   , data = MD2Train)
pred_RF<-predict(rf, MD2Test_no_labels) 
(table(pred_RF, MD2Test_labels))
(attributes(rf))
(rf$confusion)
(rf$classes)

hist(treesize(rf))

library(e1071)

tuned_costP <- tune(svm,Medal1~., data=MD2Train,
                    kernel="polynomial", 
                    ranges=list(cost=c(.01,.1,1,10,100,100)))
summary(tuned_costP)


svmp <- svm(Medal1~., data=MD2Train, 
            kernel="polynomial", cost=10, 
            scale=FALSE)
print(svmp)

##Prediction --
pred_P <- predict(svmp, MD2Test_no_labels, type="class")
## COnfusion Matrix
(Ptable <- table(pred_P, MD2Test_labels))
confusionMatrix(Ptable)

tuned_costL <- tune(svm,Medal1~., data=MD2Train,
                   kernel="linear", 
                   ranges=list(cost=c(.01,.1,1,10,100,100)))
summary(tuned_costL)

svml <- svm(Medal1~., data=MD2Train, 
            kernel="linear", cost=.1, 
            scale=FALSE)
print(svml)

##Prediction --
pred_l <- predict(svml, MD2Test_no_labels, type="class")
## COnfusion Matrix
ltable <- table(pred_l, MD2Test_labels)
confusionMatrix(ltable)
tuned_costR <- tune(svm,Medal1~., data=MD2Train,
                    kernel="radial", 
                    ranges=list(cost=c(.01,.1,1,10,100,100)))
summary(tuned_costR)
svmr <- svm(Medal1~., data=MD2Train, 
            kernel="radial", cost=.01, 
            scale=FALSE)
print(svmr)

##Prediction --
pred_r <- predict(svmr, MD2Test_no_labels, type="class")
## COnfusion Matrix
rtable <- table(pred_r, MD2Test_labels)

confusionMatrix(rtable)



###Naive Bayes Done
library(naivebayes)
nb <- naive_bayes(Medal ~.,
                  data=MD1Train, na.action = na.pass)
nbpred <- predict(nb, MD1Test_no_labels)

table(nbpred,MD1Test_labels)

library(caret)
confusionMatrix(nbpred,MD1Test_labels)
nb2 <- naive_bayes(Medal1 ~., data=MD2Train, na.action = na.pass)
nbpred2 <- predict(nb2, MD2Test_no_labels)
table(nbpred2,MD2Test_labels)
confusionMatrix(nbpred2,MD2Test_labels)


WC1936 <- data %>% filter(Year==1936)
WC1936 <- WC1936$Team
library(tm)
WC1936Corpus <- Corpus(VectorSource(WC1936))
library(slam)
WC1936_dtm <- DocumentTermMatrix(WC1936Corpus,
                                  control = list(
                                    removePunctuation = T,
                                    removeNumbers = T,
                                    tolower=T,
                                    stemming = T,
                                    remove_separators = T))
(WordFreq <- colSums(as.matrix(WC1936_dtm)))
(head(WordFreq))
(length(WordFreq))
ord <- order(WordFreq)
(WordFreq[head(ord)])
(WordFreq[tail(ord)])

WC1936_dtm_M <- as.matrix(WordFreq)


library(wordcloud)
wordcloud(rownames(WC1936_dtm_M), WC1936_dtm_M)

WC1976 <- data %>% filter(Year==1976)
WC1976 <- WC1976$Team
library(tm)
WC1976Corpus <- Corpus(VectorSource(WC1976))
library(slam)
WC1976_dtm <- DocumentTermMatrix(WC1976Corpus,
                                 control = list(
                                   removePunctuation = T,
                                   removeNumbers = T,
                                   tolower=T,
                                   stemming = T,
                                   remove_separators = T))
(WordFreq <- colSums(as.matrix(WC1976_dtm)))
(head(WordFreq))
(length(WordFreq))
ord <- order(WordFreq)
(WordFreq[head(ord)])
(WordFreq[tail(ord)])

WC1976_dtm_M <- as.matrix(WordFreq)


library(wordcloud)
wordcloud(rownames(WC1976_dtm_M), WC1976_dtm_M)

WC2016 <- data %>% filter(Year==2016)
WC2016 <- WC2016$Team
library(tm)
WC2016Corpus <- Corpus(VectorSource(WC2016))
library(slam)
WC2016_dtm <- DocumentTermMatrix(WC2016Corpus,
                                 control = list(
                                   removePunctuation = T,
                                   removeNumbers = T,
                                   tolower=T,
                                   stemming = T,
                                   remove_separators = T))
(WordFreq <- colSums(as.matrix(WC2016_dtm)))
(head(WordFreq))
(length(WordFreq))
ord <- order(WordFreq)
(WordFreq[head(ord)])
(WordFreq[tail(ord)])

WC2016_dtm_M <- as.matrix(WordFreq)


library(wordcloud)
wordcloud(rownames(WC2016_dtm_M), WC2016_dtm_M)

