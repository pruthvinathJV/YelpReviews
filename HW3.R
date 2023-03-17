knitr::opts_chunk$set(echo = TRUE)

install.packages('expss')
install.packages("cowplot")
install.packages("gridExtra")
library("cowplot")
library("gridExtra")
library('tidyverse')
library('tidyverse')
library(ggplot2)
library(caret)
library(tidytext)
library(SnowballC)
library(textstem)
library(tidyverse)
library(textdata)
library(e1071)  # for the naiveBayes function
library(rsample)
library(pROC)
install.packages("dplyr")
install.packages("magrittr")
library(magrittr)
library(dplyr)
library('expss')

resReviewsData <- read_csv2('/Users/pruthvinathjeripityvenkata/Desktop/UIC/Sem 3/IDS 572/HW/HW3/yelpRestaurantReviews_sample_f22.csv')


#number of reviews by star-rating
resReviewsData %>% group_by(starsReview) %>% count()

#distribution of reviews across star ratings
ggplot(resReviewsData, aes(x=starsReview)) + geom_bar(width = 0.4, color="red", fill="red") + xlab("Ratings") + ylab("No. of Reviews")

#reviews from various states 
resReviewsData %>%   group_by(state) %>% tally()

#Distribution of reviews across states
ggplot(resReviewsData, aes(x=state)) + geom_bar(width = 0.4, color="darkblue", fill="darkblue") + xlab("State") + ylab("No. of Reviews") 

#Distribution of reviews across postal codes
resReviewsPostal <- resReviewsData %>% group_by(postal_code) %>% count() %>% arrange(desc(n))
resReviewsPostal <- ungroup(resReviewsPostal)
postal_code_top <- resReviewsPostal %>% top_n(10)

#to keep only the those reviews from 5-digit postal-codes
resReviewsData<-resReviewsData%>% filter(str_detect(postal_code, "^[0-9]{1,5}"))

#Distribution of reviews across top 10 postal code
#ggplot(postal_code_top, aes(x=postal_code)) + geom_bar(width = 0.4, color="darkblue", fill="lightblue") + xlab("Postal Code") + ylab("No of Reviews")

#Question 1.a: Relation of star ratings to business stars

# to find relation between business stars and star ratings
relation <- as.data.frame(resReviewsData%>%group_by(business_id,starsBusiness)%>% summarize(starsReviewsAverage=round(mean(starsReview),1)))
# Basic scatterplot
plot(relation$starsBusiness, relation$starsReviewsAverage,
     col="#69b3a2",
     xlab="business rating", ylab="avg review ratings",
     main="Relation bt Business rating and Avg reviews' ratings"
)

ratings <- list()
i<-1 #initialising the iterator
for(r in c(1.5,2,2.5,3,3.5,4,4.5,5)){
  tbl_=resReviewsData[resReviewsData$starsBusiness==r,]
  ratings[[i]]<-ggplot(tbl_, aes(x=starsReview))+geom_bar()+ggtitle(paste("starsBusiness:",r))
  i<-i+1
}
do.call(grid.arrange,ratings)


#Question 1.b
#positive and negative sentiment
# star ratings for reviews counts
ggplot(data.frame(resReviewsData$starsReview), aes(x=resReviewsData$starsReview)) +
  geom_bar()

resReviewsData$sentiment<-ifelse(resReviewsData$starsReview >= 3,"Positive","Negative") #didn't consider rating 3 as positive or negative
x <- resReviewsData%>%group_by(sentiment)%>%tally() %>% as.data.frame()
barplot(x$n,names.arg=x$sentiment,xlab="sentiment",ylab="count",col="blue",
        main="sentiment count", ylim = c(0, 37000))

#tokenize the text of the reviews in the column named 'text'
rrTokens <- resReviewsData%>% unnest_tokens(word, text)
rrTokens$word <- tolower(rrTokens$word)
rrTokens <- rrTokens %>% anti_join(stop_words)

#positive words
words_pos <- as.data.frame(rrTokens %>% filter(sentiment == 'Positive') %>%
                               group_by(word) %>% 
                               summarize(count_of_words = n())) 
words_pos[order(words_pos$count_of_words, decreasing = TRUE), ]%>% view() #in decreasing order of number of repititions of each word

#negatoive words
words_neg <- as.data.frame(rrTokens %>% filter(sentiment == 'Negative') %>%
                             group_by(word) %>% 
                             summarize(count_of_words = n())) 
words_neg[order(words_neg$count_of_words, decreasing = TRUE), ]%>% view() #in decreasing order of number of repititions of each word


#Stemming and Lemmatization
rrTokens_lemm<-rrTokens %>%  mutate(word_lemma = textstem::lemmatize_words(word))

#tokenize, remove stopwords, and lemmatize (or you can use stemmed words instead of lemmatization)
rrTokens<-rrTokens %>%  mutate(word = textstem::lemmatize_words(word))

#We may want to filter out words with less than 3 characters and those with more than 15 characters
rrTokens<-rrTokens %>% filter(str_length(word)<=3 | str_length(word)<=15)
rrTokens<- rrTokens %>% group_by(review_id, starsReview) %>% count(word)

#count total number of words by review, and add this in a column
totWords<-rrTokens  %>% group_by(review_id) %>%  count(word, sort=TRUE) %>% summarise(total=sum(n))
num<-left_join(rrTokens, totWords)
# now n/total gives the tf values
num<-num %>% mutate(tf=n/total)
head(num)

#We can use the bind_tfidf function to calculate the tf, idf and tfidf values
rrTokens<-rrTokens %>% bind_tf_idf(word, review_id, n)
head(rrTokens)
dfToken <- rrTokens

###############
# Question: 3.a Matching Terms for each Dictionary
###############
rrSenti_bing1 <- rrTokens %>% inner_join(get_sentiments("bing"), by="word") 
rrSenti_nrc1<- rrTokens %>% inner_join(get_sentiments("nrc"), by="word") 
rrSenti_affin1<- rrTokens %>% inner_join(get_sentiments("afinn"), by="word") 

binn<- nrow(rrSenti_bing1)
nrcn <- nrow(rrSenti_nrc1)
afinn <- nrow(rrSenti_affin1)

table_terms <- matrix(c(binn,nrcn,afinn),ncol=3,byrow=TRUE)
colnames(table_terms) <- c("Bing","NRC","AFINN")
rownames(table_terms) <- c("Number of terms")
table_terms <- as.table(table_terms)%>%view()
barplot(table_terms$Freq,names.arg=table_terms$Var2,xlab="dictionary",ylab="matched words",col="blue",
        main="Matching count: Dictionary-wise", ylim = c(0, 1000000))
##############
# Question 3.b Overlap in matching terms between the different dictionaries
##############
rrSenti_bing1_semi <- rrTokens %>% semi_join(get_sentiments("bing"), by="word") 
rrSenti_nrc1_semi<- rrTokens %>% semi_join(get_sentiments("nrc"), by="word") 
rrSenti_affin1_semi<- rrTokens %>% semi_join(get_sentiments("afinn"), by="word") 

binn1<- nrow(rrSenti_bing1_semi)
nrcn1 <- nrow(rrSenti_nrc1_semi)
affn1 <- nrow(rrSenti_affin1_semi)

table_terms_semi <- matrix(c(binn1,nrcn1,affn1),ncol=3,byrow=TRUE)
colnames(table_terms_semi) <- c("Bing","NRC","AFFIN")
rownames(table_terms_semi) <- c("Number of terms")
table_terms <- as.table(table_terms_semi)%>%view()
barplot(table_terms$Freq,names.arg=table_terms$Var2,xlab="dictionary",ylab="overlapped words",col="blue",
        main="Overlapping count: Dictionary-wise", ylim = c(0, 500000))

##############################
# Question 3.c and Question 4 
##############################

#-------------------- BING ------------------------------------#

# retain only the words which match the sentiment dictionary, do an inner-join
dfSenti_bing<- dfToken %>% inner_join( get_sentiments("bing"), by="word")

#count the occurrences of positive/negative sentiment words in the reviews
num_bing <-dfSenti_bing %>% group_by(word, sentiment) %>% summarise(totOcc=sum(n)) %>% arrange(sentiment, desc(totOcc))

#negate the counts for the negative sentiment words and finding most positive and most negative words in reviews
num_bing<- num_bing %>% mutate (totOcc=ifelse(sentiment=="positive", totOcc, -totOcc))
num_bing<-ungroup(num_bing)
num_bing %>% top_n(25)
num_bing %>% top_n(-25)

num_bing

#Plot with reordering of words
rbind(top_n(num_bing, 20), top_n(num_bing, -20)) %>% mutate(word=reorder(word,totOcc)) %>% ggplot(aes(word, totOcc, fill=sentiment)) + geom_col()+coord_flip() + labs(title = "Top 20 postive and 20 negative words with Bing")

#How many words for the different sentiment categories
num_bing %>% group_by(sentiment) %>% summarise(count=n(), sumn=sum(totOcc))

#top few words for different sentiments
num_bing %>% group_by(sentiment) %>% arrange(sentiment, desc(totOcc)) %>% top_n(10) %>% view()

##################
# Predict sentiment using Bing
##################

# let's look into sentiment by review and see how that relates to review's star ratings
dfSenti_bing<- dfToken %>% inner_join(get_sentiments("bing"), by="word")
view(dfSenti_bing)

#summarise positive/negative sentiment words per review
dfrevSenti_bing <- dfSenti_bing %>% group_by(review_id, starsReview) %>% summarise( nwords = n(), posSum = sum( sentiment == 'positive' ) , negSum = sum( sentiment == 'negative'))

#calculate sentiment score based on proportion of positive, negative words
dfrevSenti_bing <- dfrevSenti_bing %>% mutate( posProp = posSum/nwords, negProp = negSum/nwords) 
dfrevSenti_bing <- dfrevSenti_bing %>% mutate( sentiScore = posProp-negProp)

#Do review star ratings correspond to the positive/negative sentiment words
dfrevSenti_bing %>% group_by(starsReview) %>%   summarise(avgPos=mean(posProp), avgNeg=mean(negProp), avgSentiSc=mean(sentiScore))

#we can consider reviews with 1 to 2 stars as negative, and this with 4 to 5 stars as positive
dfrevSenti_bing <- dfrevSenti_bing %>% mutate(hiLo=ifelse(starsReview<=2,-1, ifelse(starsReview>=4, 1, 0 )))
dfrevSenti_bing <- dfrevSenti_bing %>% mutate(pred_hiLo=ifelse(sentiScore >0, 1, -1)) 


dfrevSenti_bing
#filter out the reviews with 3 stars, and get the confusion matrix for hiLo vs pred_hiLo
num_bing
num_bing<-dfrevSenti_bing %>% filter(hiLo!=0)
table(actual=num_bing$hiLo, predicted=num_bing$pred_hiLo )

#------------------------------------- NRC -----------------------------------------------------#

# retain only the words which match the sentiment dictionary, do an inner-join
dfSenti_nrc<- dfToken %>% inner_join( get_sentiments("nrc"), by="word")

#count the occurrences of positive/negative sentiment words in the reviews
num_nrc <-dfSenti_nrc %>% group_by(word, sentiment) %>% summarise(totOcc=sum(n)) %>% arrange(sentiment, desc(totOcc))
view(num_nrc)
num_nrc %>% distinct(word) %>% dim()

#How many words for the different sentiment categories
num_nrc %>% group_by(sentiment) %>% summarise(count=n(), sumn=sum(totOcc))

#top few words for different sentiments
num_nrc %>% group_by(sentiment) %>% arrange(sentiment, desc(totOcc)) %>% top_n(10) %>% view()

num_nrc<-num_nrc %>% mutate(goodBad=ifelse(sentiment %in% c('anger', 'disgust', 'fear', 'sadness', 'negative'), -totOcc, ifelse(sentiment %in% c('positive', 'joy', 'anticipation', 'trust'), totOcc, 0)))

num_nrc %>% mutate (positive=ifelse(goodBad<0, 0, 1)) %>% group_by(positive) %>% summarise(count=n())
num_nrc %>% distinct(word) %>% dim()

num_nrc<-ungroup(num_nrc)
top_n(num_nrc, -20)
top_n(num_nrc, 20)


#Plot with reordering of words
rbind(top_n(num_nrc,30), top_n(num_nrc, -30)) %>% mutate(word=reorder(word,totOcc)) %>% 
  ggplot(aes(word, totOcc, fill=sentiment)) + geom_col()+coord_flip() + labs(title = "Words - Occurence in each sentiment - NRC")


##########################
# Predicting the sentiment using NRC 
##########################
dfSenti_nrc <-dfToken %>% inner_join(get_sentiments("nrc"), by="word") %>% group_by (review_id, starsReview, word, tf, idf, tf_idf, sentiment) %>% summarise(totOcc=sum(n)) %>% arrange(sentiment, desc(totOcc))

unique(dfSenti_nrc$sentiment)
dfSenti_nrc <- dfSenti_nrc %>% mutate(goodBad=ifelse(sentiment %in% c('anger', 'disgust', 'fear', 'sadness', 'negative'), 
                                                     -totOcc, ifelse(sentiment %in% c('positive', 'joy', 'anticipation', 'trust','surprise'), totOcc, 0)))
dfSenti_nrc
dfrevSenti_nrc <- dfSenti_nrc %>% group_by(review_id, starsReview) %>% summarise(nwords=n(), sentiSum =sum(goodBad))
dfrevSenti_nrc %>% group_by(starsReview) %>% summarise(avgLen=mean(nwords), avgSenti=mean(sentiSum))

#we can consider reviews with 1 to 2 stars as negative, and this with 4 to 5 stars as positive
dfrevSenti_nrc <- dfrevSenti_nrc %>% mutate(hiLo=ifelse(starsReview<=2,-1, ifelse(starsReview>=4, 1, 0 )))
dfrevSenti_nrc <- dfrevSenti_nrc %>% mutate(pred_hiLo=ifelse(sentiSum >0, 1, -1)) 

#filter out the reviews with 3 stars, and get the confusion matrix for hiLo vs pred_hiLo
num_nrc<-dfrevSenti_nrc %>% filter(hiLo!=0)
table(actual=num_nrc$hiLo, predicted=num_nrc$pred_hiLo )

#-------------------------------------- AFINN ---------------------------------------#

dfSenti_afinn<- dfToken %>% inner_join(get_sentiments("afinn"), by="word")
dfSenti_afinn
dfSenti_afinn %>% group_by(word) %>% summarise(avgvalue=mean(value)) %>% mutate(positive =ifelse(avgvalue>0,1,0)) %>% group_by(positive) %>% summarise(count=n())

dfrevSenti_afinn <- dfSenti_afinn %>% group_by(review_id, starsReview) %>% summarise(nwords=n(), sentiSum =sum(value))
dfrevSenti_afinn %>% group_by(starsReview) %>% summarise(avgLen=mean(nwords), avgSenti=mean(sentiSum))


#considering reviews with 1 to 2 starsReview as negative, and this with 4 to 5 stars as positive
dfrevSenti_afinn <- dfrevSenti_afinn %>% mutate(hiLo = ifelse(starsReview <= 2, -1, ifelse(starsReview >=4, 1, 0 )))
dfrevSenti_afinn <- dfrevSenti_afinn %>% mutate(pred_hiLo=ifelse(sentiSum > 0, 1, -1))

#filter out the reviews with 3 stars, and get the confusion matrix for hiLo vs pred_hiLo
num_afinn<-dfrevSenti_afinn %>% filter(hiLo!=0)
table(actual=num_afinn$hiLo, predicted=num_afinn$pred_hiLo )

#ROC Curves

plot.roc(roc(num_bing$hiLo, num_bing$pred_hiLo, levels=c(-1, 1)),col='green', legacy.axes = TRUE)
plot.roc(roc(num_nrc$hiLo, num_nrc$pred_hiLo, levels=c(-1, 1)),col='blue', add = TRUE)
plot.roc(roc(num_afinn$hiLo, num_afinn$pred_hiLo, levels=c(-1, 1)),col='red', add = TRUE)
legend("bottomright", legend=c("Bing","NRC", "AFINN"),col=c("green", "blue", "red"), lwd=4, title = "ROC Curves for each dictionary", labels(x='FPR', y='TPR'))

#AUC Values

auc(as.numeric(num_bing$hiLo), num_bing$pred_hiLo, levels=c(-1, 1))
auc(as.numeric(num_nrc$hiLo), num_nrc$pred_hiLo, levels=c(-1, 1))
auc(as.numeric(num_afinn$hiLo), num_afinn$pred_hiLo, levels=c(-1, 1))

#--------------------------- MODELS -------------------------------------------------------#
##############
# Question 5
#############

################
# RANDOM FOREST
################

# Learn a model to predict hiLo ratings, from words in reviews
#Create document-term matrices
revDTM_sentiBing <- dfSenti_bing %>%  pivot_wider(id_cols = c(review_id,starsReview), names_from = word, values_from = tf_idf, values_fn = mean)  %>% ungroup()
revDTM_sentiNRC <- dfSenti_nrc %>%  pivot_wider(id_cols = c(review_id,starsReview), names_from = word, values_from = tf_idf,values_fn = mean)  %>% ungroup()
revDTM_sentiAFINN <- dfSenti_afinn %>%  pivot_wider(id_cols = c(review_id,starsReview), names_from = word, values_from = tf_idf)  %>% ungroup()


#filter out the reviews with starsReview=3, and calculate hiLo sentiment 'class'
revDTM_sentiBing <- revDTM_sentiBing %>% filter(starsReview!=3) %>% mutate(hiLo=ifelse(starsReview<=2, -1, 1)) %>% select(-starsReview)
revDTM_sentiNRC <- revDTM_sentiNRC %>% filter(starsReview!=3) %>% mutate(hiLo=ifelse(starsReview<=2, -1, 1)) %>% select(-starsReview)
revDTM_sentiAFINN <- revDTM_sentiAFINN %>% filter(starsReview!=3) %>% mutate(hiLo=ifelse(starsReview<=2, -1, 1)) %>% select(-starsReview)

#how many review with 1, -1  'class'
revDTM_sentiBing %>% group_by(hiLo) %>% tally()
revDTM_sentiNRC %>% group_by(hiLo) %>% tally()
revDTM_sentiAFINN %>% group_by(hiLo) %>% tally()


#-----develop a random forest model to predict hiLo from the words in the reviews----#


library(ranger)

#replace all the NAs with 0
revDTM_sentiBing<-revDTM_sentiBing %>% replace(., is.na(.), 0)
revDTM_sentiBing$hiLo<- as.factor(revDTM_sentiBing$hiLo)


revDTM_sentiNRC<-revDTM_sentiNRC %>% replace(., is.na(.), 0)
revDTM_sentiNRC<-revDTM_sentiNRC %>% replace(., is.null(.), 0)
revDTM_sentiNRC$hiLo<- as.factor(revDTM_sentiNRC$hiLo)

revDTM_sentiAFINN<-revDTM_sentiAFINN %>% replace(., is.na(.), 0)
revDTM_sentiAFINN$hiLo<- as.factor(revDTM_sentiAFINN$hiLo)

library(rsample)

revDTM_sentiBing_split<- initial_split(revDTM_sentiBing, 0.5)
revDTM_sentiBing_trn<- training(revDTM_sentiBing_split)
revDTM_sentiBing_tst<- testing(revDTM_sentiBing_split)

revDTM_sentiNRC_split<- initial_split(revDTM_sentiNRC, 0.5)
revDTM_sentiNRC_trn<- training(revDTM_sentiNRC_split)
revDTM_sentiNRC_tst<- testing(revDTM_sentiNRC_split)

revDTM_sentiAFINN_split<- initial_split(revDTM_sentiAFINN, 0.5)
revDTM_sentiAFINN_trn<- training(revDTM_sentiAFINN_split)
revDTM_sentiAFINN_tst<- testing(revDTM_sentiAFINN_split)


rfModelBing<-ranger(dependent.variable.name = "hiLo", data=revDTM_sentiBing_trn %>% select(-review_id), num.trees = 500, importance='permutation', probability = TRUE, max.depth = 12)
rfModelNRC<-ranger(dependent.variable.name = "hiLo", data=revDTM_sentiNRC_trn %>% select(-review_id), num.trees = 500, importance='permutation', probability = TRUE, max.depth = 12)
rfModelAFINN<-ranger(dependent.variable.name = "hiLo", data=revDTM_sentiAFINN_trn %>% select(-review_id), num.trees = 500, importance='permutation', probability = TRUE, max.depth = 12)

#Finding Importance
importance(rfModelBing) %>% view()
importance(rfModelNRC) %>% view()
importance(rfModelAFINN) %>% view()

#Obtaining predictions

#Bing

revSentiBing_predTrn<- predict(rfModelBing, revDTM_sentiBing_trn %>% select(-review_id))$predictions
revSentiBing_predTst<- predict(rfModelBing, revDTM_sentiBing_tst %>% select(-review_id))$predictions

#NRC

revSentiNRC_predTrn<- predict(rfModelNRC, revDTM_sentiNRC_trn %>% select(-review_id))$predictions
revSentiNRC_predTst<- predict(rfModelNRC, revDTM_sentiNRC_tst %>% select(-review_id))$predictions

#AFINN

revSentiAFINN_predTrn<- predict(rfModelAFINN, revDTM_sentiAFINN_trn %>% select(-review_id))$predictions
revSentiAFINN_predTst<- predict(rfModelAFINN, revDTM_sentiAFINN_tst %>% select(-review_id))$predictions


#Confusion Matrix

#Bing

table(actual=revDTM_sentiBing_trn$hiLo, preds=revSentiBing_predTrn[,2]>0.5)
table(actual=revDTM_sentiBing_tst$hiLo, preds=revSentiBing_predTst[,2]>0.5)

#NRC

table(actual=revDTM_sentiNRC_trn$hiLo, preds=revSentiNRC_predTrn[,2]>0.5)
table(actual=revDTM_sentiNRC_tst$hiLo, preds=revSentiNRC_predTst[,2]>0.5)

#AFINN

table(actual=revDTM_sentiAFINN_trn$hiLo, preds=revSentiAFINN_predTrn[,2]>0.5)
table(actual=revDTM_sentiAFINN_tst$hiLo, preds=revSentiAFINN_predTst[,2]>0.5)


######Finding Best Threshold


#Bing

rocTrnBing <- roc(revDTM_sentiBing_trn$hiLo, revSentiBing_predTrn[,2], levels=c(-1, 1))
rocTstBing <- roc(revDTM_sentiBing_tst$hiLo, revSentiBing_predTst[,2], levels=c(-1, 1))

plot.roc(rocTrnBing, col='blue', legacy.axes = TRUE)
plot.roc(rocTstBing, col='red', add=TRUE)
legend("right", legend=c("Training", "Test"), col=c("blue", "red"), lwd=2, cex=0.8, bty='n', title = "Bing Dictionary")

#NRC

rocTrnNRC <- roc(revDTM_sentiNRC_trn$hiLo, revSentiNRC_predTrn[,2], levels=c(-1, 1))
rocTstNRC <- roc(revDTM_sentiNRC_tst$hiLo, revSentiNRC_predTst[,2], levels=c(-1, 1))

plot.roc(rocTrnNRC, col='blue', legacy.axes = TRUE)
plot.roc(rocTstNRC, col='red', add=TRUE)
legend("right", legend=c("Training", "Test"), col=c("blue", "red"), lwd=2, cex=0.8, bty='n', title = "NRC Dictionary")

#AFINN

rocTrnAFINN <- roc(revDTM_sentiAFINN_trn$hiLo, revSentiAFINN_predTrn[,2], levels=c(-1, 1))
rocTstAFINN <- roc(revDTM_sentiAFINN_tst$hiLo, revSentiAFINN_predTst[,2], levels=c(-1, 1))

plot.roc(rocTrnAFINN, col='blue', legacy.axes = TRUE)
plot.roc(rocTstAFINN, col='red', add=TRUE)
legend("right", legend=c("Training", "Test"), col=c("blue", "red"), lwd=2, cex=0.8, bty='n', title = "AFINN Dictionary")


#Best threshold from ROC analyses

bThrBing<-coords(rocTrnBing, "best", ret="threshold", transpose = FALSE)
as.numeric(as.character((bThrBing)))

bThrNRC<-coords(rocTrnNRC, "best", ret="threshold", transpose = FALSE)
as.numeric(as.character((bThrNRC)))

bThrAFINN<-coords(rocTrnAFINN, "best", ret="threshold", transpose = FALSE)
as.numeric(as.character((bThrAFINN)))

#Confusion Matrix

#Bing
table(actual=revDTM_sentiBing_trn$hiLo, preds=revSentiBing_predTrn[,2]>bThrBing[1,1])
table(actual=revDTM_sentiBing_tst$hiLo, preds=revSentiBing_predTst[,2]>bThrBing[1,1])

#NRC
table(actual=revDTM_sentiNRC_trn$hiLo, preds=revSentiNRC_predTrn[,2]>bThrNRC[1,1])
table(actual=revDTM_sentiNRC_tst$hiLo, preds=revSentiNRC_predTst[,2]>bThrNRC[1,1])

#AFINN
table(actual=revDTM_sentiAFINN_trn$hiLo, preds=revSentiAFINN_predTrn[,2]>bThrAFINN[1,1])
table(actual=revDTM_sentiAFINN_tst$hiLo, preds=revSentiAFINN_predTst[,2]>bThrAFINN[1,1])

#AUC
auc(as.numeric(revDTM_sentiBing_trn$hiLo), revSentiBing_predTrn[,2])
auc(as.numeric(revDTM_sentiBing_tst$hiLo), revSentiBing_predTst[,2])

auc(as.numeric(revDTM_sentiNRC_trn$hiLo), revSentiNRC_predTrn[,2])
auc(as.numeric(revDTM_sentiNRC_tst$hiLo), revSentiNRC_predTst[,2])

auc(as.numeric(revDTM_sentiAFINN_trn$hiLo), revSentiAFINN_predTrn[,2])
auc(as.numeric(revDTM_sentiAFINN_tst$hiLo), revSentiAFINN_predTst[,2])

#########################
# Naive Bayes for each dictionary
#########################

nbModelBing<-naiveBayes(hiLo ~ ., data=revDTM_sentiBing_trn %>% select(-review_id))
nbModelNRC<-naiveBayes(hiLo ~ ., data=revDTM_sentiNRC_trn %>% select(-review_id))
nbModelAFINN<-naiveBayes(hiLo ~ ., data=revDTM_sentiAFINN_trn %>% select(-review_id))

#Training/Testing Predictions
revSentiBing_NBpredTrn<-predict(nbModelBing, revDTM_sentiBing_trn, type = "raw")
revSentiBing_NBpredTst<-predict(nbModelBing, revDTM_sentiBing_tst, type = "raw")

revSentiNRC_NBpredTrn<-predict(nbModelNRC, revDTM_sentiNRC_trn, type = "raw")
revSentiNRC_NBpredTst<-predict(nbModelNRC, revDTM_sentiNRC_tst, type = "raw")

revSentiAFINN_NBpredTrn<-predict(nbModelAFINN, revDTM_sentiAFINN_trn, type = "raw")
revSentiAFINN_NBpredTst<-predict(nbModelAFINN, revDTM_sentiAFINN_tst, type = "raw")


#ROC Curve

#Bing
rocTrnNBBing <- roc(revDTM_sentiBing_trn$hiLo, revSentiBing_NBpredTrn[,2], levels=c(-1, 1))
rocTstNBBing <- roc(revDTM_sentiBing_tst$hiLo, revSentiBing_NBpredTst[,2], levels=c(-1, 1))

plot.roc(rocTrnNBBing, col='blue', legacy.axes = TRUE)
plot.roc(rocTstNBBing, col='red', add=TRUE)
legend("right", legend=c("Training", "Test"), col=c("blue", "red"), lwd=2, cex=0.8, bty='n', title = "Bing Dictionary")

#NRC

rocTrnNBNRC <- roc(revDTM_sentiNRC_trn$hiLo, revSentiNRC_NBpredTrn[,2], levels=c(-1, 1))
rocTstNBNRC <- roc(revDTM_sentiNRC_tst$hiLo, revSentiNRC_NBpredTst[,2], levels=c(-1, 1))

plot.roc(rocTrnNBNRC, col='blue', legacy.axes = TRUE)
plot.roc(rocTstNBNRC, col='red', add=TRUE)
legend("right", legend=c("Training", "Test"), col=c("blue", "red"), lwd=2, cex=0.8, bty='n', title = "NRC Dictionary")

#AFINN

rocTrnNBAFINN <- roc(revDTM_sentiAFINN_trn$hiLo, revSentiAFINN_NBpredTrn[,2], levels=c(-1, 1))
rocTstNBAFINN <- roc(revDTM_sentiAFINN_tst$hiLo, revSentiAFINN_NBpredTst[,2], levels=c(-1, 1))

plot.roc(rocTrnNBAFINN, col='blue', legacy.axes = TRUE)
plot.roc(rocTstNBAFINN, col='red', add=TRUE)
legend("right", legend=c("Training", "Test"), col=c("blue", "red"), lwd=2, cex=0.8, bty='n', title = "AFINN Dictionary")


#AUC

auc(as.numeric(revDTM_sentiBing_trn$hiLo), revSentiBing_NBpredTrn[,2])
auc(as.numeric(revDTM_sentiBing_tst$hiLo), revSentiBing_NBpredTst[,2])

auc(as.numeric(revDTM_sentiNRC_trn$hiLo), revSentiNRC_NBpredTrn[,2])
auc(as.numeric(revDTM_sentiNRC_tst$hiLo), revSentiNRC_NBpredTst[,2])

auc(as.numeric(revDTM_sentiAFINN_trn$hiLo), revSentiAFINN_NBpredTrn[,2])
auc(as.numeric(revDTM_sentiAFINN_tst$hiLo), revSentiAFINN_NBpredTst[,2])

#Confusion Matrix

#Bing
table(actual=revDTM_sentiBing_trn$hiLo, preds=revSentiBing_NBpredTrn[,2]>0.5)
table(actual=revDTM_sentiBing_tst$hiLo, preds=revSentiBing_NBpredTst[,2]>0.5)

#NRC
table(actual=revDTM_sentiNRC_trn$hiLo, preds=revSentiNRC_NBpredTrn[,2]>0.5)
table(actual=revDTM_sentiNRC_tst$hiLo, preds=revSentiNRC_NBpredTst[,2]>0.5)

#AFINN
table(actual=revDTM_sentiAFINN_trn$hiLo, preds=revSentiAFINN_NBpredTrn[,2]>0.5)
table(actual=revDTM_sentiAFINN_tst$hiLo, preds=revSentiAFINN_NBpredTst[,2]>0.5)

###################################
#      SVM Classification - Bing
###################################

dim(revDTM_sentiBing_trn)
dim(revDTM_sentiBing_tst)
revDTM_sentiBing_trn_2=revDTM_sentiBing_trn[1:10000,]
revDTM_sentiBing_tst_2=revDTM_sentiBing_tst[1:10000,]

#Parameter Tuning
system.time(svm_tuneBing <-tune(svm, as.factor(hiLo) ~., data = revDTM_sentiBing_trn_2 %>% select(-review_id), kernel="radial", scale=FALSE, ranges = list( cost=c(0.1,1,10), gamma = c(0.5,1,5))))

#Best model
svm_tuneBing$best.parameters
svm_tuneBing$best.model

#develop a SVM model on the sentiment dictionary terms
svmBing <- svm(as.factor(hiLo) ~., data = revDTM_sentiBing_trn_2 %>% select(-review_id),
               kernel="radial", cost=10, gamma=0.5, scale=FALSE)

#Training/Testing Predictions
revDTM_predTrn_svmBing<-predict(svmBing, revDTM_sentiBing_trn_2)
revDTM_predTst_svmBing<-predict(svmBing, revDTM_sentiBing_tst_2)

#ROC Curve

#Bing
rocTrnSVMBing <- roc(revDTM_sentiBing_trn_2$hiLo,as.numeric(revDTM_predTrn_svmBing),levels=c(-1, 1))
rocTstSVMBing <- roc(revDTM_sentiBing_tst_2$hiLo,as.numeric(revDTM_predTst_svmBing),levels=c(-1, 1))

plot.roc(rocTrnSVMBing, col='blue', legacy.axes = TRUE)
plot.roc(rocTstSVMBing, col='red', add=TRUE)
legend("right", legend=c("Training", "Test"), col=c("blue", "red"), lwd=2, cex=0.8, bty='n', title = "Bing Dictionary")

#AUC
auc(as.numeric(revDTM_sentiBing_trn_2$hiLo), as.numeric(revDTM_predTrn_svmBing))
auc(as.numeric(revDTM_sentiBing_tst_2$hiLo), as.numeric(revDTM_predTst_svmBing))

#Confusion Matrix

#Bing
table(actual= revDTM_sentiBing_trn_2$hiLo, predicted= revDTM_predTrn_svmBing)
table(actual= revDTM_sentiBing_tst_2$hiLo, predicted= revDTM_predTst_svmBing)

###################################
#      SVM Classification - NRC
###################################

dim(revDTM_sentiNRC_trn)
dim(revDTM_sentiNRC_tst)
revDTM_sentiNRC_trn_2=revDTM_sentiNRC_trn[1:10000,]
revDTM_sentiNRC_tst_2=revDTM_sentiNRC_tst[1:10000,]

#Parameter Tuning
system.time(svm_tuneNRC <-tune(svm, as.factor(hiLo) ~., data = revDTM_sentiNRC_trn_2 %>% select(-review_id),kernel="radial", scale=FALSE, ranges = list( cost=c(0.1,1,10), gamma = c(0.5,1,5))))

#Best model
svm_tuneNRC$best.parameters
svm_tuneNRC$best.model

#develop a SVM model on the sentiment dictionary terms

svmNRC <- svm(as.factor(hiLo) ~., data = revDTM_sentiNRC_trn_2 %>% select(-review_id),
              kernel="radial", cost=10, gamma=0.5, scale=FALSE)

#Training/Testing Predictions
revDTM_predTrn_svmNRC<-predict(svmNRC, revDTM_sentiNRC_trn_2)
revDTM_predTst_svmNRC<-predict(svmNRC, revDTM_sentiNRC_tst_2)

#ROC Curve
#NRC
rocTrnSVMNRC <- roc(revDTM_sentiNRC_trn_2$hiLo, as.numeric(revDTM_predTrn_svmNRC),levels=c(-1, 1))
rocTstSVMNRC <- roc(revDTM_sentiNRC_tst_2$hiLo, as.numeric(revDTM_predTst_svmNRC),levels=c(-1, 1))

plot.roc(rocTrnSVMNRC, col='blue', legacy.axes = TRUE)
plot.roc(rocTstSVMNRC, col='red', add=TRUE)
legend("right", legend=c("Training", "Test"), col=c("blue", "red"), lwd=2, cex=0.8, bty='n', title = "NRC Dictionary")

#AUC
auc(as.numeric(revDTM_sentiNRC_trn_2$hiLo), as.numeric(revDTM_predTrn_svmNRC))
auc(as.numeric(revDTM_sentiNRC_tst_2$hiLo), as.numeric(revDTM_predTst_svmNRC))


#Confusion Matrix
#NRC
table(actual= revDTM_sentiNRC_trn_2$hiLo, predicted= revDTM_predTrn_svmNRC)
table(actual= revDTM_sentiNRC_tst_2$hiLo, predicted= revDTM_predTst_svmNRC)


###################################
#      SVM Classification - AFINN
###################################

dim(revDTM_sentiAFINN_trn)
dim(revDTM_sentiAFINN_tst)
revDTM_sentiAFINN_trn_2=revDTM_sentiAFINN_trn[1:10000,]
revDTM_sentiAFINN_tst_2=revDTM_sentiAFINN_tst[1:10000,]

#Parameter Tuning
system.time(svm_tuneAFINN <-tune(svm, as.factor(hiLo) ~., data = revDTM_sentiAFINN_trn_2 %>% select(-review_id), kernel="radial", scale=FALSE, ranges = list( cost=c(0.1,1,10), gamma = c(0.5,1,5))))

#Best model
svm_tuneAFINN$best.parameters
svm_tuneAFINN$best.model

#develop a SVM model on the sentiment dictionary terms
svmMAFINN <- svm(as.factor(hiLo) ~., data = revDTM_sentiAFINN_trn_2 %>% select(-review_id),
                 kernel="radial", cost=10, gamma=1, scale=FALSE)

#Training/Testing Predictions
revDTM_predTrn_svmAFINN<-predict(svmMAFINN, revDTM_sentiAFINN_trn_2)
revDTM_predTst_svmAFINN<-predict(svmMAFINN, revDTM_sentiAFINN_tst_2)

#ROC Curve
#AFINN
rocTrnSVMAFINN <- roc(revDTM_sentiAFINN_trn_2$hiLo, as.numeric(revDTM_predTrn_svmAFINN),levels=c(-1, 1))
rocTstSVMAFINN <- roc(revDTM_sentiAFINN_tst_2$hiLo, as.numeric(revDTM_predTst_svmAFINN),levels=c(-1, 1))

plot.roc(rocTrnSVMAFINN, col='blue', legacy.axes = TRUE)
plot.roc(rocTstSVMAFINN, col='red', add=TRUE)
legend("right", legend=c("Training", "Test"), col=c("blue", "red"), lwd=2, cex=0.8, bty='n', title = "AFINN Dictionary")

#AUC
auc(as.numeric(revDTM_sentiAFINN_trn_2$hiLo), as.numeric(revDTM_predTrn_svmAFINN))
auc(as.numeric(revDTM_sentiAFINN_tst_2$hiLo), as.numeric(revDTM_predTst_svmAFINN))


#Confusion Matrix
#AFINN
table(actual= revDTM_sentiAFINN_trn_2$hiLo, predicted= revDTM_predTrn_svmAFINN)
table(actual= revDTM_sentiAFINN_tst_2$hiLo, predicted= revDTM_predTst_svmAFINN)

######################################
# Random Forest with Combined Dictionary
######################################

revDTM_sentiCOM <- merge(revDTM_sentiBing, revDTM_sentiNRC)
revDTM_sentiCOM <- merge(revDTM_sentiCOM, revDTM_sentiAFINN)

#Training/Testing Split
revDTM_sentiCOM_split<- initial_split(revDTM_sentiCOM, 0.5)
revDTM_sentiCOM_trn<- training(revDTM_sentiCOM_split)
revDTM_sentiCOM_tst<- testing(revDTM_sentiCOM_split)

revDTM_sentiCOM_trn=revDTM_sentiCOM_trn[1:10000,]
revDTM_sentiCOM_tst=revDTM_sentiCOM_tst[1:10000,]

#Random Forest
rfModelCOM<-ranger(dependent.variable.name = "hiLo", data=revDTM_sentiCOM_trn %>% select(-review_id), num.trees = 400, importance='permutation', probability = TRUE, max.depth=15)
rfModelCOM

#Obtain predictions
revSentiCOM_predTrn<- predict(rfModelCOM, revDTM_sentiCOM_trn %>% select(-review_id))$predictions
revSentiCOM_predTst<- predict(rfModelCOM, revDTM_sentiCOM_tst %>% select(-review_id))$predictions

#ROC Curve
rocTrnCOM <- roc(revDTM_sentiCOM_trn$hiLo, revSentiCOM_predTrn[,2], levels=c(-1, 1))
rocTstCOM <- roc(revDTM_sentiCOM_tst$hiLo, revSentiCOM_predTst[,2], levels=c(-1, 1))

plot.roc(rocTrnCOM, col='blue', legacy.axes = TRUE)
plot.roc(rocTstCOM, col='red', add=TRUE)
legend("right", legend=c("Training", "Test"), col=c("blue", "red"), lwd=2, cex=0.8, bty='n', title = 'Random Forest: Combined')

#Best threshold from ROC analyses
bThrCOM<-coords(rocTrnCOM, "best", ret="threshold", transpose = FALSE)
as.numeric(as.character((bThrCOM)))

#Confusion Matrix
table(actual=revDTM_sentiCOM_trn$hiLo, preds=revSentiCOM_predTrn[,2]>bThrCOM[1,1])
table(actual=revDTM_sentiCOM_tst$hiLo, preds=revSentiCOM_predTst[,2]>bThrCOM[1,1])

#AUC
auc(as.numeric(revDTM_sentiCOM_trn$hiLo), revSentiCOM_predTrn[,2])
auc(as.numeric(revDTM_sentiCOM_tst$hiLo), revSentiCOM_predTst[,2])

######################################
# Naive-Bayes with Combined Dictionary
######################################

nbModelCOM<-naiveBayes(hiLo ~ ., data=revDTM_sentiCOM_trn %>% select(-review_id))

#Training/Testing Predictions
revSentiCOM_NBpredTrn<-predict(nbModelCOM, revDTM_sentiCOM_trn, type = "raw")
revSentiCOM_NBpredTst<-predict(nbModelCOM, revDTM_sentiCOM_tst, type = "raw")

#ROC Curve
rocTrnNBCOM <- roc(revDTM_sentiCOM_trn$hiLo, revSentiCOM_NBpredTrn[,2], levels=c(-1, 1))
rocTstNBCOM <- roc(revDTM_sentiCOM_tst$hiLo, revSentiCOM_NBpredTst[,2], levels=c(-1, 1))

plot.roc(rocTrnNBCOM, col='blue', legacy.axes = TRUE)
plot.roc(rocTstNBCOM, col='red', add=TRUE)
legend("right", legend=c("Training", "Test"), col=c("blue", "red"), lwd=2, cex=0.8, bty='n',title = 'Naive Bias: Combined')

#AUC
auc(as.numeric(revDTM_sentiCOM_trn$hiLo), revSentiCOM_NBpredTrn[,2])
auc(as.numeric(revDTM_sentiCOM_tst$hiLo), revSentiCOM_NBpredTst[,2])

#Confusion Matrix
table(actual=revDTM_sentiCOM_trn$hiLo, preds=revSentiCOM_NBpredTrn[,2]>0.5)
table(actual=revDTM_sentiCOM_tst$hiLo, preds=revSentiCOM_NBpredTst[,2]>0.5)

#####################################################
# SVM Classification with Combined Dictionary
#####################################################

#Parameter Tuning
system.time(svm_tuneCOM <-tune(svm, as.factor(hiLo) ~., data = revDTM_sentiCOM_trn %>% select(-review_id),
                               kernel="radial", scale=FALSE, ranges = list( cost=c(0.1,1,10), gamma = c(0.5,1,5))))

#Best model
svm_tuneCOM$best.parameters
svm_tuneCOM$best.model

#develop a SVM model on the sentiment dictionary terms
svmMCOM <- svm(as.factor(hiLo) ~., data = revDTM_sentiCOM_trn %>% select(-review_id),
               kernel="radial", cost=10, gamma=0.5, scale=FALSE)

#Training/Testing Predictions
revDTM_predTrn_svmCOM<-predict(svmMCOM, revDTM_sentiCOM_trn)
revDTM_predTst_svmCOM<-predict(svmMCOM, revDTM_sentiCOM_tst)

#ROC Curve
rocTrnSVMCOM <- roc(revDTM_sentiCOM_trn$hiLo, as.numeric(revDTM_predTrn_svmCOM),levels=c(-1, 1))
rocTstSVMCOM <- roc(revDTM_sentiCOM_tst$hiLo, as.numeric(revDTM_predTst_svmCOM),levels=c(-1, 1))

plot.roc(rocTrnSVMCOM, col='blue', legacy.axes = TRUE)
plot.roc(rocTstSVMCOM, col='red', add=TRUE)
legend("right", legend=c("Training", "Test"), col=c("blue", "red"), lwd=2, cex=0.8, bty='n', title = 'SVM: Combined')

#AUC
auc(as.numeric(revDTM_sentiCOM_trn$hiLo), as.numeric(revDTM_predTrn_svmCOM))
auc(as.numeric(revDTM_sentiCOM_tst$hiLo), as.numeric(revDTM_predTst_svmCOM))


#Confusion Matrix
table(actual= revDTM_sentiCOM_trn$hiLo, predicted= revDTM_predTrn_svmCOM)
table(actual= revDTM_sentiCOM_tst$hiLo, predicted= revDTM_predTst_svmCOM)

##############
# QUESTION 6
##############

extractAmbience <- function(q) {
  sub(":.*","", q[which(str_extract(q, "True") == "True")])
}

x<- resReviewsData %>% select(review_id, attributes)
x2<-x %>% mutate (atts = str_split( attributes, '\\|')) %>% unnest(atts)
x3<- x2 %>% cbind( str_split_fixed ( x2$atts, ":", 2))
colnames(x3)[4] <- 'attName'
colnames(x3)[5] <- 'attValue'
colnames(x3)
x3<-x3 %>% select (-c (attributes ,atts))
x3<-x3 %>% filter(str_length(x3$attName) > 0)
x4<- x3 %>% pivot_wider(names_from = attName, values_from = attValue)
dim(x4)
glimpse(x4)

#Deeper look into ‘Ambience’
paste(x4[1,3])
x5 <- x4 %>% mutate( amb = str_split( Ambience, ","))
dim(x4)
dim(x5)
typeof(x5$amb)
x5$amb[1]
x5$amb[1000]


x6<- x5 %>% mutate( amb = lapply( amb, extractAmbience ) ) 

#how many examples by different values for 'Ambience'
x6 %>% group_by(amb) %>% tally() %>% view()

x6 %>% filter( str_detect (amb, 'casual')) %>% count()
x6 %>% filter( str_detect (amb, 'classy')) %>% count()

##################################
#Deeper look into ‘BusinessParking’
paste(x4[1,5])
x5 <- x4 %>% mutate( bsnsPrk = str_split( BusinessParking, ","))
dim(x4)
dim(x5)
typeof(x5$bsnsPrk)
x5$bsnsPrk[1]
x5$bsnsPrk[1000]


x6<- x5 %>% mutate( bsnsPrk = lapply( bsnsPrk, extractAmbience ) ) 

#how many examples by different values for 'Ambience'
x6 %>% group_by(bsnsPrk) %>% tally() %>% view()

x6 %>% filter( str_detect (bsnsPrk, 'lot')) %>% count()
x6 %>% filter( str_detect (bsnsPrk, 'street')) %>% count()

###################################
#Deeper look into ‘GoodForMeal’
paste(x4[1,7])
x5 <- x4 %>% mutate( GdFrMl = str_split( GoodForMeal, ","))
dim(x4)
dim(x5)
typeof(x5$GdFrMl)
x5$GdFrMl[1]
x5$GdFrMl[1000]


x6<- x5 %>% mutate( GdFrMl = lapply( GdFrMl, extractAmbience ) ) 

#how many examples by different values for 'BusinessParking'
x6 %>% group_by(GdFrMl) %>% tally() %>% view()

x6 %>% filter( str_detect (GdFrMl, 'lunch')) %>% count()
x6 %>% filter( str_detect (GdFrMl, 'dinner')) %>% count()

