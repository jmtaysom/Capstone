#library(tm)
#library(RWeka)
library(quanteda)
library(stringr)
library(plyr)
library(dplyr)
#Download the data for the capstone
setwd("~/Capstone")
url = 'https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'
zip = 'Swiftkey.zip'
download.file(url,zip, method = 'curl')
unzip(zip)
rm(url)
rm(zip)
# Create a connection to the text file and read in a selection.
con <- file('final/en_US/en_US.blogs.txt',"r")
twi.con <- file('final/en_US/en_US.twitter.txt','r')
news.con <-file('final/en_US/en_US.news.txt','r') 
blogs <- readLines(con, 50000)
twitter <- readLines(twi.con, 50000)
news <- readLines(news.con, 50000)
#blog.mid <- readLines(con, 20000)
close(con)
rm(con)
close(twi.con)
rm(twi.con)
close(news.con)
rm(news.con)

#write.table(blogs, file='blogs_sample.txt')
blogs <- readLines(file('blogs_sample.txt'))
#blogs <- iconv(blogs, to = "utf-8", sub="")
#blog_corpus<- corpus(blogs)
rm(blogs)
# 

# BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
# corpus <- Corpus(VectorSource(blogs))
# corpus <- tm_map(corpus,removePunctuation)
# corpus <- tm_map(corpus,stripWhitespace)
# corpus <- tm_map(corpus,removePunctuation);
# matrix_terms <- DocumentTermMatrix(corpus)

#tdm <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))
# ngram <- NGramTokenizer(blogs)

blog.ngram <- dfm(blogs, ngrams = 3:3, verbose = FALSE)
twit.ngram <- dfm(twitter, ngrams = 3:3, verbose = FALSE)
news.ngram <- dfm(news, ngrams = 3:3, verbose = FALSE)
#bigram <- tokenize(toLower(blogs), removePunct = TRUE, removeNumbers = TRUE, ngrams =2)
#bigram.test<- sum(ntoken(bigram))
tf.blog <- topfeatures(blog.ngram, n = ncol(blog.ngram))
tf.blog <- data.frame(tf.blog)
tf.blog <- add_rownames(tf.blog)
tf.twit <- topfeatures(twit.ngram, n = ncol(twit.ngram))
tf.twit <- data.frame(tf.twit)
tf.twit <- add_rownames(tf.twit)


temp<- str_split_fixed(df.tf$rowname,'_',2)
df.tf <- cbind(df.tf,temp)
dup <- duplicated(df.tf$`1`)
df.tf.sub<-df.tf[!dup,]
as.character(df.tf.sub[df.tf.sub$`1` == 'merry',]$`2`)


#dfm.bi <- dfm(bigram)

#tf <- textfile('blogs_sample.txt', textField = 'text')

# colocations are amazing
coloc3.blog <- collocations(blogs, size=3)
coloc3.twit <- collocations(twitter, size=3)
coloc3.news <- collocations(news, size=3)  
coloc3.comb <- rbind(coloc3.blog, coloc3.news, coloc3.twit)
coloc3.comb <- aggregate(count~word1+word2+word3, data=coloc3.comb, FUN=sum)
coloc3.comb <- coloc3.comb[order(-coloc3.comb$count),]
coloc3.comb <- coloc3.comb[coloc3.comb$count >1,]
rm(coloc3.news)
rm(coloc3.blog)
rm(coloc3.twit)

coloc2.blog <- collocations(blogs, size=2)
coloc2.twit <- collocations(twitter, size=2)
coloc2.news <- collocations(news, size=2)  
coloc2.comb <- rbind(coloc2.blog, coloc2.news, coloc2.twit)
coloc2.comb <- aggregate(count~word1+word2+word3, data=coloc2.comb, FUN=sum)
coloc2.comb <- coloc2.comb[order(-coloc2.comb$count),]
coloc2.comb <- coloc2.comb[coloc2.comb$count >1,]
rm(coloc2.news)
rm(coloc2.blog)
rm(coloc2.twit)



coloc2.comb[coloc2.comb$word1 == 'there',]$word2[1]
coloc3.comb[coloc3.comb$word1 == 'case' & coloc3.comb$word2 == 'of',]$word3[2]

rm(blogs)
rm(news)
rm(twitter)
#maybe create tables using tokenize of unigrams, bigrams and trigrams

c3<- cbind(coloc3$word1, coloc3$word2)
dup3 <- duplicated(c3)
coloc3.sub <- coloc3[!dup3]

dup2 <- duplicated(coloc2)
coloc2.sub <- coloc2[!dup2]


rm(c3)
rm(dup3)
rm(coloc3)
rm(coloc2)
rm(dup2)
coloc3.sub[coloc3.sub$word1 == 'of' & coloc3.sub$word2 == 'the',]$word3[1]


#clean the dataset of profanity, offensive terms, .
pattern = 'fuck|shit|damn|hell|cunt|nigger|dick'
blogs_clean <- gsub(pattern, '', blogs, ignore.case = TRUE)
rm(pattern)
rm(blogs)

# matches all punctuation that is either alone by itself or
# is at the end of a word (but not including the word).
# used to remove periods, commas, and the like.
pattern2 = '(?!\w)[^\w\s](?=\s)'

# matches all special characters and can include the whole 
# word if it is in the middle of a word
pattern3 ='\w*[^\w\s]\w*' 

# deal with upper vs lower case
# finding a match (punctions) at end of line uses $ after the pattern
# finding a match  at the beginning of a line uses ^ before the pattern 



head(blogs_clean,3)
tail(blogs,3)
blogs[500]

