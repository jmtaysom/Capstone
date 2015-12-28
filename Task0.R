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
blogs <- readLines(con, 10000)
#blog.mid <- readLines(con, 20000)
close(con)
rm(con)
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

#blogs.ngram <- dfm(blogs, ngrams = 2:2, verbose = FALSE)

#bigram <- tokenize(toLower(blogs), removePunct = TRUE, removeNumbers = TRUE, ngrams =2)
bigram.test<- sum(ntoken(bigram))
topfeatures(blogs.ngram, n = ncol(blogs.ngram)) -> tf
data.frame(tf) -> df.tf
df.tf <- add_rownames(df.tf)
temp<- str_split_fixed(df.tf$rowname,'_',2)
df.tf <- cbind(df.tf,temp)
dup <- duplicated(df.tf$`1`)
df.tf.sub<-df.tf[!dup,]
as.character(df.tf.sub[df.tf.sub$`1` == 'merry',]$`2`)


#dfm.bi <- dfm(bigram)

#tf <- textfile('blogs_sample.txt', textField = 'text')

# colocations are amazing
coloc3 <- collocations(blogs, size=3)
coloc3 <- coloc3[order(-coloc3$count)]
coloc2<- collocations(blogs, size=2)
coloc2 <- coloc2[order(-coloc2$count)]
#coloc[coloc$word1 == 'there',]$word2[1]
#coloc[coloc$word1 == 'what' & coloc$word2 == 'the',]$word3[2]
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

