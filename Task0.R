#Download the data for the capstone
setwd("~/datasciencecoursera/Capstone")
url = 'https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'
zip = 'Swiftkey.zip'
download.file(url,zip, method = 'curl')
unzip(zip)
rm(url)
rm(zip)
# Create a connection to the text file and read in a selection.
con <- file('final/en_US/en_US.blogs.txt',"r")
blogs <- readLines(con, 10000)
close(con)
rm(con)
write.table(blogs, file='blogs_sample.txt')
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

head(blogs_clean,3)
tail(blogs,3)
blogs[500]

