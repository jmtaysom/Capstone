twit <- file('final/en_US/en_US.twitter.txt','r')
t <- readLines(twit)
max(sapply(t, nchar))
rm(twit)
close(twit)

love <- grep('love',t)
hate <- grep('hate',t)
bio <- t[grep('biostats',t)]
chess <- grep("A computer once beat me at chess, but it was no match for me at kickboxing",t)


blog <- file('final/en_US/en_US.blogs.txt','r')
b <- readLines(blog)
close(blog)
max(sapply(b, nchar))


news <- file('final/en_US/en_US.news.txt','r')
n <- readLines(news)
close(news)
max(sapply(n, nchar))
