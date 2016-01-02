# setwd("~/Capstone")
# tf2 <- read.table(file = 'tf2_100k.txt')
# tf3 <- read.table(file = 'tf3_100k.txt')
# tf4 <- read.table(file = 'tf4_100k.txt')

test <- 'Merry '

split <- strsplit(tolower(test), ' ')

end <- tail(split[[1]],3)
len <- length(end)

if (len > 2) {
  a <- as.character(tf4[tf4$V1 == end[1] & tf4$V2 == end[2] & tf4$V3 == end[3],]$V4)
  if (identical(a,character(0))){
    a <- as.character(tf3[tf3$X1 == end[2] & tf3$X2 == end[3],]$X3)
    if (identical(a,character(0))){
      a <- as.character(tf2[tf2$X1 == end[3],]$X2)
    }
  }
}else if (len == 2) {
  a <- as.character(tf3[tf3$X1 == end[1] & tf3$X2 == end[2],]$X3)
  if (identical(a,character(0))){
    a <- as.character(tf2[tf2$X1 == end[2],]$X2)
  }
}else {
  a <- as.character(tf2[tf2$X1 == end[1],]$X2)
}
if (identical(a,character(0))) {
  a <- 'the'
}
print(a)