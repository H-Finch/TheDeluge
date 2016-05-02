library(stringr)
library(cluster)
library(tm)
library(RWeka)
library("RTextTools")
library(qdap)
dat1=read.csv("myCorpus.txt",header=TRUE)
dat2=subset(dat1, nchar(as.character(terms)) > 4)
head(dat2)
apply(dat2,2,max,na.rm=TRUE)

dat2c=Corpus(VectorSource(dat2))
dat2c <- tm_map(dat2c, PlainTextDocument)  
dtm = DocumentTermMatrix(dat2c)   

dtm = create_matrix(dat2,removeStopwords=TRUE,minWordLength = 6,stemWords = TRUE, removePunctuation= TRUE)

kmeans5= kmeans(dtm, 6)
clusplot(as.matrix(d), kmeans5$cluster, color=T, shade=T, labels=2, lines=0)
kw_with_cluster <- as.data.frame(cbind(searchkeywords$'Natural Search Keyword', kmeans5$cluster))
names(kw_with_cluster) <- c("keyword", "kmeans5")
write.csv(as.data.frame(kmeans5),"kmeans.csv")

library(fpc)   
d = dist(t(dtm), method="euclidian")   
kfit = kmeans(d, 1)   
clusplot(d, kmeans5$cluster, color=T, shade=T, labels=2, lines=0)    


m2 = as.matrix(dtm)
# cluster terms
distMatrix = dist(scale(m2))
fit = hclust(distMatrix, method = "ward.D")
plot(fit)
rect.hclust(fit, 5, border="red")
res=cutree(fit, 5)
res
kterms=as.data.frame(cbind(kmeans5$'terms',kmeans5$cluster))
names(kterms)=c("term","kmean5")
