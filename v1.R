getwd()
library(stringr)
library(lubridate)
library(plotly)
library(tm)
library(RWeka)
library(qdap)
library(lda)
library(LDAvis)
library(useful)
dat=read.csv("final - tweets.txt",header=TRUE,sep="\t",nrows=500)
nrow(dat)
head(dat)
#hash=regmatches(dat$text,gregexpr("#(\\d|\\w)+",dat$text))
#hash
# build a corpus, and specify the source to be character vectors
myCorpus = Corpus(VectorSource(dat$text))
# convert to lower case
myCorpus = tm_map(myCorpus, content_transformer(tolower))
# remove URLs
removeURL = function(x) gsub("http\\w+", "", x)
myCorpus = tm_map(myCorpus, content_transformer(removeURL))
#myCorpus = tm_map(myCorpus, removeURL)
removeURL = function(x) gsub("pic\\w+", "", x)
myCorpus = tm_map(myCorpus, content_transformer(removeURL))
removeURL = function(x) gsub("twitter\\w+", "", x)
myCorpus = tm_map(myCorpus, content_transformer(removeURL))
removeURL = function(x) gsub("docs\\w+", "", x)
myCorpus = tm_map(myCorpus, content_transformer(removeURL))
#remove hashtags
removeURL = function(x) gsub("#(\\d|\\w)+", "", x)
myCorpus = tm_map(myCorpus, content_transformer(removeURL))
#remove @mentions
removeURL = function(x) gsub("@\\w+", "", x)
myCorpus = tm_map(myCorpus, content_transformer(removeURL))
# remove anything other than English letters or space
removeNumPunct = function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus = tm_map(myCorpus, content_transformer(removeNumPunct))
# remove punctuation
myCorpus = tm_map(myCorpus, removePunctuation)
# remove numbers
myCorpus = tm_map(myCorpus, removeNumbers)
#remove stopwords
myCorpus = tm_map(myCorpus, removeWords, c(stopwords("english"),"http","rt","chennai","pic","help","please"))
# remove extra whitespace
myCorpus = tm_map(myCorpus, stripWhitespace)

# stem words
myCorpus = tm_map(myCorpus, stemDocument)


# keep a copy of corpus to use later as a dictionary for stem completion
myCorpusCopy = myCorpus

# inspect the first 5 documents (tweets)
# inspect(myCorpus[1:5])
# The code below is used for to make text fit for paper width


for (i in c(1:5, 320)) {
  cat(paste0("[", i, "] "))
  writeLines(strwrap(as.character(myCorpus[[i]]), 60))
}
myCorpus=tm_map(myCorpus, PlainTextDocument)

stemCompletion2 = function(x, dictionary) {
  x = unlist(strsplit(as.character(x), " "))
  # Unexpectedly, stemCompletion completes an empty string to
  # a word in dictionary. Remove empty string to avoid above issue.
  x = x[x != ""]
  x = stemCompletion(x, dictionary=dictionary)
  x = paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}

myCorpus = lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)
myCorpus = Corpus(VectorSource(myCorpus))

#counting term frequencies
miningCases = lapply(myCorpusCopy,
                     function(x) { grep(as.character(x), pattern = "nn<chennai")} )
sum(unlist(miningCases))
tdm = TermDocumentMatrix(myCorpus,
                         control = list(wordLengths = c(4, Inf)))
tdm

#frequent words & associations
idx = which(dimnames(tdm)$Terms == "floods")
dim(tdm)
inspect(tdm[idx + (0:5), 101:110])
# inspect frequent words
(freq.terms = findFreqTerms(tdm, lowfreq = 15))
term.freq = rowSums(as.matrix(tdm))
term.freq = subset(term.freq, term.freq >= 15)
df = data.frame(term = names(term.freq), freq = term.freq)
library(ggplot2)
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") +
  xlab("Terms") + ylab("Count") + coord_flip()
# which words are associated with 'flood'?
findAssocs(tdm, "flood", 0.20)

source("http://bioconductor.org/biocLite.R")
biocLite("graph")
biocLite("Rgraphviz")

plot(tdm, term = freq.terms, corThreshold = 0.1, weighting = T)

# remove sparse terms
tdm2 = removeSparseTerms(tdm, sparse = 0.95)
m2 = as.matrix(tdm2)
# cluster terms
distMatrix = dist(scale(m2))
fit = hclust(distMatrix, method = "ward.D2")
plot(fit)
rect.hclust(fit, k = 4) # cut tree into 6 clusters

m = as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq = sort(rowSums(m), decreasing = T)
# colors
pal = brewer.pal(9, "BuGn")
pal = pal[-(1:4)]
# plot word cloud
library(wordcloud)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F, colors = pal)


m3 = t(m2) # transpose the matrix to cluster documents (tweets)
set.seed(122) # set a fixed random seed
k = 6 # number of clusters
kmeansResult = kmeans(m3, k)
round(kmeansResult$centers, digits = 3) # cluster centers
#clusplot(m3, kmeansResult$cluster, color=T, shade=T, labels=2, lines=0)
plot(kmeansResult,m3)

for (i in 1:k) {
  cat(paste("cluster ", i, ": ", sep = ""))
  s = sort(kmeansResult$centers[i, ], decreasing = T)
  cat(names(s)[1:5], "nn")
  # print the tweets of every cluster
  # print(tweets[which(kmeansResult$cluster==i)])
}

dtm = as.DocumentTermMatrix(tdm)
library(topicmodels)
rowTotals = apply(dtm , 1, sum) #Find the sum of words in each Document
dtm  = dtm[rowTotals> 0, ]           #remove all docs without words
lda = LDA(dtm, k = 8) # find 8 topics
(term = terms(lda, 6)) # first 6 terms of every topic
term = apply(term, MARGIN = 2, paste, collapse = ", ")

# first topic identified for every document (tweet)
topic = topics(lda, 1)
topics = data.frame(date=dmy_hms(paste(dat$date), topic))
qplot(date, ..count.., data=ldaModel$topics, geom="density",fill=term[topic], position="stack")
topics(lda)
terms(lda)

corpusLDA = lexicalize(myCorpus )
require(lda)

ldaModel=lda.collapsed.gibbs.sampler(corpusLDA$documents,K=10,vocab=corpusLDA$vocab,burnin=9999,num.iterations=1000,alpha=1,eta=0.1)
top.words = top.topic.words(ldaModel$topics, 5, by.score=TRUE)
print(top.words) 
topic.proportions = t(ldaModel$document_sums) / colSums(ldaModel$document_sums)
topic.proportions