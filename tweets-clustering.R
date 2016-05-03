getwd()
library(stringr)
library(tm)
library(RWeka)
library(qdap)
dat=read.csv("final - tweets.txt",header=TRUE,sep="\t",nrows=500)
nrow(dat)
head(dat)
#hash=regmatches(dat$text,gregexpr("#(\\d|\\w)+",dat$text))
#hash
myCorpus = Corpus(VectorSource(dat$text))

myCorpus = tm_map(myCorpus, removeNumbers)
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus = tm_map(myCorpus , stripWhitespace)
myCorpus = tm_map(myCorpus, tolower)
myCorpus = tm_map(myCorpus, removeWords, c(stopwords("english"),"http","rt","chennai","pic"))

# Then remove all "@people"
myCorpus = gsub("@\\w+", "", myCorpus)
# Then remove all the punctuation
myCorpus = gsub("[[:punct:]]", "", myCorpus)
# Then remove numbers, we need only text for analytics
myCorpus = gsub("[[:digit:]]", "", myCorpus)
# the remove html links, which are not required for sentiment analysis
myCorpus = gsub("http\\w+", "", myCorpus)



# build a corpus, and specify the source to be character vectors
myCorpus = Corpus(VectorSource(dat$text))
# convert to lower case
# tm v0.6
myCorpus = tm_map(myCorpus, content_transformer(tolower))
# tm v0.5-10
# myCorpus = tm_map(myCorpus, tolower)
# remove URLs
removeURL = function(x) gsub("http\\w+", "", x)
# tm v0.6
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
# tm v0.5-10
# myCorpus = tm_map(myCorpus, removeURL)
# remove anything other than English letters or space
removeNumPunct = function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus = tm_map(myCorpus, content_transformer(removeNumPunct))
# remove punctuation
# myCorpus = tm_map(myCorpus, removePunctuation)
# remove numbers
# myCorpus = tm_map(myCorpus, removeNumbers)
# add two extra stop words: "available" and "via"
#myStopwords = c(stopwords('english'), "available", "via")
# remove "r" and "big" from stopwords
#myStopwords = setdiff(myStopwords, c("", "big"))
# remove stopwords from corpus
myCorpus = tm_map(myCorpus, removeWords, myStopwords)
# remove extra whitespace
myCorpus = tm_map(myCorpus, stripWhitespace)
# keep a copy of corpus to use later as a dictionary for stem completion
myCorpusCopy = myCorpus
# stem words
myCorpus = tm_map(myCorpus, stemDocument)
# inspect the first 5 documents (tweets)
# inspect(myCorpus[1:5])
# The code below is used for to make text fit for paper width

myCorpus=tm_map(myCorpus, PlainTextDocument)
for (i in c(1:5, 320)) {
cat(paste0("[", i, "] "))
writeLines(strwrap(as.character(myCorpus[[i]]), 60))
}

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
                          control = list(wordLengths = c(1, Inf)))
tdm

#frequent words & associations
idx = which(dimnames(tdm)$Terms == "floods")
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
findAssocs(tdm, "flood", 0.25)

source("http://bioconductor.org/biocLite.R")
biocLite("graph")
biocLite("Rgraphviz")

plot(tdm, term = freq.terms, corThreshold = 0.1, weighting = T)

# remove sparse terms
tdm2 = removeSparseTerms(tdm, sparse = 0.95)
m2 = as.matrix(tdm2)
# cluster terms
distMatrix = dist(scale(m2))
fit = hclust(distMatrix, method = "ward.D")
plot(fit)
rect.hclust(fit, k = 5) # cut tree into 6 clusters

m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
# colors
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:4)]
# plot word cloud
library(wordcloud)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F, colors = pal)


m3 = t(m2) # transpose the matrix to cluster documents (tweets)
set.seed(122) # set a fixed random seed
k = 6 # number of clusters
kmeansResult = kmeans(m3, k)
round(kmeansResult$centers, digits = 3) # cluster centers

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
lda = LDA(dtm, k = 4) # find 8 topics
(term = terms(lda, 6)) # first 6 terms of every topic
term = apply(term, MARGIN = 2, paste, collapse = ", ")

# first topic identified for every document (tweet)
topic = topics(lda, 1)
topics = data.frame(date=dmy_hms(paste(dg1$date, dg1$time), topic))
qplot(date, ..count.., data=topics, geom="density",fill=term[topic], position="stack")

