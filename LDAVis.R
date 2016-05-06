#####LATENT DIRICHLET ALLOCATION#####
# text=dat$text
# text = function(x) gsub("'", "", x)  # remove apostrophes
# text = function(x) gsub("[[:punct:]]", " ", x)  # replace punctuation with space
# text = function(x) gsub("[[:cntrl:]]", " ", x)  # replace control characters with space
# text = function(x) gsub("^[[:space:]]+", "", x) # remove whitespace at beginning of documents
# text = function(x) gsub("[[:space:]]+$", "", x) # remove whitespace at end of documents
# text = function(x) gsub("http\\w+", "", x)
# text = function(x) gsub("pic\\w+", "", x)
# text = function(x) gsub("twitter\\w+", "", x)
# text = function(x) gsub("#(\\d|\\w)+", "", x)
# text = function(x) gsub("@\\w+", "", x)
# text = function(x) tolower(x) # force to lowercase
tdm=removeSparseTerms(tdm,0.99)
dim(tdm)
tdm = tdm[rowSums(as.matrix(tdm))>0,]
SpecificTerms = function(lda.model,n=1) {
  p = posterior(lda.model)$terms
  n = min(n,ncol(p))
  cumulativep = colSums(p)
  specificp = t(apply(p,1,function(x) ((x) + (x/cumulativep) )))
  
  topterms = t(apply(specificp, 1, function(x)
    (colnames(specificp)[order(x,decreasing=T)[1:n]]) ))
  
  topterms
}
set.seed(51)
trainpoints = sample(1:nrow(dtm),1.0*nrow(dtm),replace=F) # to train on a subsample, change 1.0 to a lower value, say 0.8
k = 6
lda.model = LDA(dtm[trainpoints,], k)

t(SpecificTerms(lda.model,10))
terms(lda.model,100)#final result

save(lda.model,file="results/lda.model")
require(reshape2)
require(ggplot2)
require(RColorBrewer)

lda.topics = topics(lda.model,1)
termgenerator = posterior(lda.model)$terms

###1: relative probabilities of words in each topic ###
termimportance = apply(termgenerator,1,
                        function(x)	x[order(x,decreasing=T)[1:100]])
termimportance.longform = melt(termimportance,
                                value.name="probability",
                                varnames=c("termnumber","topic"))

ggplot(data=termimportance.longform,
       aes(
         x=termnumber,
         y=probability,
         color=factor(topic),
         group=topic)) + 
  geom_line()
###2: Individual tweets' distribution over topics ###
tweets.topics = posterior(lda.model,dtm[trainpoints,])$topics
df.tweets.topics = as.data.frame(tweets.topics)
df.tweets.topics = cbind(tweet=as.character(rownames(df.tweets.topics)),
                          df.tweets.topics, stringsAsFactors=F)

df.tweets.topics.longform = melt(df.tweets.topics,
                                  id.vars="tweet",variable.name="topic")
df.tweets.topics.longform = df.tweets.topics.longform[order(as.numeric(df.tweets.topics.longform$tweet)),]

topic.names = terms(lda.model,6)
topic.names = apply(topic.names, 2, paste, collapse=",")
topic.names
names(topic.names) = NULL

n.tweets.to.plot = 30
start.tweet = 5000

four.colour.palette = brewer.pal(10,"Paired")[c(1,3,5,7)]

bplot = ggplot(df.tweets.topics.longform[(start.tweet*k)+1:(n.tweets.to.plot*k),],
                aes(x=tweet,y=value)) + 
  
  geom_bar(stat="identity",position="stack",aes(fill=topic))+
  coord_flip()
bplot
bplot + 
  scale_fill_manual(values=four.colour.palette,
                    name="Topic", breaks=1:k,labels=topic.names) +
  coord_flip()

