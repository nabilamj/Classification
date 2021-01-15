#### NAIVE BAYES ####

library(tm)
library(NLP)
library(e1071)
library(naivebayes)

berita = read.csv("C:\\Users\\nab\\Documents\\databerita.csv",header=T,quote="",
                  col.names = c("text","tag"),stringsAsFactors=F)
str(berita)
berita$text
berita$tag

beritatag = as.factor(berita$tag)
beritacorpus = VCorpus(VectorSource(berita$text))
beritacorpus
str(beritacorpus)
beritacorpus[[4]]$content
beritaDTM = DocumentTermMatrix(beritacorpus,control=list(tolower=TRUE,
            removeNumbers=TRUE,stopwords=TRUE, removePunctuation=TRUE,
            stemming=TRUE))
beritaDTM
inspect(beritaDTM)
str(beritaDTM)
beritaDTM$dimnames$terms
beritafreq = findFreqTerms(beritaDTM,1)
beritafreq
beritaDTMfreq = beritaDTM[,beritafreq]
beritaDTMfreq

convert.counts = function(x){x<-ifelse(x>0,"yes","no")}
beritatrain = apply(beritaDTMfreq,MARGIN=2,convert.counts)
beritatrain

beritaklas = naiveBayes(beritatrain,beritatag,laplace=1)
beritaklas

tanya = read.csv("C:\\Users\\nab\\Documents\\tanyaberita.csv",header = T,
                 quote = "",col.names = c("text","tag"),stringsAsFactors = F)
str(tanya)
tanya$text
tanyacorpus = VCorpus(VectorSource(tanya$text))
tanyacorpus

tanyaDTM = DocumentTermMatrix(tanyacorpus,control=list(tolower=TRUE,
           removeNumbers=TRUE,stopwords=TRUE,removePunctuation=TRUE,
           stemming=TRUE))
tanyaDTM
tanyafreq = findFreqTerms(tanyaDTM,1)
tanyafreq
tanyaDTMfreq = tanyaDTM[,tanyafreq]
tanyaDTMfreq

tanyatest = apply(tanyaDTMfreq,MARGIN=2,convert.counts)
tanyatest

hasil = predict(beritaklas,tanyatest,type="class")
hasil

hasil = predict(beritaklas,tanyatest,type="raw")
hasil
