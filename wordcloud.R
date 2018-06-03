#to install the below packages uncomment
#install.packes("tm")
#this package is used to create a corpus
library(tm) 
#install.packages("SnowballC")
#this package contains gsub() which helps to replace some symbols to ""
library(SnowballC) 
#install.packages("RColorBrewer")
#this package is to provide distinct colors to the most frequently occuring words while visualising it
library(RColorBrewer)
#install.packages("wordcloud")
#this package helps to visualise the words in the comments in terms of frequency of words
library(wordcloud)

#the below reads the from the survey.csv file
ip<-read.csv('survey.csv',stringsAsFactors = FALSE)
#the below makes it into a data frame
ip<-data.frame(ip)
com<-c()
ct<-0
#the below for loop gets all the valid comments
for(i in 1:nrow(ip)){
  if(!(is.na(ip$comments[i]))==TRUE){
    com[ct]<-ip$comments[i]
    ct<-ct+1
  }
}
#the below is to create a R readable format of the comments
ip.Corpus<-Corpus(VectorSource(com))


#the below is to create a plain text document of the comments
ip.Clean<-tm_map(ip.Corpus, PlainTextDocument)
#the below assigns ip.Clean to x
x = ip.Clean
#the below 7 statements replaces the first argument in x(comments) with the second argument("")
#this helps in filtering out many unneccessary symbols which occur in the comments 
x = gsub("&amp", "", x)

x = gsub("(RT|via)((?:\b\\W*@\\w+)+)", "", x)

x = gsub("@\\w+", "", x)

x = gsub("[[:punct:]]", "", x)

x = gsub("[[:digit:]]", "", x)

x = gsub("http\\w+", "", x)

x = gsub("[ t]{2,}", "", x)

x = gsub("^\\s+|\\s+$", "", x)

#the below again takes x and creates corpus(or R readable format of comments)
clean_comment_corpus = Corpus(VectorSource(x))
#the below strips whitespaces
clean_comment_corpus<-tm_map(clean_comment_corpus,stripWhitespace)
#the below converts all letters to lowercase
clean_comment_corpus<-tm_map(clean_comment_corpus,tolower)
#the below removes numbers
clean_comment_corpus<-tm_map(clean_comment_corpus,removeNumbers)
#the below removes punctuations
clean_comment_corpus<-tm_map(clean_comment_corpus,removePunctuation)
#the below removes words like "a" , "and", "the" ,"that" , etc
clean_comment_corpus<-tm_map(clean_comment_corpus,removeWords, stopwords("english"))

#the below statement converts the comments into a structured dataset (document matrix) after removing all the filler words
doc_matrix = TermDocumentMatrix(clean_comment_corpus)
require(plyr)

#the below redefines doc_matrix as matrix
m = as.matrix(doc_matrix) 

#the below gets the word orders in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE)
#the below creates a data frame of the words and the frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs) 
#the below creates a word cloud with 100 most frequently occuring words
#and it places the most frequently occuring words in the center
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:2)]
w<-wordcloud(dm$word,dm$freq,scale = c(5,.4),colors=brewer.pal(7, "Dark2"),max.words = 100)
