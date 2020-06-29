#pre-condition: 
#install the needed libraries.
#a scenario

library(tm)
library(qdap)
library(quanteda)
library(tm)
library(koRpus)
library(WikidataR)

#arguments:
#1. Disambiguate given a target (seed concept) 
#2. Get related concepts
#3. Process a paragraph-article from wikipedia 
#4. Create a query based on concepts (2. and 3.) 
#5. Create a corpus using the keywords selected by elicitor 
#6. Find keywords in GitHub
#7. Create a new corpus based on keywords selection (6.) and concepts (4.)


#post-condition
#A query to be used in http://corpus-retrieval.herokuapp.com/


#1. Disambiguate given a target
target_query <- c("digital library") #elicitor input or main concept
options <- find_item(target_query, language = "en")
selection <- 1 #elicitor input
selection_text <- options[[1]]$description

#2. Get related concepts
#put selection_text in http://wikifier.org/ to get concepts

#3. Process a paragraph from wikipedia article 
#select the main article in wikipedia (https://en.wikipedia.org/wiki/Digital_library) and select a paragraph 
#use paragraph in http://wikifier.org/ to get concepts

#4. Create a query based on concepts (from 2. and 3.) 
#4.1 show concepts collected in 2. and 3.
#4.2 ask to select 5 keywords
#4.3 ask to create a query with them: (main concept + other concept)
keywords_kb <- c("digital library","database") #elicitor input 


#5. Create a corpus using the keywords selected by elicitor 
c <- Corpus(DirSource("path/_digital_library___database__readmes")) #load unzip corpus created in http://corpus-retrieval.herokuapp.com

#6. Find keywords in GitHub
#6.1 preparing Readme texts
content <- paste(as.character(c[[1]]), collapse="\n")
id <- c[[1]]$meta$id
readme.df <- data.frame(id=id, content=sapply(content, as.character), stringsAsFactors=F)
for (i in 2:length(c)){
	content <- paste(as.character(c[[i]]), collapse="\n")
	id <- c[[i]]$meta$id
    temp <- data.frame(id=id, content=sapply(content, as.character), stringsAsFactors=F)
    readme.df<-rbind(readme.df,temp)
}

#6.2 perform the kwic for the 1st concept in query
contents<-as.character(readme.df$content)
k<-kwic(contents, keywords_kb[1], window=40)

#6.3 join the [pre]-[keyword]-[pos] columns in one column
kwic_text <- stripWhitespace(paste(k$contextPre[1], k$keyword[1], k$contextPost[1]))
kwic_id<-k$docname[1]
kwic_df<- data.frame(id=kwic_id, text=kwic_text, stringsAsFactors=F)
for (i in 2:length(k$docname)){
	kwic_text <- stripWhitespace(paste(k$contextPre[i], k$keyword[i], k$contextPost[i]))
	kwic_id<-k$docname[i]
	temp <- data.frame(id=kwic_id, text=kwic_text, stringsAsFactors=F)
	kwic_df <- rbind(kwic_df, temp)
}

#6.4 filter the kwics that also match with the second concept
contents<- as.character(kwic_df$text)
k2<-kwic(contents, keywords_kb[2], window=80)
idx_k<-which(k$docname %in% k2$docname)
k_query <- k[idx_k,]

#6.5 join the [pre]-[keyword]-[pos] columns in one column
kwic_text <- stripWhitespace(paste(k_query$contextPre[1], k_query$keyword[1], k_query$contextPost[1]))
kwic_id<-k_query$docname[1]
kwic_df<- data.frame(id=kwic_id, text=kwic_text, stringsAsFactors=F)
for (i in 2:length(k_query$docname)){
	kwic_text <- stripWhitespace(paste(k_query$contextPre[i], k_query$keyword[i], k_query$contextPost[i]))
	kwic_id<-k_query$docname[i]
	temp <- data.frame(id=kwic_id, text=kwic_text, stringsAsFactors=F)
	kwic_df <- rbind(kwic_df, temp)
}

#6.6 remove duplicated texts
u_kwic_df<-unique(kwic_df$text)

#6.7 create a corpus of KWICs
corpus_kwic<-Corpus(VectorSource(u_kwic_df))

#6.9 pre-processing of KWIC corpus
docs <- tm_map(corpus_kwic, PlainTextDocument)
docs <- tm_map(docs, stripWhitespace)
removeURL<- function(x) gsub("http[[:alnum:]]*", " ", x)
docs <- tm_map(docs, content_transformer(removeURL))
removePunct <- function(x) gsub("[[:punct:]]"," ", x)
docs<- tm_map(docs, content_transformer(removePunct))
removeImages <- function(x) gsub("image::", " ", x)
docs <- tm_map(docs, content_transformer(removeImages))
removeTarget <- function(x) gsub(":target:", " ", x)
docs<- tm_map(docs, content_transformer(removeTarget))
for(j in seq(docs))   
   {   
	 docs[[j]] <- gsub("/", " ", docs[[j]])   
	 docs[[j]] <- gsub("@", " ", docs[[j]])   
	 docs[[j]] <- gsub("\\|", " ", docs[[j]])   
  }
docs <- tm_map(docs, removeNumbers) 
docs <- tm_map(docs, tolower) 
#there is a bug with stopwords in R. Some stopwords as "will","the","this" is not removed 
my_stopwords <- append(stopwords("english"), c("github", "digital", "library",
				"will", "this", "database", "application","development","document")
				#put as stopword the concepts from query
				#ir is need a work to identify GitHub environmet stopwords such: github, application, development, document
docs<-tm_map(docs, removeWords, my_stopwords)
#docs <- tm_map(docs, stemDocument) # future-work as needs a recovering of origin word after term-frequency calculus
docs <- tm_map(docs, stripWhitespace) 
docs <- tm_map(docs, PlainTextDocument)   
dtm <- TermDocumentMatrix(docs, control=list(wordLengths=c(4,Inf),weighting = weightTfIdf))
m<-as.matrix(dtm)


#select top words
v<-sort(rowSums(m),decreasing = TRUE)
d<-data.frame(word = names(v),freq=v)
write.csv(d, file="top-tfidf-1abril.csv")
list_words<-as.vector(d$word)
string_words<-as.String(paste(list_words, collapse=" "))

#from top words, filter the nouns
pos_tag<-treetag(string_words, treetagger="manual", format="obj", 
                      TT.tknz=FALSE , lang="en",
                      TT.options=list(path="C:/TreeTagger", preset="en"))
res<-pos_tag@TT.res
nn_words <- res[res$tag=="NN",]

#select the top 30 nouns
head(nn_words$token,30) #show to elicitor

#7. Create a new corpus based on concepts selected by elicitor (4.) and keywords (6.)