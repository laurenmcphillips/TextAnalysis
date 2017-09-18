# Goal: Importing text and performing word frequency analysis
#       This code is specifically aimed towards analyzing text from multiple .pdf files of
#       research articles.
# Authors: Lauren E. McPhillips & Matthew R. Herndon
# Origin Date: Sept 2016
# Last update: May 2017

# PRE-PROCESSING in the command line (outside of R)
#    Required if you are extracting text from .pdf versions of articles
#    This command line call will convert all pdfs in a folder to txt files
#     Note: currently this pre-processing approach only works on Mac or UNIX machines.
#         There are some online tools available for Windows that allow you to perform this
#         initial step of extracting text from .pdf files.
# Step 1: Open up a command window. 
# Step 2: navigate to your folder containing the .pdf files using the 'cd' command
#       e.g., cd /Users/lauren/Dropbox/Research/ASU/ExtremeEventAnalysis/Papers
# Step 3:enter the following lines into the command window which will convert the .pdf files to text
#       find . -name '*.pdf' -print0 | xargs -0 -n1 pdftotext

# FORMATTING AND ANALYSIS OF TEXT

# Packages to install and load-'tm' and 'SnowballC'
install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)

#   Indicate the directory in which your text files are located
folderPath = "/Users/laurenmcphillips/Dropbox (ASU)/URExSRN/DEE_TF/Analysis/CLIM_text"
#   Designate a name for your output graph
graphName = "MyGraph.png"
#   Change working directory to the folder where your text files are
setwd(folderPath)
getwd()

# Create a 'Corpus'- a body of text that aggregates all the text from your various files
#   Pull text from your directory into a corpus
origDocs <- Corpus(DirSource(folderPath))

# Optional check: Print out text of the first document to the command line
strwrap(origDocs[[1]])

# Remove the Reference section from files so they do not skew word counts
#     Make a new No_References directory to put all the cleaned up file copies in so you can review them
if(!dir.exists("No_References")){
  dir.create("No_References")
}
setwd("No_References")

#     For each text file, find the last occurrence of the word References and trim everything beyond it 
#     Also remove the boilerplate 'For personal use only' strings
for(i in 1:length(origDocs)){
  fileName = as.character(meta(origDocs[i], "id"))
  print(fileName)
  maxRef = max(grep("references|LITERATURE CITED",as.character(origDocs[[i]]),ignore.case=TRUE,perl=TRUE))
  stringOut <- as.character(origDocs[[i]])[1:maxRef]
  fileConn<-file(fileName)
  writeLines(stringOut,fileConn)
  close(fileConn)
}

#   Check your directory
getwd()
#   Create a new corpus with the text with references removed
docs <- Corpus(DirSource("./"))

#Clean up text for word analysis by removing punctuation, numbers, common stopwords (the, a, an, etc.), and extra spaces
#   Create the toSpace content transformer
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})

#   Use toSpace to convert punctuation that removePunctuation misses
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, "`")
docs <- tm_map(docs, toSpace, " -")

#   Transform to lower case so words like Bird and bird will be equivalent (need to wrap in content_transformer)
docs <- tm_map(docs,content_transformer(tolower))
#   Strip digits (standard transformation, so no need for content_transformer)
docs <- tm_map(docs, removeNumbers)
#   Remove stopwords using the standard list in tm
docs <- tm_map(docs, removeWords, stopwords("english"))
#   Strip whitespace 
docs <- tm_map(docs, stripWhitespace)
#   Print out the first document after all the transforms to check what it looks like now
strwrap(docs[[1]])

#'Stem' the words in the document 
#   this cuts words into their stems so 'flexibility' and 'flexible' both become 'flexibl' and counted together
#   beware: Stem is somewhat crude, sometimes it will incorrectly match things, others it will fail to match things
docs <- tm_map(docs,stemDocument)
strwrap(docs[[1]])
dtm <- DocumentTermMatrix(docs)
dtm
freq <- colSums(as.matrix(dtm))

#Organize and re-order stemmed words
#   Order the words in descending frequency
ord <- order(freq,decreasing=TRUE)
#   Inspect most frequently occurring terms
freq[head(ord)]
#   Inspect least frequently occurring terms
freq[tail(ord)]   

#Filter text based on how many documents they appear in
#   Include only those words that occur in  3 to n documents with lengths between 4 and 20 characters.
#     The number of documents chosen should depend on how many docs you have total a good rule of them
#     that a word must occur in 10% of docs or more but not show up in more than 90% of all docs
#   Indicate what your minimum and maximum cut-offs are for how many documents the word appears in
    MinDoc=5
    MaxDoc=45
#   Create a new document matrix that filters the text
dtmr <-DocumentTermMatrix(docs, control=list(wordLengths=c(4, 20), bounds = list(global = c(MinDoc,MaxDoc))))
m <- as.matrix(dtmr)
freqr <- colSums(m)
length(freqr)
#   Re-check the most frequently and least frequently occurring words
ordr <- order(freqr,decreasing=TRUE)
freqr[head(ordr,20)]
freqr[tail(ordr)]

#Optional: assessment of associated words in a text file
#   What terms appear in the same document? 
#   The correlation means that in x% of docs, we find both of these words,
#     so not necessarily right after one another, but just in the same document
#   Indicate correlation threshold
CorrThres=0.7
#   Indicate word for which you want to find associations
AssocWord="hazard"
#   Find associations
findAssocs(dtmr,AssocWord,CorrThres)

# VISUALIZATION OF PROCESSED TEXT

# Packages to install and load-'ggplot2' and 'wordcloud'
install.packages("ggplot2")
library(ggplot2)
install.packages("wordcloud")
library(wordcloud)

# Plot histogram of most common words
#   First, make a dataframe of your words and their frequencies
wf=data.frame(term=names(freqr),occurrences=freqr)
wf=data.frame(term=names(freq),occurrences=freq)
#   Normalize the word frequency by the # of text files analyzed
wf$occ.norm=wf$occurrences/length(docs)

#   Plot up, with order of terms by descending order of occurrence 
#     Set a frequency threshold for the words to include
FreqThres=10
#     Set up plot
p2 <- ggplot(data=subset(wf, wf$occurrences/length(docs)>FreqThres), aes(x=reorder(term, -occurrences/length(docs)), y=occurrences/length(docs)))
p2 <- p2 + geom_bar(stat="identity")
p2 <- p2 + theme(axis.text.x=element_text(angle=45, hjust=1))
p2 <- p2 + labs(x="",y="Frequency")+ theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"), 
                                           legend.text=element_text(size=12), legend.title=element_text(size=12),
                                           axis.text.x = element_text(angle = 45, hjust = 1)) 
#   Preview your graph
p2
#   Designate a name for your output graph
graphName = "MyGraph.png"
#   Save your graph to the current directory
ggsave(filename=graphName,units="in",width=6,height=4,dpi=200)

#Create a wordcloud, with words sized by their frequency of occurrence
#   Setting the same seed each time ensures consistent look across clouds
set.seed(42)
#   Limit words by specifying a minimum frequency
wordcloud(names(freqr),freqr, min.freq=4)
#   Add color
wordcloud(names(freqr),freqr,min.freq=10,colors=brewer.pal(6,"Dark2"))
#   Specify a graph name
graphName2=SecondGraph.png
#   Save your wordcloud
ggsave(filename=graphName2,units="in",width=6,height=4,dpi=200)
