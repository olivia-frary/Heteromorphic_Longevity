# this package is able to access NCBI databases we're using 
# it to access GenBank
library(reutils)

# below is a function that Carl wrote.
grab.results <- function(term){
  # The esearch() function is part of the reutils package and can
  # retrieve primary UIDs for use with esummary, elink, or efetch.
  # Also, it returns term translations so you can store the result.
  # Search for the given term on nuccore (nucleotide database). 
  # This gives us a list of record IDs.
  # term <- c("Ablabesmyia longistyla","coi")
  ids <- esearch(term, db="nuccore")
  # Grab summaries for the given record IDs, as a sort-of data frame.
  sum <- esummary(ids, db="nuccore")
  data <- content(sum, as="parsed")
  # For some reason, this parser gives us lists of lists instead of a
  # proper data frame (which should be lists of vectors). Return a
  # fixed-up version.
  data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
}

# read species list in to object 'spec'
spec <- read.csv("diptera_species.csv")
str(spec) # convert data frame to a string data type
# testing <- grab.results(term=(paste(spec$Species[1],"coi")))
# testing$AccessionVersion[1]
i <- 1
diptera_out<-matrix(,nrow=length(spec$Species),ncol=3)
####issues
for(i in 1:10){
  diptera_out[i,1]<-spec$Species[i]
  testing<-(grab.results(term=(paste(spec$Species[i],"coi"))))
  ifelse((testing == "Error:
          Empty id list - nothing todo
          Warning messages:
          Errors parsing DocumentSummary "),
         (i<-(i+1)),
         (diptera_out[i,2]<-testing$AccessionVersion[1]))

}

for(i in 1:10){
  diptera_out[i,1]<-spec$Species[i]
  testing<-(grab.results(term=(paste(spec$Species[i],"coi"))))
  ifelse((testing == "Error:
          Empty id list - nothing todo
          Warning messages:
          Errors parsing DocumentSummary "),
         (i<-(i+1)), # change this or check that we aren't iterating too much.
         (diptera_out[i,3]<-testing$Slen[1]))
}

# do if else
#Column of whether its coi, mitochondrial, coii, coii

for(i in 1:10){
  diptera_out[i,1]<-spec$Species[i]
  testing<-(grab.results(term=(paste(spec$Species[i],"coi"))))
  if(length(testing)<1){
    (i<-(i+1))
  } else {
    diptera_out[i,2]<-testing$AccessionVersion[1]
    diptera_out[i,3]<-testing$Slen[1]
  }
}

# add a nested loop for multiple genes - hox hedgehog
# hox evodevo head-tail axis of animals

#if(condition){
#   code block
# } else{
#   
# }


# added for loop for the third column, find a way to add the third
# columns using only the for loop above
# for(i in 1:100){
#   diptera_out[i,1]<-spec$Species[i]
#   testing<-(grab.results(term=(paste(spec$Species[i],"coi"))))
#   ifelse((testing == "Error:
#           Empty id list - nothing todo
#           Warning messages:
#           Errors parsing DocumentSummary "),
#          (i<-(i+1)),
#          ((diptera_out<-rbind(diptera_out, c(testing$AccessionVersion[1],
#                               testing$Slen[1]))))
# }

