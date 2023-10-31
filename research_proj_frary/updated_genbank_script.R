#install.packages("reutils")
library(reutils)

grab.results <- function (term) {
  # Search for the given term on nuccore. This gives us a list of
  # record IDs.
  ids <- esearch(term, db="nuccore")
  
  # Grab summaries for the given record IDs, as a sort-of data frame.
  sum <- esummary(ids, db="nuccore")
  data <- content(sum, as="parsed")
  
  # For some reason, this parser gives us lists of lists instead of a
  # proper data frame (which should be lists of vectors). Return a
  # fixed-up version.
  data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
}

#read in list of species
spec<-read.csv("lifespan_species.csv")
#put in list of genes
gene.list<-c("mitochondrion","coi","coii", "coiii", "cytb", "hunchback") # did hjelmen suggest cytb and hunchback??? I can't remember
#Make matrix for data
accession_out<-matrix(,nrow=length(spec$Species),ncol=(length(gene.list)*2+1))
#make list of column names for table
columns<-list()
columns<-append(columns, "Spec")
for(i in 1:length(gene.list)){
  columns<-append(columns, paste(gene.list[i],"Acc"))
  columns<-append(columns, "Len")
}
colnames(accession_out)<-columns

#loop to add data to table
for(i in 1:length(spec$Species)){
    accession_out[i,1]<-spec$Species[i]
   for(k in 1:6){
  testing<-(grab.results(term=(paste(spec$Species[i],gene.list[k]))))
  if(length(testing)<1){
    (k<-(k+1))}
  #the below lets us know if after we filter to only DNA and remove other species that we still have data
  #this lets us filter out RNA and also organisms that don't match but might appear
  #like parasites, worms, etc.
  else if(length((subset(testing, testing$MolType == "dna" & testing$Organism == spec$Species[i] & as.numeric(testing$Slen)<21000))[1])
<1){
    k<-(k+1)
    }
  #this pulls data if we have data, DNA, and the right species, and filters out things larger than mitochondria
  else{
  testagain<-(subset(testing, testing$MolType == "dna" & testing$Organism == spec$Species[i] & as.numeric(testing$Slen)<21000))
  accession_out[i,(k*2)]<-testagain$AccessionVersion[1]
  accession_out[i,(k*2+1)]<-testagain$Slen[1]}
  }
}

write.csv(accession_out,"accession_numbers.csv", row.names = FALSE)
