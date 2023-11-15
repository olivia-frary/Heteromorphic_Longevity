#### MSA Alignment ####
# installing msa
# if(!requireNamespace("BiocManager",quietly=TRUE)) 
#   install.packages("BiocManager") 
# BiocManager::install("msa")

# load in msa
library(msa)
library(seqinr)
library(tidyverse)
library(latex)
system.file("tex","texshade.sty",package="msa")

# load in example fasta
mySequenceFile<-system.file("examples","exampleDNA.fasta",package="msa")
mySequences<-readDNAStringSet(mySequenceFile)
mySequences

# attempt to load in my multi fasta
mySequences <- readDNAStringSet("mitochondrion_seq_format.fasta", format="fasta")
mySequences

# run the msa function which, by default, runs ClustalW with default parameters
myFirstAlignment <- msa(mySequences)
print(myFirstAlignment,show="complete")

# issue with pretty print - pdflatex is not available
# msaPrettyPrint(myFirstAlignment,output="pdf",showNames="none",
#                showLogo="none",askForOverwrite=FALSE,verbose=FALSE)
# msaPrettyPrint(myFirstAlignment,y=c(164,213),output="asis",
#                showNames="none",showLogo="none",askForOverwrite=FALSE)
# so far issue with producing a tex file instead of a pdf