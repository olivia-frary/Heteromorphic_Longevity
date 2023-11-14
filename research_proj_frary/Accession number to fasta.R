# Install and load the required packages
# install.packages("ape")
# install.packages("seqinr")
library(ape)
library(seqinr)
library(janitor)
library(tidyverse)

#vector for accession numbers
# mitochondrion_accession_numbers <- c("OR413803.1", "NC_006817.1", "NC_081591.1", "NC_046946.1", "OR183370.1")

# read in your file of accession numbers to feed into script
df <- read_csv("accession_numbers.csv") %>% 
  clean_names()

mitochondrion_accession_numbers <- df$coi_acc

# Fetching the sequences from GenBank
mitochondrion_sequences <- read.GenBank(mitochondrion_accession_numbers)

#accession number corresponding to species names/gene
mitochondrion_sequences_GenBank_IDs <- paste(attr(mitochondrion_sequences, "species"), names(mitochondrion_sequences), sep=" coi ")

# Write the sequences to a FASTA file 
write.dna(mitochondrion_sequences, file = "coi.fasta", format = "fasta", append = FALSE, nbcol = 6, colsep = " ", colw = 10)

# Read the sequences from the FASTA file
mitochondrion_seq_format <- read.FASTA(file = "coi.fasta")

# Modify the names of the sequences
names(mitochondrion_seq_format) <- mitochondrion_sequences_GenBank_IDs

# Write sequences to a new FASTA file 
write.FASTA(mitochondrion_seq_format, file = "coi_seq_format.fasta")

# this output a nice multi fasta, I just don't know for sure if I have all the species I'm aiming for