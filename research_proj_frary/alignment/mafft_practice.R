#### Attempting to interface with MAFFT through R ####
# install.packages("ips")
#install.packages("adegenet")
library(ips)
help(ips)

#### slow code written while learning ####
# look at converting fasta into DNAbin with adegenet::fasta2DNAbin
multi_fasta <- adegenet::fasta2DNAbin("mitochondrion_seq_format.fasta")
# Warning message:
# In matrix(res, nrow = length(IND.LAB), byrow = TRUE) :
  # data length [616705] is not a sub-multiple or multiple of the number of rows [39]

# edit the mafft program pathway so that it is more universal
output <- mafft(multi_fasta, exec="C:/Users/olivi/mafft-7.520-win64-signed/mafft-win/mafft.bat",
                     quiet = TRUE)
# try having mafft software downloaded to your pc -- BETTER VERSION REQUIRES UBUNTU
# I'm using the windows one because I got sick of setting up ubuntu and r.

# writing output files
write.fas(output, file = "mafft_output.fasta")
write.nex(output, file = "mafft_output.nex", taxblock = TRUE)

#### faster code ####
# Try the above code but faster - took about 5 minutes
input <- read.fas("mitochondrion_seq_format.fasta")
# still need to do the exec file pathway differently :)
output2 <- mafft(input, exec="C:/Users/olivi/mafft-7.520-win64-signed/mafft-win/mafft.bat",
                      quiet = TRUE)
# write to files, learn what the different parameters mean.
write.fas(output2, file = "mafft2_output.fasta") # fasta better resembles web version's
write.nex(output2, file = "mafft2_output.nex", taxblock = TRUE)


# trying to visualize the data

# if (!requireNamespace("devtools", quietly=TRUE))
#   install.packages("devtools")
# devtools::install_github("YuLab-SMU/ggmsa")
library(ggmsa)

ggmsa(output2, start = 221, end = 280)
# Error in tidy_msa(msa, start = start, end = end) : 
#   Sequences must have unique names