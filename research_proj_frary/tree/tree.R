library(Biostrings)
library(DECIPHER)
library(phangorn)
library(ggtree)
library(ape)
library(tidyverse)
library(janitor)

# create phylogenetic tree of the smaller subset of species
gimme_fast <- readDNAStringSet("accession/coi_seq_format.fasta") # read in multi fasta as a string set
alignment <- AlignSeqs(gimme_fast) # perform profile-to-profile alignment
distance <- dist.dna(as.DNAbin(alignment)) # find pairwise distances
tree <- njs(distance) # have to use this because there is studd missing from the distance matrix
ape::write.tree(tree, file='tree/tree.txt') # write tree out in newick format

tree <- read.tree("tree/tree.txt")

# is.rooted(tree)
# tree <- root(tree, outgroup = 18, resolve.root = TRUE)
# tree <- drop.tip(tree, tip = 18)

options(ignore.negative.edge=TRUE) # WHAT IS THIS - i did this to stop the tree going back on itself

ggtree(tree, layout = "rectangular") +
  geom_tiplab(align=TRUE, linesize=.5)

tree2 <- drop.tip(tree, tip = 12) # removes pan troglodytes because we know that one is weird and wrong
tree2 <- drop.tip(tree2, tip = 11) # removes ovis aries because it is so big we can't see anything else
tree2 <- drop.tip(tree2, tip = 1)
ape::write.tree(tree2, file='tree/tree2.txt')

tree2 <- read.tree("tree/tree2.txt") # load the tree file in
tree2$edge.length <- rep(1, Nedge(tree2))

p <- ggtree(tree2)

# add in the data you want
# currently this plot shows 22 of the 25 observations in a neighbor based phylogeny 
df <- read_csv("short_complete_data.csv") %>% 
  filter(species != c("Ovis aries","Pan troglodytes")) %>% 
  filter(species != "Amblyomma cajennense")

d1 <- data.frame(id=tree2$tip.label,
                 life=df$ln_r_rlifespan,
                 gs_diff=df$gs_diff_mb,
                 sex=df$sex_determination)

p2 <- facet_plot(p, panel="Difference in Lifespan",
                 data=d1,
                 geom=geom_point,
                 mapping=aes(x=life, color=sex))

p3 <- facet_plot(p2, panel="Difference in Genome Size",
                 data=d1,
                 geom=geom_segment, 
                 aes(x=0, xend=gs_diff, y=y, yend=y, color=sex), size=10)

p3 + theme_tree2() + geom_vline(xintercept = 0, alpha=0.5, linetype = "dashed")

ggsave("tree/facet_tree.png") # make sure thr photo it saves is nicely formatted 

# this plot makes it more obvious that Ovis aries is a big outlier that makes the other
# data hard to see
# also remember that Pan troglodytes is incorrect because it is an X0 system. Female must
# have a larger genome size
tree$tip.label

t <- full_join(tree,d1, join_by(tip.label==id))

?full_join()

p1 <- ggtree(tree2, aes(color=as.numeric(d1$ln_r_rlifespan)), layout = 'circular', 
             ladderize = FALSE, continuous = 'colour', size=2) +
             scale_color_gradientn(colours=c("red", 'orange', 'green', 'cyan', 'blue')) +
             geom_tiplab(hjust = -.1) + 
             xlim(0, 1.2) + 
             theme(legend.position = c(.05, .85))


