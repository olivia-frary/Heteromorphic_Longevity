library(Biostrings)
library(DECIPHER)
library(phangorn)
library(ggtree)
library(ape)
library(tidyverse)
library(janitor)
library(ggmsa)
library(treedata.table)

# create phylogenetic tree of the smaller subset of species
gimme_fast <- readDNAStringSet("accession/coi_seq_format.fasta") # read in multi fasta as a string set
alignment <- AlignSeqs(gimme_fast) # perform profile-to-profile alignment DECIPHER

# get a multiple alignment 
mult_alignment <- DNAMultipleAlignment(alignment)
ggmsa(mult_alignment, start = 221, end = 280, color = "Shapely_NT", char_width = 0.5, seq_name = T) + geom_seqlogo() + geom_msaBar()
# picking the color did not work here, see why this is happening

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
tree2 <- drop.tip(tree2, tip = 1) # WHAT IS THISSSS why did I not label it
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
treemafft$tip.label

t <- full_join(tree,d1, join_by(tip.label==id))

?full_join()

df <- read_csv("short_complete_data.csv")

d1 <- data.frame(correct_species = df$species,
                 life=df$ln_r_rlifespan,
                 gs_diff=df$gs_diff_mb,
                 sex=df$sex_determination)

# this isn't working, I think cuz I don't have data for the branches
p1 <- ggtree(treemafft, aes(color=as.numeric(td$dat$life)), layout = 'circular', 
             ladderize = FALSE, continuous = 'color', size=2) +
             geom_tiplab(hjust = -.1) + 
             xlim(0, 1.2) + 
             theme(legend.position = c(.05, .85))

p1 <- ggtree(treemafft, aes(color=as.numeric(d1$life)), data=p$data %>% dplyr::filter(isTip==TRUE), layout = 'circular', 
             ladderize = FALSE, continuous = 'color', size=2) +
              geom_tiplab(hjust = -.1) + 
              xlim(0, 1.2) + 
              theme(legend.position = c(.05, .85))

ggtree(treemafft, aes(color=td$dat$life),data=p$data %>% dplyr::filter(isTip==TRUE)) +
  scale_color_continuous(low='darkgreen', high='red') +
  theme(legend.position="right")



# make sure data has the same amount as tree tips
p <- ggtree(treemafft) 
p2 <- p+geom_tippoint(aes(colour=as.numeric(td$dat$life)),
                      data=p$data %>% dplyr::filter(isTip==TRUE),
                      size=3) +
  scale_colour_gradient(low='blue', high='Orange', name = "Difference in Lifespan")
p2 + geom_tiplab(linesize=.5,offset = 0.02) + ggplot2::xlim(0, 1)

ggsave("tree/gradient_tree.png")

# HAS ACCURATE DATA NOW
ggtree(treemafft) +
  geom_label2(aes(label = label), ) +
  ggplot2::xlim(0, 1)

# other tree
treemafft <- read.nexus("tree/first_tree") # better tree made with Dr. Hjelmen
ggtree(treemafft) +
  geom_tiplab(align=TRUE, linesize=.5) + ggplot2::xlim(0, 1)

ggsave("tree/beast_tree.png")

# trying to match data to beast tree

new_tiplabels <- c("Amblyomma cajennense","Aphelocoma coerulescens","Callosobruchus maculatus",
                   "Cochliomyia hominivorax","Drosophila ananassae","Drosophila erecta",
                   "Drosophila kikkawai","Drosophila melanogaster","Drosophila mojavensis",
                   "Drosophila montana","Drosophila mulleri","Drosophila sechellia",
                   "Drosophila simulans","Drosophila virilis","Drosophila yakuba",
                   "Episyrphus balteatus","Lucilia cuprina","Nauphoeta cinerea",
                   "Ovis aries","Pan troglodytes","Symphalangus syndactylus",
                   "Chrysomya megacephala","Drosophila bipectinata","Musca domestica",
                   "Periplaneta americana")
treemafft$tip.label <- new_tiplabels

td <- as.treedata.table(tree = treemafft, data=d1)
td$dat$life

# try faceted plot again but with the BEAST tree
p <- ggtree(treemafft)
p2 <- facet_plot(p, panel="Difference in Lifespan",
                 data=td$dat,
                 geom=geom_point,
                 mapping=aes(x=life, color=sex))

p3 <- facet_plot(p2, panel="Difference in Genome Size",
                 data=td$dat,
                 geom=geom_segment, 
                 aes(x=0, xend=gs_diff, y=y, yend=y, color=sex), size=10)

p3 + theme_tree2() + geom_vline(xintercept = 0, alpha=0.5, linetype = "dashed")

# now lets remove the pesky outliers???
# the freaking sheep is so annoyinggggg
treemafft$tip.label
sub_treemafft <- drop.tip(treemafft, tip = 20) # removes pan troglodytes because we know that one is weird and wrong
sub_treemafft <- drop.tip(sub_treemafft, tip = 19) # removes ovis aries because it is so big we can't see anything else
sub_treemafft <- drop.tip(sub_treemafft, tip = 1) # removes Amblyomma cajennense because it is big
sub_treemafft$tip.label

p <- ggtree(sub_treemafft)
p2 <- facet_plot(p, panel="Difference in Lifespan",
                 data=td$dat,
                 geom=geom_point,
                 mapping=aes(x=life, color=sex))

p3 <- facet_plot(p2, panel="Difference in Genome Size",
                 data=td$dat,
                 geom=geom_segment, 
                 aes(x=0, xend=gs_diff, y=y, yend=y, color=sex), size=5)

p3 + theme_tree2() + geom_vline(xintercept = 0, alpha=0.5, linetype = "dashed")
ggsave("tree/facet_tree1.png") 
# there is one more to remove we just need to figure out what it was