#### trying out the normalized genome-size stuff ####

library(tidyverse)
library(janitor)
library(ggtree)


# load in the data
df <- read_csv("analysis_data.csv") %>% 
  janitor::clean_names() %>% 
  mutate_at('ln_r_rlifespan', as.numeric)
# subset data to our focus data
dl <- 
  df %>% 
  filter(ln_r_rlifespan != "NA")
dg <- 
  dl %>% 
  filter(gs_diff_mb != "NA")
# change the female heterogametic gs_diff_mb equations
dg$gs_diff_mb <- ifelse(dg$sex_determination == "female heterogametic", -dg$gs_diff_mb, dg$gs_diff_mb)
# lets add a column that gives the average total genome 
dg %>% names
dg$gs_ave <- (dg$gs_female_mb + dg$gs_male_mb)/2
# lets add a column that gives the proportion of difference:average
dg$gs_prop <- dg$gs_diff_mb/dg$gs_ave

write.csv(dg,"gs_test_data.csv", row.names = FALSE)

# now what does plotting the proportion by the normalized lifespan difference do???
dg %>% 
  ggplot(aes(x=gs_prop, y=ln_r_rlifespan, color=sex_determination)) +
  geom_smooth(method="lm",se=FALSE, color='gray', linetype=2, size=0.5) +
  geom_point() +
  theme_minimal()
# honestly the data does not look very different...
# how is there one with a difference that is near 0.75 of the total genome size??? It's Ovis aries man. 
# get rid of outliers (proportions less than 0.25) - not official
dg[dg$gs_prop < 0.25,] %>% 
  ggplot(aes(x=gs_prop, y=ln_r_rlifespan, color=sex_determination)) +
  geom_smooth(method="lm", color='black', size=0.5) +
  geom_point() +
  theme_minimal()
# similar to the absolute gs graph, there is just a cloud of points with no correlation

# let's plot the proportion data alongside our tree and lifespan data like we've done before
df <- read_csv("gs_test_data.csv")

d1 <- data.frame(correct_species = df$species,
                 life=df$ln_r_rlifespan,
                 gs_prop=df$gs_prop,
                 sex=df$sex_determination)

treemafft <- read.nexus("tree/first_tree") # better tree made with Dr. Hjelmen
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

td$dat

p <- ggtree(treemafft)
p2 <- facet_plot(p, panel="Difference in Lifespan",
                 data=td$dat,
                 geom=geom_point,
                 mapping=aes(x=life, color=sex))

p3 <- facet_plot(p2, panel="Difference in Genome Size",
                 data=td$dat,
                 geom=geom_segment, 
                 aes(x=0, xend=gs_prop, y=y, yend=y, color=sex), size=10)

p3 + theme_tree2() + geom_vline(xintercept = 0, alpha=0.5, linetype = "dashed")

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
                 aes(x=0, xend=gs_prop, y=y, yend=y, color=sex), size=5)

p3 + theme_tree2() + geom_vline(xintercept = 0, alpha=0.5, linetype = "dashed")


