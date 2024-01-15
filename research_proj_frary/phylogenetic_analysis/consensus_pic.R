# conduct a pic analysis on coi consensus tree
# pic means phylogenetically independent contrasts
library(ape)
library(treedata.table)

# load in the data file
data <- read.csv("gs_test_data.csv")
# load in the consensus tree
tree <- read.nexus("trees_genes/coi_consensus")

# old and ugly solution - rewrite this to just remove '_' and capitalize first letter, still have to change the reverse ones though...
new_tiplabels <- c("Amblyomma cajennense","Aphelocoma coerulescens","Callosobruchus maculatus",
                   "Cochliomyia hominivorax","Drosophila ananassae","Drosophila erecta",
                   "Drosophila kikkawai","Drosophila melanogaster","Drosophila mojavensis",
                   "Drosophila montana","Drosophila mulleri","Drosophila sechellia",
                   "Drosophila simulans","Drosophila virilis","Drosophila yakuba",
                   "Episyrphus balteatus","Lucilia cuprina","Nauphoeta cinerea",
                   "Ovis aries","Pan troglodytes","Symphalangus syndactylus",
                   "Chrysomya megacephala","Drosophila bipectinata","Musca domestica",
                   "Periplaneta americana")
tree$tip.label <- new_tiplabels

# Save a tree with these tip labels cuz they're nice. **************

# pic will assume the data is in the same order as the tree unless x has names
# ours doesn't so we're gonna reorder the data to match the tip labels
td <- as.treedata.table(tree=tree, data=data)

# try pic - I think this initial one should be the response variable
lifespan_pic<-pic(td$dat$ln_r_rlifespan, phy=tree) # previous error maybe because issue with negatives and the log
# well it worked? but I don't really understand the values or how to use them now

# make a summary statistics table?
output.table<-matrix(,nrow=2, ncol=8)
colnames(output.table)<-c("Variable", "Estimate", "St. Error", "t-value", "p-value", "r-square",
                          "Adj. r-square", "F-stat")
output.table[1,1]<-names(data[13]) # populate first column with the names of your variables
output.table[2,1]<-names(data[15])

# get pic for predictor
gsdiff_pic<-pic(td$dat$gs_diff_mb, phy=tree)
# linear model of the pic, response as a function of the predictor + 0 to set intercept
picmod<-lm(lifespan_pic~gsdiff_pic+0)

testing<-summary(picmod)
#estimate
output.table[1,2]<-testing$coefficients[1]
#st error
output.table[1,3]<-testing$coefficients[2]
#t-value
output.table[1,4]<-testing$coefficients[3]
#p-value
output.table[1,5]<-testing$coefficients[4]
#r-square
output.table[1,6]<-testing$r.squared
#adj. R square
output.table[1,7]<-testing$adj.r.squared[1]
#f stat
output.table[1,8]<-testing$fstatistic[1]

# SECOND VARIABLE FOR FUNSIES
gsprop_pic<-pic(td$dat$gs_prop, phy=tree)
# linear model of the pic, response as a function of the predictor + 0 to set intercept
picmod2<-lm(lifespan_pic~gsprop_pic+0)

testing<-summary(picmod2)
#estimate
output.table[2,2]<-testing$coefficients[1]
#st error
output.table[2,3]<-testing$coefficients[2]
#t-value
output.table[2,4]<-testing$coefficients[3]
#p-value
output.table[2,5]<-testing$coefficients[4]
#r-square
output.table[2,6]<-testing$r.squared
#adj. R square
output.table[2,7]<-testing$adj.r.squared[1]
#f stat
output.table[2,8]<-testing$fstatistic[1]

# NEXT STEPS
# make the program more concise
# watch a video on pic to understand how you want to use it
# see if you can get better results, if no -> ask hjelmen about em

# figures OG and pic adjusted
data %>% 
  ggplot(aes(x=gs_diff_mb, y=ln_r_rlifespan, color=sex_determination)) +
  geom_smooth(method="lm", se=FALSE , color='gray', linetype=2, size=0.5) +
  geom_point(size=2,alpha=.7) +
  labs(x="Absolute Difference in Genome Size (Mb)", y="log ratio of lifespan\nln(homogametic/heterogametic)") +
  theme_minimal()+
  scale_color_brewer(palette="Dark2")

plot(gsdiff_pic,lifespan_pic,xlab="PICs for difference in genome size",
     ylab="PICs for difference in lifespan",bg="grey",
     cex=1.4,pch=21)
abline(picmod,lwd=2,lty="dashed",col="red")



#### example ####

#PGLS through caper on BIO 1
#we need this to prevent an error with the next function dealing with node names and tip label
#tree1$node.label <- NULL
#bio1_pgls1 <- comparative.data(tree1, data1, Species, vcv = TRUE)
#make empty matrix
output.table<-matrix(,nrow=21, ncol=8)
colnames(output.table)<-c("Variable", "Estimate", "St. Error", "t-value", "p-value", "r-square",
                          "Adj. r-square", "F-stat")
# corBM<-corBrownian(phy=tree1, ~data1$Species)
# gs<-data1$GS_ave
# names(gs)<-data1$Species
# mta<-data1$Mean_Temp_Annual
# names(mta)<-data1$Species
#str(data1)
gspic<-pic(log(data1$GS_ave), phy=tree1)
#i<-3
#loop to fill matrix
for(i in 3:24){
  output.table[(i-2),1]<-names(data1[i])
  #var2<-data1[[i]]
  #names(var2)<-data1[[1]]
  #mod1pgls <- gls(log(gs) ~ log(var2), correlation=corBM)
  varpic<-pic(log(data1[[i]]), phy=tree1)
  picmod<-lm(gspic~varpic+0)
  testing<-summary(picmod)
  #estimate
  output.table[(i-2),2]<-testing$coefficients[1]
  #st error
  output.table[(i-2),3]<-testing$coefficients[2]
  #t-value
  output.table[(i-2),4]<-testing$coefficients[3]
  #p-value
  output.table[(i-2),5]<-testing$coefficients[4]
  #r-square
  output.table[(i-2),6]<-testing$r.squared
  #adj. R square
  output.table[(i-2),7]<-testing$adj.r.squared[1]
  #f stat
  output.table[(i-2),8]<-testing$fstatistic[1]
}

write.csv(output.table, "leucaena_gs_pic_results.csv")

#make a dataframe of our matrix
testing.table<-as.data.frame(output.table)

#making our new dataframe as numeric
for(i in 2:8){
  testing.table[[i]]<-as.numeric(testing.table[[i]])
}

#get list of models that are significant at 0.01 level
sigresults<-testing.table$Variable[testing.table$`p-value`< 0.01]
