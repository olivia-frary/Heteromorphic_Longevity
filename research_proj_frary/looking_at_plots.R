library(tidyverse)
library(readr)
library(janitor)
library(skimr)
library(GGally)
library(shiny)


# read in the data
df <- read_csv("analysis_data.csv") %>% 
  clean_names %>% 
  mutate_at('ln_r_rlifespan', as.numeric)

head(df) # take a look at the variables

names(df) # remember the names for later entry

glimpse(df) # shows variables and data types
skim(df) # shows distribution of data... not sure I will use this except for critique
# ggpairs(df), there are too many species for this plot glance to be useful

# take a look at the available lifespan data
dl <- 
df %>% 
  filter(ln_r_rlifespan != "NA")

# the response variables we are interested in are to do with lifespan
# the possible predictors are species, sex_determination, population_source, 
# and lifespan_data_type
# as we move further to the smaller data set we will look at the difference in
# size between the heteromorphic sex chromosomes

# this plot shows the difference in lifespan based on species
# the more positive the value the greater the difference between the homogametic
# sex and heterogametic sex.

# try to re order the species by family - so far no luck with reorder or sort
dl %>% 
  ggplot(aes(x=ln_r_rlifespan,y=species, color=sex_determination)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  theme(axis.text.y = element_text(size=7))
# I would ideally like to add a phylogeny along the side of this plot
# but for now, here is a shiny app of this graph with interactive colors
# Shiny App
# runApp("longevity")

# let's check out if there seems to be relationships between lifespan and any
# of the other data
# this plot is another way of illustrating how much of the lifespan difference
# resides -above- zero.
dl %>% 
  ggplot(aes(x=sex_determination,y=ln_r_rlifespan, fill=sex_determination)) +
  geom_violin() +
  theme_minimal()

# consider doing sex determination as a logical - true for female heterogametic
# false for male heterogametic, then the models would be family = 'binomial'

# lets try some models 
m1 <- glm(data=dl, formula = ln_r_rlifespan ~ sex_determination)
summary(m1)
plot(m1) # most data is normally distributed - though this makes sense due to the log transformation

m2 <- glm(data=dl, ln_r_ )

#### NOTE: could have homogametic Mb and heterogametic Mb so the difference is (homogametic*2 - heterogametic*2)
# let's take a look at plotting the difference in genome size based on sex determination
dg <- 
  dl %>% 
  filter(gs_diff_mb != "NA")

write.csv(dg,"short_complete_data.csv", row.names = FALSE)

names <- dg$species
names <- as.data.frame(names)

write.csv(names,"lifespan_species.csv", row.names = FALSE) # export limited species for use in phylogeny

# ASK CHATGPT
# write something that says 
# if sex_determination == "female heterogametic":
#     gs_diff_mb = (gs_male_mb*2) - (gs_female_mb*2)
# dg[dg$sex_determination == "female heterogametic",]$gs_diff_mb = -df$gs_diff_mb
# BAD:dg$gs_diff_mb <- ifelse(dg$sex_determination == "female heterogametic", -1, 1)
dg$gs_diff_mb <- ifelse(dg$sex_determination == "female heterogametic", -df$gs_diff_mb, df$gs_diff_mb)

dg %>% 
  ggplot(aes(x=ln_r_rlifespan, y=gs_diff_mb, color=sex_determination)) +
  geom_smooth(method="lm",se=FALSE, color='gray', linetype=2, size=0.5) +
  geom_point() +
  labs(x="Difference in Lifespan", y="Difference in Genome Size") +
  theme_minimal()

# ignoring the points that reside outside of 100bp difference on each side.
# still need to fix the female heterogametic sign
dg[dg$gs_diff_mb < 100 & dg$gs_diff_mb > -100,] %>% 
  ggplot(aes(x=ln_r_rlifespan, y=gs_diff_mb, color=sex_determination)) +
  geom_smooth(method="lm", se=FALSE , color='gray', linetype=2, size=0.5) +
  geom_point() +
  labs(x="Difference in Lifespan", y="Difference in Genome Size") +
  theme_minimal()




# basic plots of differences
df %>% 
  ggplot(aes(x=as.numeric(ln_r_rlifespan),y=abs(gs_diff_mb))) +
  geom_point() +
  geom_smooth(method="lm")
df %>% 
  ggplot(aes(x=abs(gs_diff_mb),y=as.numeric(ln_r_rlifespan))) +
  geom_point() +
  geom_smooth(method="lm")

glm(data=df, formula=ln_r_rlifespan~gs_diff_mb)

# random plots to look at data
# pull out just the female heterogametic species
df %>% 
  filter(sex_determination == "female heterogametic") %>% 
  ggplot(aes(x=as.numeric(ln_r_rlifespan),y=abs(gs_diff_mb))) +
  geom_point() +
  geom_smooth(method="lm")

# pull out just the male heterogametic species
df %>% 
  filter(sex_determination == "male heterogametic") %>% 
  ggplot(aes(x=as.numeric(ln_r_rlifespan),y=abs(gs_diff_mb))) +
  geom_point() +
  geom_smooth(method="lm")

# plot the og data that doesn't compare differences in genome size
df %>% 
  ggplot(aes(x=as.numeric(ln_r_rlifespan), y=species)) +
  geom_point() + 
  facet_wrap(~sex_determination) +
  geom_vline(xintercept = 0, 
             color = "blue", size = 1)
df %>% 
  ggplot(aes(x=as.numeric(ln_r_rlifespan), y=species, color=sex_determination)) +
  geom_point() +
  geom_vline(xintercept = 0, size = 1)

# look at removing outliers
# df %>% 
#   ggplot(aes(y=gs_diff_mb)) + 
#   geom_boxplot()

# Shiny App
# library(shiny)
# runApp("longevity")
# display.mode = "showcase" will show the code associated with the app

# consider creating a "best model" of the variables in your source data


