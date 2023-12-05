library(tidyverse)
library(readr)
library(janitor)
library(skimr)
library(GGally)
library(shiny)
library(modelr)
library(MASS)
library(easystats)
library(plotly)

#### load in and check out the data ####
# read in the data
df <- read_csv("analysis_data.csv") %>% 
  janitor::clean_names() %>% 
  mutate_at('ln_r_rlifespan', as.numeric)

head(df) # take a look at the variables

names(df) # remember the names for later entry

glimpse(df) # shows variables and data types
skim(df) # shows distribution of data... not sure I will use this except for critique
# ggpairs(df), there are too many species for this plot glance to be useful


#### looking at lifespan data with larger subset ####
# take a look at the available lifespan data
dl <- 
df %>% 
  filter(ln_r_rlifespan != "NA")

# the response variables we are interested in are to do with lifespan
# the possible predictors are species, sex_determination, population_source, 
# and er
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

#### Lifespan subset models ####
# lets try some models 

dl %>% names # what we have to work with

# cor(d1$sex_determination,d1$ln_r_rlifespan)
# not valid because sex_determination is categorial/binary

m1 <- glm(data=dl, formula = ln_r_rlifespan ~ sex_determination)
summary(m1) # suggests that the sex_determination predictor is statistically significant
# plot(m1) # most data is normally distributed - though this makes sense due to the log transformation

# lets make a horrible overfit model for funsies
m2 <- glm(data=dl, formula = ln_r_rlifespan ~ sex_determination*population_source*lifespan_data_type)
compare_performance(m1,m2, rank = TRUE)
# we get a warning messages that has to do with varying rows of NAs, ignoring for now
step <- stepAIC(m2)
# AIC will find the simplelist best model
step$formula # this spits out the best simplified model
mod_best <- glm(data=dl, formula = step$formula)
s <- compare_performance(m1,m2,mod_best, rank = TRUE)
summary(mod_best) # sex determination seems to have the best affect
s %>% 
  select(c('Model','R2','RMSE','AIC_wt','Performance_Score'))

# add predictions to dl
dl %>% 
  gather_predictions(mod_best) %>% 
  ggplot(aes(x=pred,y=species, color=sex_determination)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  theme(axis.text.y = element_text(size=7))
  
dl %>% 
  gather_predictions(mod_best) %>% view

# this is really weird let's try the smaller subset cuz what


# species, sex_determination, population_source, 
# and lifespan_data_type

#### looking at more specific genome data ####
# let's take a look at plotting the difference in genome size based on sex determination
dg <- 
  dl %>% 
  filter(gs_diff_mb != "NA")

# fix the values for female heterogametic species so that they show homogametic - heterogametic
dg$gs_diff_mb <- ifelse(dg$sex_determination == "female heterogametic", -dg$gs_diff_mb, dg$gs_diff_mb)

write.csv(dg,"short_complete_data.csv", row.names = FALSE) # to use in other scripts

names <- dg$species
names <- as.data.frame(names)

write.csv(names,"lifespan_species.csv", row.names = FALSE) # export limited species for use in phylogeny


# Create a scatter plot with a linear regression line
p <- dg %>% 
  ggplot(aes(x=ln_r_rlifespan, y=gs_diff_mb, color=sex_determination)) +
  geom_smooth(method="lm",se=FALSE, color='gray', linetype=2, linewidth=0.5) +
  geom_point() +
  theme_minimal()
ggplotly(p)

p <- dg %>% 
  ggplot(aes(x=gs_diff_mb, y=ln_r_rlifespan, color=sex_determination)) +
  geom_smooth(method="lm",se=FALSE, color='gray', linetype=2, size=0.5) +
  geom_point() +
  theme_minimal()
ggplotly(p)

# explain the line seen above
lm_mod <- lm(gs_diff_mb ~ ln_r_rlifespan, dg)
lm_mod1 <- lm(ln_r_rlifespan ~ gs_diff_mb, dg)
sum_lm_mod <- summary(lm_mod)
sum_lm_mod$r.squared # 0.04240
# equation: y = 1.1276 + 0.0002x
plot(lm_mod1)

# ignoring the points that reside outside of 100Mb difference on each side.
dg[dg$gs_diff_mb < 100 & dg$gs_diff_mb > -100,] %>% 
  ggplot(aes(x=ln_r_rlifespan, y=gs_diff_mb, color=sex_determination)) +
  geom_smooth(method="lm", se=FALSE , color='gray', linetype=2, size=0.5) +
  geom_point() +
  labs(x="Difference in Lifespan", y="Difference in Genome Size") +
  theme_minimal()

# this plot is set so that the predictor is on the x-axis, and response is on y-axis
p <- dg[dg$gs_diff_mb < 250 & dg$gs_diff_mb > -250,] %>% 
  ggplot(aes(x=gs_diff_mb, y=ln_r_rlifespan, color=sex_determination)) +
  geom_smooth(method="lm", se=FALSE , color='gray', linetype=2, size=0.5) +
  geom_point() +
  labs(x="Difference in Genome Size", y="Difference in Lifespan") +
  theme_minimal()
ggplotly(p)

dg1 <- dg[dg$gs_diff_mb < 250 & dg$gs_diff_mb > -250,]
lm_mod2 <- lm(ln_r_rlifespan ~ gs_diff_mb, dg1)
sum_lm_mod2 <- summary(lm_mod2)
sum_lm_mod2$r.squared # 0.005538
# equation: y = 0.1276 + 0.0004x
plot(lm_mod2)

dg2 <- dg[dg$gs_diff_mb < 100 & dg$gs_diff_mb > -100,]
lm_mod3 <- lm(ln_r_rlifespan ~ gs_diff_mb, dg2)
sum_lm_mod3 <- summary(lm_mod3)
sum_lm_mod3$r.squared # 0.006046
# equation: y = 0.1468 - 0.0009x

m <- glm(data=dg1, formula= ln_r_rlifespan ~ gs_diff_mb)
summary(m)

#complicated model
mess <- glm(data = dg1,
            formula = ln_r_rlifespan ~ gs_diff_mb*sex_determination)
step <- stepAIC(mess)
# AIC will find the simplelist best model
step$formula # this spits out the best simplified model
mod_best <- glm(data=dl, formula = step$formula)
compare_performance(m,mess,mod_best, rank = TRUE)

dg1 %>%
  gather_predictions(m) %>% 
  ggplot(aes(x=gs_diff_mb, y=pred, color=sex_determination)) +
  geom_point() +
  theme_minimal()

#### old stuff idk what is here ####
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


