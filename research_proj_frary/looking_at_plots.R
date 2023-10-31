library(tidyverse)
library(readr)
library(janitor)

df <- read_csv("analysis_data.csv") %>% 
  clean_names

df %>% names

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