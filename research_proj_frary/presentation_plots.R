library(tidyverse)
library(janitor)
library(ggthemes)

# nice poster plots
df <- read_csv("analysis_data.csv") %>% 
  janitor::clean_names() %>% 
  mutate_at('ln_r_rlifespan', as.numeric)

dl <- 
  df %>% 
  filter(ln_r_rlifespan != "NA")

p1 <-  dl %>% 
  ggplot(aes(x=ln_r_rlifespan,y=species, fill=class)) +
  geom_point(aes(color=class, fill=class), 
             shape = 21,size = 2,color = "black") +
  geom_vline(xintercept = 0) +
  facet_wrap(~class) +
  labs(x = "log ratio of lifespan\nln(homogametic/heterogametic)", y = "Species") +
  theme_test() +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

p2 <- dl %>% 
  filter(class == c("Aves","Insecta","Mammalia")) %>% 
  ggplot(aes(x=ln_r_rlifespan,y=species, fill=class)) +
  geom_point(aes(color=class, fill=class), 
             shape = 21,size = 2,color = "black") +
  geom_vline(xintercept = 0) +
  facet_wrap(~class) +
  labs(x = "log ratio of lifespan\nln(homogametic/heterogametic)", y = "Species") +
  theme_test() +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

dl %>% 
  ggplot(aes(x=ln_r_rlifespan,y=species, color=class)) +
  geom_point(size=2,alpha=.7) +
  geom_vline(xintercept = 0) +
  facet_wrap(~class) +
  labs(x = "log ratio of lifespan\nln(homogametic/heterogametic)", y = "Species") +
  theme_test() +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  scale_color_brewer(palette="Dark2")

ggsave("bg.png")

# used in poster
dl %>% 
  filter(class == c("Aves","Insecta","Mammalia")) %>% 
  ggplot(aes(x=ln_r_rlifespan,y=species, color=class)) +
  geom_point(size = 2, alpha=.7) +
  geom_vline(xintercept = 0) +
  facet_wrap(~class) +
  labs(x = "log ratio of lifespan\nln(homogametic/heterogametic)", y = "Species") +
  theme_test() +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  scale_color_brewer(palette="Dark2")

ggsave("bg_short.png")

dg <- 
  dl %>% 
  filter(gs_diff_mb != "NA")
dg$gs_diff_mb <- ifelse(dg$sex_determination == "female heterogametic", -dg$gs_diff_mb, dg$gs_diff_mb)

dg %>% 
  ggplot(aes(x=gs_diff_mb, y=ln_r_rlifespan, color=sex_determination)) +
  geom_smooth(method="lm",se=FALSE, color='gray', linetype=2, size=0.5) +
  geom_point() +
  theme_minimal()

# used in poster
dg[dg$gs_diff_mb < 250 & dg$gs_diff_mb > -250,] %>% 
  ggplot(aes(x=gs_diff_mb, y=ln_r_rlifespan, color=sex_determination)) +
  geom_smooth(method="lm", se=FALSE , color='gray', linetype=2, size=0.5) +
  geom_point(size=2,alpha=.7) +
  labs(x="Absolute Difference in Genome Size (Mb)", y="log ratio of lifespan\nln(homogametic/heterogametic)") +
  theme_minimal()+
  scale_color_brewer(palette="Dark2")
ggsave("compare.png")
