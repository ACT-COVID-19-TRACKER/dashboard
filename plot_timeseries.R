# Quick plots of daily observations - similar to Jon's box plots,
# using scatterplots and a smoother. This is with randomly generated
# data so expecting the flat curves.
#
# Author: Graham.Williams@anu.edu.au
# Date: 20200419

ALLPLOTS=FALSE
ALLPLOTS=TRUE

library(readr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggbeeswarm)
library(glue)

dsname <- "random_timeseries"
dspath <- glue("{dsname}.csv")
random_timeseries <- read_csv(dspath)
ds <- get(dsname)

# Need to focus visualisations on personas. This PERSONA might be
# CLINCIAL OVERVIEW?

# We will consider positive patients only.

# Daily count.

if (ALLPLOTS)
{
fname <- "daily_count.pdf"
pdf(fname, height=4, width=15)
ds %>%
  group_by(date) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=date, y=count)) +
  geom_line()
dev.off()
system(glue("evince {fname}"))
}

# Time series aggregations. Could use colour bands for normal, and
# abnormal ranges as per the sample patient sheet from Nikhil.

if (ALLPLOTS)
{
fname <- "daily_obs.pdf"
pdf(fname, height=12, width=25)
ds %>%
  select(-URN, -time, -swabbed, -result, -previous, -clinical, -sedation) %>%
  pivot_longer(-date, names_to="test") %>%
  ggplot(aes(x=date, y=value)) +
  geom_point(shape=".") +
  geom_smooth(method="gam", formula=y~s(x, bs="cs")) +
  facet_wrap(~test, scales="free") +
  labs(x=NULL, y=NULL)
dev.off()
system(glue("evince {fname}"))
}

# Focus on MEWS

if (ALLPLOTS)
{
fname <- "daily_mews.pdf"
pdf(fname, width=15) #height=12, width=25)
ds %>%
  filter(date >= "2020-04-10") %>%
  select(date, MEWS) %>%
  mutate(MEWS=ifelse(MEWS>8,8,MEWS)) %>%
  mutate(MEWS=as.factor(MEWS)) %>%
  pivot_longer(-date, names_to="test") %>%
  drop_na() %>%
  ggplot(aes(x=date, y=value, colour=value)) +
#  geom_quasirandom(method="smiley") + 
  geom_quasirandom(method="tukey", groupOnX=FALSE) + 
#  geom_beeswarm() +
#  geom_quasirandom() +
#  geom_jitter() +
#  geom_point() +
  facet_wrap(~test, scales="free") +
  scale_color_brewer(palette="Reds") +
  theme(legend.position="none") +
  labs(x=NULL, y=NULL)
dev.off()
system(glue("evince {fname}"))
}

# MEWS and Beds - UNDER DEVELOPMENT

if (FALSE)
{
fname <- "daily_beds.pdf"
pdf(fname, width=15) #height=12, width=25)
ds %>%
  filter(date >= "2020-04-10") %>%
  select(date, MEWS) %>%
  drop_na() %>%
  group_by(date, MEWS) %>%
  summarise(n=n()) %>%
  mutate(MEWS=as.factor(MEWS)) %>%
  ggplot(aes(x=date, y=n, colour=MEWS)) +
  geom_bar() +
  theme(legend.position="none") +
  labs(x=NULL, y=NULL)
dev.off()
system("evince daily_mews.pdf")
}

