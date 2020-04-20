# Quick plots of daily observations - similar to Jon's box plots,
# using scatterplots and a smoother. This is with randomly generated
# data so expecting the flat curves.
#
# Author: Graham.Williams@anu.edu.au
# Date: 20200419

ALLPLOTS=FALSE

library(readr)
library(stringi)
library(magrittr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggbeeswarm)

dsname <- "random_timeseries"
dspath <- dsname %s+% ".csv"
random_timeseries <- read_csv(dspath)
ds <- get(dsname)

# Need to focus visualisations on personas. This PERSONA might be
# CLINCIAL OVERVIEW?

# We will consider positive patients only.

# Daily count.

if (ALLPLOTS)
{
pdf("daily_count.pdf", height=4, width=15)
ds %>%
  filter(result == "positive") %>%
  group_by(date) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=date, y=count)) +
  geom_line()
dev.off()
}

# Time series aggregations. Could use colour bands for normal, and
# abnormal ranges as per the sample patient sheet from Nikhil.

if (ALLPLOTS)
{
pdf("daily_obs.pdf", height=12, width=25)
ds %>%
  filter(result == "positive") %>%
  select(-URN, -time, -swabbed, -result, -previous, -clinical,
         -MEWSrr, -MEWSsao2, -MEWStemp, -MEWShr) %>%
  pivot_longer(-date, names_to="test") %>%
  ggplot(aes(x=date, y=value)) +
  geom_point(shape=".") +
  geom_smooth(method="gam", formula=y~s(x, bs="cs")) +
  facet_wrap(~test, scales="free") +
  labs(x=NULL, y=NULL)
dev.off()
# system("evince daily_obs.pdf")
}

# Focus on MEWS

fname <- "daily_mews.pdf"
pdf(fname, width=15) #height=12, width=25)
ds %>%
  filter(date >= "2020-04-10") %>%
  select(date, MEWS) %>%
  mutate(MEWS=as.factor(MEWS)) %>%
  pivot_longer(-date, names_to="test") %>%
  drop_na() %>%
  ggplot(aes(x=date, y=value, colour=value)) +
#  geom_quasirandom(method="smiley") + 
  geom_quasirandom(method="tukey") + 
#  geom_beeswarm() +
#  geom_quasirandom() +
#  geom_jitter() +
#  geom_point() +
  facet_wrap(~test, scales="free") +
  scale_color_brewer(palette="Reds") +
  theme(legend.position="none") +
  labs(x=NULL, y=NULL)
dev.off()
system("evince daily_mews.pdf")
