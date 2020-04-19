# Quick plots of daily observations - similar to Jon's box plots,
# using scatterplots and a smoother. This is with randomly generated
# data so expecting the flat curves.
#
# Author: Graham.Williams@anu.edu.au
# Date: 20200419


library(readr)
library(stringi)
library(magrittr)
library(dplyr)
library(ggplot2)
library(tidyr)

dsname <- "random_timeseries"
dspath <- dsname %s+% ".csv"
random_timeseries <- read_csv(dspath)
ds <- get(dsname)

# We will consider positive patients only.

# Daily count.

pdf("daily_count.pdf", height=4, width=15)
ds %>%
  filter(result == "positive") %>%
  group_by(date) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=date, y=count)) +
  geom_line()
dev.off()

# Time series aggreations.

pdf("daily_obs.pdf", height=12, width=25)
ds %>%
  filter(result == "positive") %>%
  select(-URN, -time, -swabbed, -result, -previous, -clinical) %>%
  pivot_longer(-date, names_to="test") %>%
  ggplot(aes(x=date, y=value)) +
  geom_point(shape=".") +
  geom_smooth(method="gam", formula=y~s(x, bs="cs")) +
  facet_wrap(~test, scales="free") +
  labs(x=NULL, y=NULL)
dev.off()

