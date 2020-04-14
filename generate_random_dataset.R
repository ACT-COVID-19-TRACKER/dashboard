# Generate random data for testing visualisations.
#
# Author: Graham Williams
# Date: 20200414
#
# Work in progress.... Currently generating the time series
# observations for each patient, each has different number of days
# admitted, different admission date, with OBS observations each day.

library(wakefield)
library(readr)
library(stringi)
library(magrittr)

n <- 100 # Number of patients.
obs <- 5 # Number of observations per day.

# Fixed patient data - COMPLETE.

r_data_frame(n=n,
             URN=id_factor,
             givennames=name,
             surname=name,
             dob=dob(start=Sys.Date()-365*100, k=365*99),
             sex=sex,
             admdr=name,
             drno=r_sample(x=400000000:499999999),
             admscr=name,
             scrno=r_sample(x=400000000:499999999),
             diabetes=answer,
             hypertension=answer,
             bmigt30=answer,
             ihd=answer,
             copd=answer) %>%
  mutate(URN="u" %s+% URN) %T>%
  write_csv("random_fixed.csv") ->
patients

# Timeseries observations - IN PROGRESS

days <- r_sample(n, 5:20) # Number of days each patient is in hospital.

# Date admitted.

dadm <- date_stamp(n,
                   x = seq(as.Date("2020-03-15"), length = 30, by = "1 day"),
                   random=TRUE)

# Generate enough observations. Purely random.

r_data_frame(n=n*obs*days,
             swabbed=answer,
             result=r_sample_factor(x=c("penging", "positive", "negative", "na")),
             previous=answer(prob=c(0.8,0.2)),
             clinical=answer,
             hr=r_sample(40:150),
             bps=r_sample(80:250),
             bpd=r_sample(40:180),
             sao2=r_sample(85:100),
             rr=r_sample(5:35),
             temp=normal(37, 1.2),
             gcs=r_sample(0:30),
             wcc=normal(7,3),
             neutrophil=normal(4, 2),
             lymphocyte=normal(2.5, 2),
             ddimer =normal(3, 1),
             crp=normal(7, 4),
             ferritin=r_sample(7:270),
             troponin=normal(0.3, 0.1),
             pao2=r_sample(70:100),
             paco2=r_sample(35:45),
             ph=normal(7.4, 0.1),
             hco3=r_sample(19:25)) %T>%
  write_csv("random_timeseries.csv") ->
ds
