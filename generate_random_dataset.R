# Generate random data for testing visualisations.
#
# Author: Graham Williams
# Date: 20200415

library(wakefield)
library(readr)
library(stringi)
library(magrittr)
library(lubridate)

n <- 100 # Number of patients.
obs <- 2 # Number of observations per day.

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

# Timeseries observations.

days <- r_sample(n, 5:20) # Number of days each patient is in hospital.

# Date admitted.

dadm <- date_stamp(n,
                   x = seq(as.Date("2020-03-15"), length = 30, by = "1 day"),
                   random=TRUE)

# Generate enough observations. Purely random.

observations <- NULL

for (p in seq(n))
{
  nobs <- days[p] * obs # Observations of this patient during admission.
  URN <- rep(patients[[p,'URN']], nobs)  # Patient ID
  dt <- rep(seq(ymd(dadm[p])+1, ymd(dadm[p])+days[p], by='1 day'), each=obs)
  ts <- format(time_stamp(nobs, random=TRUE, prob=probs(24)))

  # Patient tibble.
  
  tibble(URN=URN, date=dt, time=ts) %>%
    group_by(date) %>%
    arrange(time, .by_group=TRUE) %>%
    ungroup() ->
  ptbl

  # Observations for this patient.
  
  r_data_frame(n=nobs,
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
               hco3=r_sample(19:25)) ->
  otbl

  # Add to table of observations across all patients.
  
  observations %<>% bind_rows(cbind(ptbl, otbl))

}

write_csv(format(observations, digits=2), "random_timeseries.csv")

