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
               result=r_sample_factor(x=c("pending", "positive", "negative", "na")),
               previous=answer(prob=c(0.8,0.2)),
               clinical=answer,
               hr=r_sample(60:299, prob=c(rep(0.02, 40), rep(0.001, 200))), # 60-100 0-300+
               bps=r_sample(90:135), # 90-135 0-200+
               bpd=r_sample(60:130), # 60-90 0-130+
               sao2=r_sample(85:100), # 95-100 0-100
               rr=r_sample(5:35), # 12-20 0-40+
               temp=normal(36.3, 1.2), # 35.7-37.5 34-46+
               gcs=r_sample(3:15, prob=c(rep(0.01, 12), 0.88)), # 15 3-15
               wcc=normal(7,3, min=0), # 3.5-10 0-20+
               neutrophil=normal(4, 2, min=0), # 1.5-6.5 0-10+
               lymphocyte=normal(2.5, 2, min=0), # 1.0-4.0 0-10+
               ddimer =normal(0.3, 0.25, min=0), # 0-0.5 0-5
               crp=normal(2.5, 2, min=0, max=200), # 0-5 0-200+
               ferritin=r_sample(7:270), # 0-200+
               troponin=normal(0.02, 0.01, min=0, max=10), # 0-0.03 0-10
               pao2=r_sample(70:105), # 70-100 0-100+  FIXME
               paco2=r_sample(25:45),
               ph=normal(7.4, 0.1, min=7.2, max=7.6), # 7.35-7.45 7.2-7.6
               hco3=r_sample(12:30)) -> # 21-28 10-40
  otbl

  # Add to table of observations across all patients.
  
  observations %<>% bind_rows(cbind(ptbl, otbl))

}

write_csv(format(observations), "random_timeseries.csv")

