# Generate random data for testing visualisations.
#
# Author: Graham Williams
# Date: 20200415

library(wakefield)    # Generate random datasets.
library(readr)        # Modern and efficient data reader/writer.
library(stringi)      # String concat operator: %s+%.
library(magrittr)     # Data pipelines: %>% %<>% %T>% equals().
library(lubridate)    # Dates and time.
library(dplyr)

n <- 50 # Number of patients.
obs <- 1 # Number of observations per day.

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
             copd=answer,
             hosp=r_sample_factor(x=c("H1", "H2", "H3"))) %>%
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

  # Next refinement - for a single patient probably don't want so much
  # variation observation to observation.

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
               hco3=r_sample(12:30)) %>%  # 21-28 10-40
    mutate(MEWSrr=case_when(rr <= 4 | rr >= 36 ~ 4,
                           rr %in% c(5:8, 31:35) ~ 3,
                           rr %in% 25:30 ~ 2,
                           rr %in% 21:24 ~ 1,
                           rr %in% 9:20 ~ 0),
           MEWSsao2=case_when(sao2 <= 84 ~ 4,
                              sao2 %in% 85:89 ~ 3,
                              sao2 %in% 90:92 ~ 2,
                              sao2 %in% 93:94 ~ 1,
                              sao2 >= 95 ~ 0),
           MEWStemp=case_when(temp <= 34 ~ 3,
                              temp >= 34.1 & temp <= 35.0 ~ 2,
                              temp >= 35.1 & temp <= 36.0 ~ 1,
                              temp >= 36.1 & temp <= 37.9 ~ 0,
                              temp >= 38.0 & temp <= 38.5 ~ 1,
                              temp >= 38.6 ~ 2),
           MEWShr=case_when(hr <= 39 ~ 4,
                            hr %in% 40:49 ~ 1,
                            hr %in% 50:99 ~ 0,
                            hr %in% 100:109 ~ 1,
                            hr %in% 110:129 ~ 2,
                            hr %in% 130:139 ~ 3,
                            hr > 140 ~ 4),
           MEWS=MEWSrr+MEWSsao2+MEWShr+MEWStemp) ->
  otbl

  # Add to table of observations across all patients.
  
  observations %<>% bind_rows(cbind(ptbl, otbl))

}

# Exploring option to remove flip/flop of specific variables - might
# still want some variation within a patient though. 

## r_data_frame(n=n,
##              swabbed=answer,
##              result=r_sample_factor(x=c("pending", "positive", "negative", "na"))) %>%
##   mutate(result=factor(ifelse(swabbed == "Yes", result, "na"), labels=c("pending", "positive", "negative", "na", "na"))) ->
## potbl

## cbind(potbl[rep(1:n, obs*days),], observations)

write_csv(format(observations), "random_timeseries.csv")

