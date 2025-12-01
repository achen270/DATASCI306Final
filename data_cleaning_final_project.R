
library(haven)
library(tidyverse)
cardio_health <- read_xpt("/Users/brandonbuckner/Desktop/P_CDQ.xpt")
diabetes <- read_xpt("/Users/brandonbuckner/Desktop/P_DIQ.xpt")
before_covid <- cardio_health |> left_join(diabetes, by = "SEQN")

pre <- read_xpt('/Users/brandonbuckner/Desktop/P_MCQ.xpt')
post <- read_xpt('/Users/brandonbuckner/Desktop/MCQ_L.xpt')
post_demo <- read_xpt('/Users/brandonbuckner/Desktop/DEMO_L.xpt')
pre_demo <- read_xpt('/Users/brandonbuckner/Desktop/P_DEMO.xpt')

pre <- pre |> mutate(post_covid = FALSE)
post <- post|> mutate(post_covid = TRUE)
med_conditions <- bind_rows(pre, post)
med_conditions_clean <- med_conditions[c("SEQN", "post_covid", "MCQ010", "AGQ030", "MCQ053",
                                         "MCQ160A", "MCQ160B", "MCQ160C", "MCQ160D", "MCQ160E",
                                         "MCQ160F", "MCQ160M", "MCQ160P", "MCQ160L", "MCQ550",
                                         "MCQ220")] 

med_conditions_clean <- med_conditions_clean |> rename(
  asthma = MCQ010,
  hay_fever_past_yr = AGQ030,
  anemia_past_3_mon = MCQ053,
  #overweight = MCQ080,
  #blood_transfusion = MCQ092,
  arthritis = MCQ160A,
  congestive_heart_failure = MCQ160B,
  coronary_heart_disease = MCQ160C,
  angina = MCQ160D,
  heart_attack = MCQ160E,
  stroke = MCQ160F,
  thyroid_prob = MCQ160M,
  COPD_Emph_ChB = MCQ160P,
  liver_cond = MCQ160L,
  gallstones = MCQ550,
  cancer = MCQ220
)

demo <- bind_rows(pre_demo, post_demo)
demo_clean <- demo[c('SEQN', 'RIDAGEYR', 'RIDRETH3', 'RIAGENDR')]

demo_clean <- demo_clean |> rename(
  Age_yr = RIDAGEYR,
  Race = RIDRETH3,
  Gender = RIAGENDR,
)

med_conditions_demo <- demo_clean |> left_join(med_conditions_clean, by = "SEQN")
med_conditions_demo |> filter(asthma %in% c(1,2)) |> summarize(prop_asthma = mean(asthma == 1))

save(med_conditions_demo, file = "NHANES_Medical_Conditions_Pre_Post_Covid.RData")

     