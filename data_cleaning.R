# R script (cleaning) for reactive phenotype 
# environment correlation manuscript

library(tidyverse)

# ------------------ CLEANING DATA FOR EXPERIMENT 1 ------------------------ #

RPEC_1_data <- read.csv("exp_1_rawdata.csv")
summary(RPEC_1_data)

# Several variables are stored as characters, which I don't want. I'm going to change these
# into factors.

RPEC_1_data <- (RPEC_1_data %>% mutate(across(where(is.character), as.factor)))

# participant.id is stored as a continuous predictor, when I want it to be a factor instead.

RPEC_1_data <- (RPEC_1_data %>% mutate(participant.ID = as.factor(participant.ID)))

summary(RPEC_1_data)

# also need to change the levels of "gender" to have periods instead of spaces.

RPEC_1_data <- (RPEC_1_data
           %>% mutate(gender = fct_recode(gender,
                      "cisgender.man" = "cisgender man",
                      "cisgender.woman" = "cisgender woman" ,
                      "pnts" = "prefer not to say",
                      "transgender.man" = "transgender man")))

# finally, I'm just going to change "participant.ID" to lowercase "participant.id"


RPEC_1_data <- (RPEC_1_data
                %>% rename(participant.id = participant.ID)
)

summary(RPEC_1_data)

# Because we are using a beta GLMM for the model, our response variables 
# (proportion of money allocated to winner
# and proportion of coaching hours allocated to the winner) cannot 
# included any 1s or 0s. Under Ben and Jonathan's
# advisement, I will use a beta GLMM on the transformed response, 
# which I will transform with a shift of 0.1 for 
# everything. 

# Transformation equation: (0.1/2) + ((1-0.1)*(prop.allocated))

# However, I'm leaving the raw values in the dataframe, and will 
# include the equation in the model (see analysis R script)

# Since we are doing a beta GLMM for the allocation task (the proportion of money or coaching
# hours that participants awarded to the winner), and each participant allocated both 
# money and coaching hours, I need to pivot my data into long format so that 
# each participant has two response rows, 1 for "money allocated to winner", and 1 for 
# "coaching hours allocated to winner".

RPEC_1_data <- (RPEC_1_data
                      %>% pivot_longer(cols = starts_with("prop"),
                                       names_to = "resource",
                                       values_to = "prop.allocated"))

# View(WLdata_allocation) visually inspecting the DF to ensure that the values copied over correctly, which they 
# did.

# Data reformatted properly for  GLMM, now just going to do some cleaning and renaming for the sake of 
# simplicity. I have made everything lowercase.

RPEC_1_data <- (RPEC_1_data
                      %>% mutate(resource = fct_recode(resource,
                                                       "money" = "prop.money.to.winner",
                                                       "coaching" = "prop.coach.to.winner"))
                      %>% mutate(comp.context = fct_recode(comp.context,
                                                           "athletic" = "Athletic",
                                                           "academic" = "Academic")))

write_rds(RPEC_1_data, "RPEC_1_data.rds")

# ------------------ CLEANING DATA FOR EXPERIMENT 2 ------------------------ #

RPEC_2_data <- read.csv("exp_2_rawdata.csv")
summary(RPEC_2_data)

# "reward_str" is just the choice of allocating to the winner or loser spelled out.
# "reward_bin" is just the numerical (1, 0) version of reward_str.

# Several variables are stored as characters, which I don't want. I'm going to change these
# into factors.

RPEC_2_data <- (RPEC_2_data %>% mutate(across(where(is.character), as.factor)))

# Participant.id is stored as a continuous predictor, when I want it to be a factor instead.

RPEC_2_data <- (RPEC_2_data %>% mutate(participant.ID = as.factor(participant.ID)))

summary(RPEC_2_data)

# just for consistency, changing levels of "gender" to have periods instead of underscores

RPEC_2_data <- (RPEC_2_data
           %>% mutate(gender = fct_recode(gender,
                                             "cisgender.man" = "cisgender_man",
                                             "cisgender.woman" = "cisgender_woman" ,
                                             "transgender.man" = "trans_man",
                                             "gender.fluid" = "gender_fluid")))

summary(RPEC_2_data)

# finally, I'm just going to change "participant.ID" to lowercase "participant.id"

RPEC_2_data <- (RPEC_2_data
           %>% rename(participant.id = participant.ID))

summary(RPEC_2_data)

write_rds(RPEC_2_data, "RPEC_2_data.rds")

# ------------------ CLEANING COMBINED PERCEPTION DATASET ------------------------ #

RPEC_perception_data <- read_csv("perception_rawdata.csv")

# changing characters to factors

RPEC_perception_data <- (RPEC_perception_data %>% mutate(across(where(is.character), as.factor)))

# changing experiment and participant.ID to factors

RPEC_perception_data <- (RPEC_perception_data %>% mutate(experiment = as.factor(experiment)) 
                                              %>% mutate(participant.ID = as.factor(participant.ID)))

# misc cleaning up of capital letters and whatnot

RPEC_perception_data <- (RPEC_perception_data
                          %>% mutate(gender = fct_recode(gender,
                                                         "cisgender.man" = "cisgender_man",
                                                         "cisgender.woman" = "cisgender_woman",
                                                         "transgender.man" = "trans_man",
                                                         "gender.fluid" = "gender_fluid",
                                                         "cisgender.man" = "cisgender man",
                                                         "cisgender.woman" = "cisgender woman" ,
                                                         "transgender.man" = "trans_man",
                                                         "transgender.man" = "transgender man",
                                                         "gender.fluid" = "gender_fluid",
                                                         "pnts" = "prefer not to say"))
                          %>% rename(participant.id = participant.ID))

summary(RPEC_perception_data)

# Also, we are planning bivariate analyses on the "perception" response
# variables as well (athleticism perception and intelligence perception). 
# Once again we need to flip the old wide dataframe into long format, but 
# this time for the "perception" response variables.

RPEC_perception_data <- (RPEC_perception_data
                          %>% pivot_longer(cols = ends_with("perception"),
                                           names_to = "trait",
                                           values_to = "perception"))

# View(RPEC_perception_data) to see if the columns reformatted properly, which they did.

RPEC_perception_data <- (RPEC_perception_data
                          %>% mutate(comp.context = fct_recode(comp.context,
                                                               "athletic" = "Athletic",
                                                               "academic" = "Academic"))
                          %>% mutate(trait = fct_recode(trait,
                                                        "athleticism" = "athleticism.perception",
                                                        "intelligence" = "intelligence.perception"))
                         %>% mutate(level = fct_recode(level,
                                                       "elementary" = "Elementary",
                                                       "highschool" = "High school")))

summary(RPEC_perception_data)
# We will be analyzing variation perception as an as an ordinal response. 
# To do that, we will need to make it an ordered factor

RPEC_perception_data_fct <- (RPEC_perception_data
                          %>% mutate(perception = factor(perception, ordered = TRUE))
)

# We can check if perception is an ordered factor with a simple function: 

if(is.ordered(RPEC_perception_data_fct$perception)) {
  print("Ordered")
} else {
  print("Not ordered")}

# Since our function returns "ordered", we know that our response "perception"
# is ordered.

summary(RPEC_perception_data_fct)

write_rds(RPEC_perception_data_fct, "RPEC_perception_data_fct.rds")
