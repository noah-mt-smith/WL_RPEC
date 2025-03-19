# R script (analyses) for reactive phenotype 
# environment correlation manuscript

library(tidyverse)
library(ggplot2);theme_set(theme_linedraw())
library(DHARMa)
library(glmmTMB)
library(ordinal)
library(emmeans)

# ------------------------------ EXPERIMENT 1 ANALYSIS (BETA GLMM) ------------------------------- #

# loading cleaned allocation data from data_cleaning.R 

RPEC_1_data <- readRDS("RPEC_1_data.rds")

## ALLOCATION BOXPLOT ##

strip_labs = c("academic" = "Academic context", "athletic" = "Athletic context")
allocation_x_text <- c("coaching" = "Coaching hours", "money" = "Money")

RPEC_1_data$comp.context <- fct_rev(RPEC_1_data$comp.context) # so athletic comes first
RPEC_1_data$resource <- fct_rev(RPEC_1_data$resource) # so money comes first

exp1_boxplot <- ggplot(RPEC_1_data, aes(x = resource, y = prop.allocated, fill = resource)) +
  geom_boxplot() + 
  facet_wrap(~comp.context, nrow = 1, labeller = as_labeller(strip_labs)) + 
  theme(legend.position = "none", 
        axis.title.x = element_text(size = 18,
                       margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 16, margin = margin(r = 10)),
        axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 18)) +
  labs(x = "", y = "Proportion allocated to winner", fill = "Resource type") +
  ylim(0,1) + 
  geom_hline(yintercept = 0.5, lty = 3) + scale_x_discrete(labels = allocation_x_text)

print(exp1_boxplot)

## BETA GLMM OF ALLOCATION BEHAVIOUR (ON TRANSFORMED RESPONSE) ##

GLMM_allocation <- glmmTMB(((0.1/2) + ((1-0.1)*(prop.allocated))) ~ resource*comp.context +
                           level + winner.name + (1|participant.id), 
                           data = RPEC_1_data, family = beta_family(link="logit"))

# Diagnostics on Beta glmm using DHARMa

testResiduals(GLMM_allocation, plot = T)

testQuantiles(GLMM_allocation)

# using emmeans to compute point estimates, confidence intervals, z- and p-values
# for log odds of resource allocated to winner, where a log odds of 0 is the "null" 
# that participants allocated 0.5 of their resources to winners and 0.5 of their resources
# to losers (i.e., resources allocated in a 1:1 manner)

allocation_logodds_emm_resourceonly <- emmeans(GLMM_allocation, specs = "resource")
allocation_logodds_df_resourceonly <- as.data.frame(allocation_logodds_emm_resourceonly)

allocation_logodds_pvals <- (allocation_logodds_df_resourceonly
                                          %>% mutate(z.ratio = emmean/SE,
                                                     p.value = 1.96*(1-pnorm(abs(z.ratio)))))
allocation_logodds_pvals

## Using emmeans to calculate point estimates and confidence intervals for 
## back-transformed response of proportion (again pooled across resource type). 

allocation_prop_emm_resourceonly <- emmeans(GLMM_allocation, specs = "resource", type = "response")
print(as.data.frame(allocation_prop_emm_resourceonly))

## Using emmeans to create inferential plot for transformed response 
## including resource by competitive context interaction)

allocation_prop_emm_interaction <- emmeans(GLMM_allocation, specs = "resource", "comp.context", type = "response")

allocation_prop_df_interaction <- as.data.frame(allocation_prop_emm_interaction)

allocation_x_text <- c("coaching" = "Coaching hours", "money" = "Money")

exp1_inferential_fig <-  ggplot(allocation_prop_df_interaction, aes(resource, response)) + 
  geom_point(size = 3.5) + facet_wrap(~comp.context, nrow = 1, 
                                      labeller = as_labeller(strip_labs)) + 
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0, linewidth = 1) +
  labs(y = "Proportion of resource allocated to winner", x = "Resource type") + 
  theme(legend.position = "none", 
        axis.title.x = element_text(size = 18,
                                    margin = margin(t = 10, r = 0, b = 0, l = 0)), 
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 18)) + 
  geom_hline(yintercept = 0.5, lty = 3) + ylim(0,0.8) + scale_x_discrete(labels = allocation_x_text)

print(exp1_inferential_fig)

## pairwise post-hoc contrast comparing coaching hour allocation in 
## academic vs. athletic scenarios (pooled across gender)

allocation_emm_contrast <- emmeans(GLMM_allocation, pairwise ~ resource:comp.context)
allocation_emm_contrast

## extracting coefficients for beta GLMM using summary function

summary(GLMM_allocation)

# ------------------------------ EXPERIMENT 2 ANALYSIS (BINOMIAL GLM) ------------------------------- #

## LOADING EXPERIMENT 2 DATA ##

RPEC_2_data <- readRDS("RPEC_2_data.rds")

## BARPLOT OF EXPERIMENT 2 DATA ##

strip_labels <- c("academic" = "Academic context", "athletic" = "Athletic context")
competitor_labels <- c("winner" = "Winner", "loser" = "Loser")

RPEC_2_data$reward_str <- fct_rev(RPEC_2_data$reward_str) # reversing levels so 
# order is "winner" then "loser" within the facets.

RPEC_2_barplot <-  ggplot(RPEC_2_data, aes(x = reward_str)) + 
  geom_bar(aes(y = after_stat(prop), group = 1)) + 
  facet_wrap(~ comp.context, nrow =1, labeller = as_labeller(strip_labels)) + 
  labs(x = "", y = "Proportion of participants awarding training program") +
  scale_x_discrete(labels = competitor_labels) +
  theme(legend.position = "none", 
        axis.title.x = element_text(size = 18,
                                    margin = margin(t = 10, r = 0, b = 0, l = 0)), 
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 18)) 

RPEC_2_barplot

RPEC_2_data$reward_str <- fct_rev(RPEC_2_data$reward_str) # reversing levels
# again before doing GLM 

## CREATING BINOMIAL GLM OF DATA ## 

GLM_reward <- glm(reward_bin ~ comp.context + level +
                  winner.name, family = "binomial",
                  data = RPEC_2_data)

# Evaluating fit with DHARMa

testResiduals(GLM_reward)
simulateResiduals(GLM_reward)
testQuantiles(GLM_reward)

## CREATING EMMEANS / INFERENTIAL PLOT FOR EXPERIMENT 2 DATA ## 

RPEC_2_allocation_inferential <- emmeans(GLM_reward, specs = "comp.context", type = "response")

RPEC_2_allocation_inferential_data <- as.data.frame(RPEC_2_allocation_inferential)

summary(RPEC_2_allocation_inferential_data)

## inferential figure of rewarding winner (back transformed to probability)

x_labels <- c("academic" = "Academic context", "athletic" = "Athletic context")

RPEC_2_allocation_inferential_fig_prob <- ggplot(RPEC_2_allocation_inferential_data,
                aes(comp.context, prob)) + geom_point(size = 3.5) +
                geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                                  width = 0, linewidth = 1) + 
                labs(y = "Proportion of participants allocating training program to winner",
                     x = "") + 
                theme(legend.position = "none", 
                      axis.title.x = element_text(size = 18,
                      margin = margin(t = 10, r = 0, b = 0, l = 0)), 
                      axis.title.y = element_text(size = 14),
                      axis.text.x = element_text(size = 16), 
                      axis.text.y = element_text(size = 12)) +
                scale_x_discrete(labels = x_labels) +
                scale_y_continuous(limits = c(0.4, 1), breaks = seq(0, 1, by = 0.1)) + 
                geom_hline(yintercept = 0.5, lty = 3)

RPEC_2_allocation_inferential_fig_prob

## CREATING LOG ODDS INFERENTIAL FIG (NOT BACK TRANSFORMED) ##

RPEC_2_inferential_logodds <- emmeans(GLM_reward, specs = "comp.context")

RPEC_2_inferential_logodds_data <- as.data.frame(RPEC_2_inferential_logodds)
# inferential figure of rewarding winner

RPEC_2_inferential_logodds_data$comp.context <- fct_rev(RPEC_2_inferential_logodds_data$comp.context)
# reversing levels so athletic comes first in fig

RPEC_2_inferential_fig_logodds <- ggplot(RPEC_2_inferential_logodds_data,
                                            aes(comp.context, emmean)) + geom_point(size = 3.5) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0, linewidth = 1) + 
  labs(y = "Log odds of accepting winner to training program",
       x = "") + 
  theme(legend.position = "none", 
        axis.title.y = element_text(size = 16, margin = margin(r = 10)),
        axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 12)) +
  scale_x_discrete(labels = x_labels) +
  geom_hline(yintercept = 0, lty = 3)

RPEC_2_inferential_fig_logodds

# Computing p-values using Wald method by comparing confints to "null" value of 0.

RPEC_2_inferential_pvals <- (RPEC_2_inferential_logodds_data 
                                %>% mutate(z.ratio = emmean/SE,
                                           p.value = 1.96*(1-pnorm(abs(z.ratio)))))

RPEC_2_inferential_pvals

# These z- and p-values are used for the probability results as well, 
# since the probabilities are just back transformed log odds values (model is same)

# summary function to extract model coefficients

summary(GLM_reward)

# ------------------------------ PERCEPTION ANALYSIS (CLMM) ------------------------------- #


## LOAD PERCEPTION DATA (COMBINED ACROSS EXPERIMENT 1 AND 2) ##

RPEC_perception_data_fct <- readRDS("RPEC_perception_data_fct.rds")

summary(RPEC_perception_data_fct)

## PERCEPTION BARPLOT ##

perception_labs = c("athleticism" = "Athleticism", "intelligence" = "Intelligence")

perception_bar_x_labs = c("0.33" = "Very little", "0.5" = "Little", "1" = "Some", "2" = "Large", "3" = "Very large")

perception_barplot <- ggplot(RPEC_perception_data_fct, aes(x = perception)) +
  geom_bar(aes(y = after_stat(prop), group = 1)) +
  facet_wrap(~ trait, nrow = 1, labeller = as_labeller(perception_labs)) +
  labs(x = "Perceived variation",
       y = "Proportion of participants") +
  scale_x_discrete(labels = perception_bar_x_labs) + 
  theme(axis.title.x = element_text(size = 18, margin = margin(t = 10, r = 0, b = 0, l = 0)), axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 12), strip.text = element_text(size = 18))

print(perception_barplot)

## CUMULATIVE LINK MIXED MODEL (CLMM) FOR PERCEPTION DATA ## 

perception_clmm <- clmm(perception ~ 1 
                        + trait*comp.context + experiment
                        + (1|participant.id), link = "logit",
                        threshold = "flexible", Hess = TRUE, nAGQ = 10,
                        data = RPEC_perception_data_fct)

# Comparing full CLMM to null CLM

null_clm  <- clm(perception ~ 1, 
                 link = "logit",
                 threshold = "flexible", Hess = TRUE, nAGQ = 10,
                 data = RPEC_perception_data_fct)

anova(null_clm, perception_clmm) # AIC of fitted model much better

# Random effects plot for CLMM #

# (from https://cran.r-project.org/web/packages/ordinal/vignettes/clmm2_tutorial.pdf) #

ci <- perception_clmm$ranef + qnorm(0.975) * sqrt(perception_clmm$condVar) %o% c(-1, 1)
ord.re <- order(perception_clmm$ranef)
ci <- ci[order(perception_clmm$ranef),]
plot(1:355, perception_clmm$ranef[ord.re], axes=FALSE, ylim=range(ci),
     xlab="Participant", ylab="Participant effect")
axis(1, at=1:355, labels = ord.re)
axis(2)
for(i in 1:355) segments(i, ci[i,1], i, ci[i, 2])
abline(h = 0, lty=2)

## EMMEANS FOR PERCEPTION CLMM AND INFERENTIAL FIG ##

emm_clmm_choices <- emmeans(perception_clmm, 
                            ~perception|trait, 
                            mode = "prob",
                            regrid = "logit")

perception_labs = c("athleticism" = "Athleticism", "intelligence" = "Intelligence")
perception_x_text <- c("1" = "Very little", "2" = "Little", "3" = "Some", "4" = "Large", "5" = "Very large")
perception_dat_inferential <- as.data.frame(emm_clmm_choices)

perception_inferential_fig <- ggplot(perception_dat_inferential, aes(x = perception, y = prob)) +
  geom_point(size = 3.5) + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                                         width = 0,
                                         linewidth = 1) +
  facet_wrap(~trait, nrow = 1, labeller = as_labeller(perception_labs)) + 
  theme(legend.position = "none", axis.title.x = element_text(size = 18,
                                                              margin = margin(t = 10, r = 0, b = 0, l = 0)), axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), strip.text = element_text(size = 18)) +
  scale_x_discrete(labels = perception_x_text) +
  labs(x = "Perceived variation", y = "Log odds of choice")

print(perception_inferential_fig)

# extracting clmm coefficients with summary function

summary(perception_clmm)
