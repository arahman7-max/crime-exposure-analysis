# Crime Exposure Analysis
#
# Methods:
# - Data visualization
# - Linear regression
# - Interaction effects
#
# Examining the relationship between crime exposure and the
# probability of committing a crime before and after an intervention.
#
# Data:
# The dataset used in this analysis was provided for academic purposes
# and is not included in this repository.

# Load libraries

library(tidyverse)
library(broom)
library(modelsummary)



# Visualize crime probability by treatment status and time period

ggplot(
  crime_exposure,
  aes(x = tstatus, y = probcrime)
) +
  stat_summary(
    geom = "pointrange",
    fun.data = "mean_se",
    fun.args = list(mult = 1.96)
  ) +
  facet_wrap(vars(After))

# Before the intervention, exposure to crime is associated with a
# slightly higher probability of committing a crime. After the
# intervention, this difference becomes substantially larger.



# Estimate interaction model

interaction_model <- lm(
  probcrime ~ tstatus + After + (tstatus * After),
  data = crime_exposure
)

tidy(interaction_model)

# Results indicate a statistically significant interaction between
# treatment status and the post-intervention period.



# Estimate interaction model with additional controls

adjusted_interaction_model <- lm(
  probcrime ~ tstatus + After + (tstatus * After) +
    male + white + SocialM,
  data = crime_exposure
)

modelsummary(
  list(
    interaction_model,
    adjusted_interaction_model
  )
)

# After controlling for demographic characteristics and social
# mimicry, the estimated association remains similar, suggesting the
# additional covariates do not substantially alter the results.





