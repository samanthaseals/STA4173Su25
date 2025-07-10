library(tidyverse)
library(ssstats)
set.seed(97514)

# Number of townsfolk surveyed
n <- 100

# Simulate happiness before the feast (scale: 1 to 10)
happiness_before <- rnorm(n, mean = 5, sd = 1.5)

# Simulate happiness after the feast (expected increase)
# Adding a positive effect with some variability
happiness_after <- happiness_before + rnorm(n, mean = 2, sd = 1)

# Build the dataset
maid_marian <- tibble(townsfolk = paste0("Townsfolk_", 1:n),
                      happiness_before = happiness_before,
                      happiness_after = happiness_after)

maid_marian %>% dependent_mean_HT(col1 = happiness_after,
                                  col2 = happiness_before,
                                  alternative = "greater")



set.seed(723456)

# Number of mice in each team
n_mice <- 50

# Simulate food collected by each mouse in the village square
village_square <- tibble(
  mouse_id = paste0("VS_", 1:n_mice),
  team = "Village Square",
  food_collected = rnorm(n_mice, mean = 7, sd = 1.5) # average 5 units of food
)

# Simulate food collected by each mouse near the castle gates
castle_gates <- tibble(
  mouse_id = paste0("CG_", 1:n_mice),
  team = "Castle Gates",
  food_collected = rnorm(n_mice, mean = 6.1, sd = 1.2) # average 6 units of food
)

# Combine both teams into one dataset
church_mice <- bind_rows(village_square, castle_gates)

church_mice %>% independent_mean_HT(continuous = food_collected,
                                    grouping = team)
