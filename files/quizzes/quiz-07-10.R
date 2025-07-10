library(tidyverse)

set.seed(123)  # For reproducibility

# Simulate data
n_per_group <- 40

sales_data <- tibble(
  employee_id = 1:(2 * n_per_group),
  group = c(rep("Trained", n_per_group), rep("Untrained", n_per_group)),
  sales = case_when(
    group == "Trained"   ~ rnorm(80, mean = 95000, sd = 8000),
    group == "Untrained" ~ rnorm(80, mean = 88000, sd = 8000)
  )
)

# Visual check
sales_data %>%
  ggplot(aes(x = sales, fill = group)) +
  geom_histogram(alpha = 0.6, bins = 15, position = "identity") +
  labs(title = "Propane Sales by Group", x = "Quarterly Sales ($)", y = "Count") +
  theme_minimal()

sales_data %>% independent_qq(continuous = sales,
                                 grouping = group)


sales_data %>% variances_HT(continuous = sales,
                            grouping = group)

sales_data %>% independent_mean_HT(continuous = sales,
                                   grouping = group)



library(tidyverse)

set.seed(234)

n_students <- 30

# Simulate skewed scores before and after
vocab_data <- tibble(
  student_id = 1:n_students,
  score_before = rpois(n_students, lambda = 10),
  score_after  = score_before + rpois(n_students, lambda = 3)  # most students improve
)

# Check distribution
vocab_data %>%
  pivot_longer(cols = c(score_before, score_after), names_to = "time", values_to = "score") %>%
  ggplot(aes(x = score, fill = time)) +
  geom_histogram(alpha = 0.6, bins = 15, position = "identity") +
  labs(title = "Vocabulary Quiz Scores Before & After Peggy's Bootcamp", x = "Score", y = "Count") +
  theme_minimal()

vocab_data %>% dependent_qq(col1 = score_before,
                            col2 = score_after)

vocab_data %>% dependent_median_HT(col1 = score_after,
                                   col2 = score_before,
                                   m = 2.5,
                                   alternative = "greater")
