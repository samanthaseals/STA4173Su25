library(tidyverse)

set.seed(1947)

# Simulate paired laughter scores
n <- 30
roger_data <- tibble(
  audience_id = 1:n,
  visual_gag = round(rbeta(n, 2, 5) * 100),  # Skewed right
  verbal_pun = round(rbeta(n, 3, 4) * 100)   # Also skewed
)

# Perform Wilcoxon signed-rank test
wilcox.test(roger_data$visual_gag, roger_data$verbal_pun, 
            paired = TRUE, alternative = "two.sided", exact = FALSE)


library(tidyverse)

set.seed(1988)

# Simulate number of pie hits for two independent groups
toontown_data <- tibble(
  neighborhood = rep(c("Sillyville", "Zany Acres"), each = 30),
  pie_hits = c(rpois(30, lambda = 6),  # Sillyville average: 6 hits
               rpois(30, lambda = 6.2)) # Zany Acres average: 6.2 hits
)

# Run Wilcoxon rank sum test
wilcox.test(pie_hits ~ neighborhood, data = toontown_data, exact = FALSE)
