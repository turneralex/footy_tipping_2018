library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(forcats)
library(magrittr)
library(ggplot2)
library(ggforce)

# data import
footy_tips <- read_csv("C:/RStudio Projects/Personal Projects/Footy Tipping 2018/footy_tips.csv")
total_margin <- read_csv("C:/RStudio Projects/Personal Projects/Footy Tipping 2018/total_margin.csv")

# create vector of round labels for various data frames
rounds <- map_chr(seq_along(footy_tips[,-1]), ~ paste("Round", .))

# add round mean row
footy_tips <- footy_tips %>% 
  bind_rows(c(tipper = "Mean", map(footy_tips[,-1], ~ mean(.) %>% round(2))))

# convert tipper variable to factor
footy_tips$tipper <- footy_tips$tipper %>% factor()

# create data frame with cumulative total tips by tipper
cumulative_scores <- footy_tips[,-1] %>% 
  pmap(lift_vd(cumsum)) %>% 
  invoke(bind_rows, .) %>% 
  bind_cols(tipper = footy_tips$tipper, .) %>% 
  gather(round, cumulative_score, -1)

# convert round variable to factor
cumulative_scores$round <- cumulative_scores$round %>%  
  factor(levels = footy_tips[,-1] %>% colnames(),
         labels = rounds,
         ordered = T)

# create data frame of summary stats by tipper
tipper_stats <- data_frame(
  tipper = footy_tips$tipper,
  total_score = footy_tips[,-1] %>% rowSums(), 
  max = footy_tips[,-1] %>% pmap_dbl(max),
  min = footy_tips[,-1] %>% pmap_dbl(min),
  mean = footy_tips[,-1] %>% rowMeans() %>% round(2),
  standard_deviation = footy_tips[,-1] %>% pmap_dbl(lift_vd(sd)) %>% round(2),
  total_margin = c(total_margin$total_margin, NA)
)

# ensure tipper order is correct (based on tips & margin)
tipper_stats <- tipper_stats %>% 
  filter(tipper != "Mean") %>% 
  arrange(desc(total_score), total_margin) %>% 
  bind_rows(filter(tipper_stats, tipper == "Mean"))

# add columns for +/- vs mean, above / below average & rank
tipper_stats <- tipper_stats %>% 
  mutate(
    plus_minus_mean = combine(
      filter(tipper_stats, tipper != "Mean")$total_score - filter(tipper_stats, tipper == "Mean")$total_score,
      NA # the NA is added as this obviously doesn't apply for the mean
    ),
    above_below_average = if_else(
      plus_minus_mean > 0,
      "Above Average",
      "Below Average"
    ),
    rank = c(1:(nrow(tipper_stats) - 1), NA)
  )

# convert above / below average variable to factor
# R always sorts factors in alphabetical order so simply reversing this 
# will set the order correctly for this ordinal variable
tipper_stats$above_below_average <- tipper_stats$above_below_average %>% 
  factor(ordered = T) %>% 
  fct_rev()

# create long format tipper stats data frame for plotting
tipper_stats_long <- tipper_stats[,1:6] %>% 
  filter(tipper != "Mean") %>% 
  gather(variable, value, -1)

# convert variable column to factor
tipper_stats_long$variable <- tipper_stats_long$variable %>% 
  factor(levels = tipper_stats[,2:6] %>% colnames(),
         labels = c("Total Score", "Max", "Min", "Mean", "Standard Deviation"))

# create data frame of summary stats by round
round_stats <- data_frame(
  round = rounds %>% factor(levels = rounds, ordered = T),
  max = footy_tips[,-1] %>% map_dbl(max),
  min = footy_tips[,-1] %>% map_dbl(min),
  mean = footy_tips%>% 
    filter(tipper != "Mean") %>% 
    select(-1) %>% 
    colMeans() %>% 
    round(2),
  standard_deviation = footy_tips %>% 
    filter(tipper != "Mean") %>% 
    select(-1) %>% 
    map_dbl(sd) %>% 
    round(2)
)

# add columns for round type, +/- vs mean & above / below average
round_stats <- round_stats %>% 
  mutate(
    round_type = if_else(
      round %in% rounds[12:14], 
      "Bye Round", 
      "Standard Round"
    ),
    plus_minus_mean = mean - mean(mean), 
    # i'll admit this code looks a bit odd, but it works fine 
    # this is because there were the same number of tippers for 
    # each round, so it is okay to the mean of the mean
    above_below_average = if_else(
      plus_minus_mean > 0,
      "Above Average",
      "Below Average"
    )
  )

# convert round type variable to factor
round_stats$round_type <- round_stats$round_type %>% factor()

# same as before 
round_stats$above_below_average <- round_stats$above_below_average %>% 
  factor(ordered = T) %>% 
  fct_rev()

# create long format round stats data frame for plotting
round_stats_long <- round_stats[,1:5] %>%
  gather(variable, value, -1)

# convert variable column to factor
round_stats_long$variable <- round_stats_long$variable %>% 
  factor(levels = round_stats[,2:5] %>% colnames(),
         labels = c("Max", "Min", "Mean", "Standard Deviation"))

# create long format data frame for round scores to be used for plotting
round_scores <- footy_tips %>% 
  filter(tipper != "Mean") %>% 
  gather(round, score, -1)

# define round variable as ordered factor
round_scores$round <- round_scores$round %>% 
  factor(levels = footy_tips[,-1] %>% colnames(),
         labels = rounds,
         ordered = T)

# add column for round type
round_scores <- round_scores %>% 
  mutate(
    round_type = if_else(
      round %in% rounds[12:14], 
      "Bye Round", 
      "Standard Round"
    ) 
  )

# define round type variable as factor
round_scores$round_type <- round_scores$round_type %>% factor()

# create data frames for fixing plot axes, adding labels to facets etc
fix_y_tipper <- data.frame( # using data.frame instead of data_frame here to avoid different column length errors
  tipper = c(sort(footy_tips$tipper)[1], 
             sort(footy_tips$tipper)[length(footy_tips$tipper)]),
  variable = c(rep("Max", 2), 
               rep("Min", 2)),
  value = c(0,9)
)

fix_y_round <- data.frame(
  round = c(rounds[1], rounds[length(rounds)]),
  variable = c(rep("Max", 2), rep("Min", 2)),
  value = c(0,9)
)

round_score_label <- data.frame(
  tipper = sort(footy_tips$tipper)[length(footy_tips$tipper) / 2],
  round = rounds[12:14],
  score = 8
)

hist_means <- data.frame(
  mean = c(filter(round_scores, round_type == "Standard Round")$score %>% mean(),
           filter(round_scores, round_type == "Bye Round")$score %>% mean()),
  round_type = c("Standard Round", "Bye Round")
)
