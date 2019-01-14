tipper_stats %>% 
  filter(tipper != "Mean") %>% 
  ggplot(aes(fct_reorder(tipper, -rank), total_score)) +
  geom_point(colour = "red3", size = 4) +
  geom_segment(aes(x = tipper,
                   xend = tipper,
                   y =  min(total_score) - 10,
                   yend = total_score),
               colour = "red3",
               linetype = "dashed",
               size = 1.5) +
  geom_text(label = filter(tipper_stats, tipper != "Mean")$total_score, 
            hjust = -1,
            size = 4) +
  coord_flip() +
  scale_y_continuous(limits = c(min(tipper_stats$total_score) - 10, max(tipper_stats$total_score) + 1)) +
  labs(title = "Final Leaderboard Line Plot",
       x = "Tipper",
       y = "Total Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 

ggplot(round_scores, aes(round, fct_rev(tipper), fill = score)) +
  geom_raster() +
  labs(title = "Heat Map of Tipper Round Scores",
       x = "Round",
       y = "Tipper",
       fill = "Round Score") +
  scale_fill_gradient(low = "white",
                      high = "red", 
                      limits = c(0, 9), 
                      breaks = seq(0, 9, by = 1)) +
  guides(fill = guide_colourbar(ticks = F)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 0.5)) 

round_plot <- round_scores %>% 
  ggplot(aes(tipper, score)) +
  geom_point(size = 2) +
  geom_hline(data = round_stats, 
             aes(yintercept = mean, linetype = ""),
             colour = "red",
             size = 1.2,
             alpha = 0.5) +
  geom_label(data = round_score_label, label = "Bye Round", size = 4) +
  scale_linetype_manual("Mean", values = "solid") +
  scale_y_continuous(limits = c(0, 9), breaks = seq(0, 9, by = 1)) +
  labs(title = "Point Plot by Round",
       x = "Tipper",
       y = "Score") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 0.5)) 

map(1:4, ~ round_plot + facet_wrap_paginate(~ round, nrow = 3, ncol = 2, page = .))

tipper_plot <- round_scores %>% 
  ggplot(aes(round, score)) +
  geom_point(size = 3) +
  geom_point(data = round_stats, 
             aes(round, mean, shape = ""), 
             colour = "red",
             size = 3,
             alpha = 0.5) +
  scale_shape_manual("Mean", values = 16) +
  scale_y_continuous(limits = c(0, 9), breaks = seq(0, 9, by = 1)) +
  labs(title = "Point Plot by Tipper",
       x = "Round",
       y = "Score") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 0.5))

map(1:6, ~ tipper_plot + facet_wrap_paginate(~ tipper, nrow = 2, ncol = 1, page = .))

ggplot(round_scores, aes(round, score)) +
  geom_jitter(colour = "dodgerblue", size = 3, width = 0.15, alpha = 0.8) +
  stat_summary(fun.y = "mean", 
               aes(shape = "Mean"), 
               geom = "point", 
               colour = "red",
               size = 4,
               alpha = 0.8) +
  stat_summary(fun.data = "mean_cl_boot", 
               geom = "errorbar", 
               colour = "red",
               size = 1,
               width = 0.2,
               alpha = 0.8) +
  scale_shape_manual("", values = 18) +
  scale_y_continuous(limits = c(-0.5, 9.5), breaks = seq(0, 9, by = 1)) +
  labs(title = "Jitter Plot by Round", 
       subtitle = "Confidence Intervals: Bootstrap 95%",
       x = "Round", 
       y = "Score") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 0.5))

ggplot(round_scores, aes(fct_reorder(tipper, -score, sum), score)) +
  geom_jitter(colour = "aquamarine4", size = 3, width = 0.2, alpha = 0.8) +
  stat_summary(fun.y = "mean", 
               aes(shape = "Mean"), 
               geom = "point", 
               colour = "red",
               size = 4,
               alpha = 0.8) +
  stat_summary(fun.data = "mean_cl_boot", 
               geom = "errorbar", 
               colour = "red",
               size = 1,
               width = 0.2,
               alpha = 0.8) +
  scale_shape_manual("", values = 18) +
  scale_y_continuous(limits = c(-0.5, 9.5), breaks = seq(0, 9, by = 1)) +
  labs(title = "Jitter Plot by Tipper", 
       subtitle = "Confidence Intervals: Bootstrap 95%",
       x = "Tipper", 
       y = "Round Scores") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 0.5))

ggplot(round_scores, aes(score)) +
  geom_histogram(binwidth = 1, colour = "black", fill  = "khaki2") +
  geom_vline(aes(xintercept = mean(score), linetype = ""),
             size = 2) +
  scale_x_continuous(breaks = seq(0, 9, by = 1)) +
  scale_linetype_manual("Mean", values = "solid") +
  labs(title = "Histogram of All Scores",
       x = "Round Score",
       y = "Count") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(round_scores, aes(score, fill = fct_rev(round_type))) +
  geom_histogram(binwidth = 1, colour = "black") +
  facet_wrap(~ fct_rev(round_type)) +
  geom_vline(data = hist_means, 
             aes(xintercept = mean, linetype = ""), 
             colour = "black",
             size = 1.5) +
  scale_fill_brewer(palette = "Pastel2") +
  scale_x_continuous(breaks = seq(0, 9, by = 1)) +
  scale_linetype_manual("Mean", values = "solid") +
  labs(title = "Histogram of All Scores - Split by Round Type",
       x = "Round Score",
       y = "Count") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = F)

ggplot(round_scores, aes(score, fill = tipper)) +
  geom_histogram(binwidth = 1, colour = "white") +
  scale_fill_brewer(palette = "Paired") +
  scale_x_continuous(breaks = seq(0, 9, by = 1)) +
  facet_wrap(~ tipper) +
  geom_vline(data = filter(tipper_stats, tipper != "Mean"),
             aes(xintercept = mean, linetype = ""),
             size = 1.5) +
  scale_linetype_manual("Mean", values = "dotted") +
  labs(title = "Histogram of All Scores by Tipper",
       x = "Round Score",
       y = "Count") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = F) 

tipper_stats %>% 
  filter(tipper != "Mean") %>% 
  ggplot(aes(fct_reorder(tipper, rank), 
             plus_minus_mean, 
             colour = above_below_average)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1.2) +
  geom_point(size = 6) +
  scale_colour_manual(values = c("red", "forestgreen")) +
  labs(title = "Point Plot of Above / Below Mean by Tipper",
       x = "Tipper",
       y = "+ / - vs Mean") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 0.5)) +
  guides(colour = F)

ggplot(round_stats, 
       aes(round, 
           plus_minus_mean, 
           colour = above_below_average,
           shape = fct_rev(round_type))) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1.2) +
  geom_point(size = 6) +
  scale_colour_manual(values = c("red", "forestgreen")) +
  scale_shape_manual(values = c(16, 18)) +
  labs(title = "Point Plot of Above / Below Mean by Round",
       x = "Round",
       y = "+ / - vs Mean",
       shape = "Round Type") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 0.5)) +
  guides(colour = F)

ggplot(round_stats, 
       aes(fct_reorder(round, -mean), 
           plus_minus_mean, 
           colour = above_below_average,
           shape = fct_rev(round_type))) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1.2) +
  geom_point(size = 6) +
  scale_colour_manual(values = c("red", "forestgreen")) +
  scale_shape_manual(values = c(16, 18)) +
  labs(title = "Point Plot of Above / Below Mean for Rounds (Descending Order)",
       x = "Round",
       y = "+ / - vs Mean",
       shape = "Round Type") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 0.5)) +
  guides(colour = F)

cumulative_scores %>% 
  filter(tipper != "Mean") %>% 
  ggplot(aes(round, cumulative_score, group = tipper, colour = tipper)) +
  geom_line(size = 1) +
  scale_colour_brewer(palette = "Paired") +
  labs(title = "Line Plot of Cumulative Tipper Scores",
       x = "Round",
       y = "Cumulative Score",
       colour = "Tipper") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 0.5)) 

ggplot(tipper_stats_long, aes(tipper, value, fill = variable)) +
  geom_col() +
  facet_wrap(~ variable, scales = "free") +
  scale_fill_brewer(palette = "Set2") +
  geom_text(label = tipper_stats_long$value, vjust = 1.5) +
  geom_blank(data = fix_y_tipper) +
  labs(title = "Summary Stats by Tipper",
       x = "Tipper",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  guides(fill = F)

ggplot(round_stats_long, aes(round, value, fill = variable)) +
  geom_col() +
  facet_wrap(~ variable, scales = "free") +
  scale_fill_brewer(palette = "Set3") +
  geom_text(label = round_stats_long$value, size = 3, vjust = 1.5) +
  geom_blank(data = fix_y_round) +
  labs(title = "Summary Stats by Round",
       x = "Round",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  guides(fill = F)