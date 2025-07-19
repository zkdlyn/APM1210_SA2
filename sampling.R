library(tidyverse)
set.seed(69)

celebrity_df <- read.csv("celebrity_df.csv")

celebrity_df <- celebrity_df %>%
  mutate(group = paste(politics, education, sep = "_"))

group_counts <- celebrity_df %>%
  count(group) %>%
  mutate(prop = n / sum(n),
         sample_size = round(prop * 30))

diff <- 30 - sum(group_counts$sample_size)
if (diff != 0) {
  adjust_index <- which.max(group_counts$sample_size)
  group_counts$sample_size[adjust_index] <- group_counts$sample_size[adjust_index] + diff
}

sampled_df <- group_counts %>%
  rowwise() %>%
  do({
    grp <- .$group
    size <- .$sample_size
    celeb_subset <- celebrity_df %>% filter(group == grp)
    sampled_rows <- celeb_subset %>% sample_n(size)
    sampled_rows
  }) %>%
  ungroup() %>%
  select(-group)

write.csv(sampled_df, "celebrity_sample30.csv", row.names = FALSE)

table(sampled_df$politics, sampled_df$education)

plot(sampled_df)
