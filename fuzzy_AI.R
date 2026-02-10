library(tidyverse)
library(stringr)
library(dplyr)
library(ggplot2)

setwd('~/Desktop/r/fuzzy')
retractions <- read_csv("retraction_watch-2.csv")
view(retractions)

# cleaning the RetractionWatch data, separating by year and for "fuzzy."

filtered_retractions <- retractions %>%
    mutate(RetractionDate = as.Date(RetractionDate, format = "%m/%d/%Y"))

year_retractions <- filtered_retractions %>%
  mutate(Year = format(as.Date(RetractionDate, "%m/%d/%Y"), "%Y"))

fuzzy_retractions <- year_retractions %>%
  filter(str_detect(Title, regex("fuzzy", ignore_case = TRUE)))

fuzzy_retractions <- fuzzy_retractions %>% select(-Author, -URLS, -RetractionNature, -ArticleType, -Paywalled)

view(fuzzy_retractions)

# did some Excel filtering and additions of PubMed data, pulling in here.

write.csv(fuzzy_retractions, "fuzzy_retractions.csv", row.names = FALSE)

fuzzy_clean <- read_csv("fuzzy_clean.csv")

view(fuzzy_clean)

# making a bar chart to look

fuzzy_clean %>%
  group_by(Year, Still_Live) %>%
  summarise(n = n(), .groups = "drop") %>%
  ggplot(aes(
    x = Year,
    y = n,
    color = factor(Still_Live),
    group = factor(Still_Live)
  )) +
  geom_line() +
  theme_minimal() +
  geom_point()

# making a donut chart to compare retractions 

donut_df <- fuzzy_clean %>%
  mutate(status = ifelse(Still_Live == 1, "Still live", "Retracted")) %>%
  count(status) %>%
  mutate(
    fraction = n / sum(n),
    ymax = cumsum(fraction),
    ymin = lag(ymax, default = 0)
  )

donut_df <- donut_df %>%
  mutate(label = paste0(status, "\n", round(fraction * 100, 1), "%"))

ggplot(donut_df, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = status)) +
  geom_rect() +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  theme_void() +
  geom_text(
    aes(x = 3.5, y = (ymin + ymax) / 2, label = label),
    size = 4)



top_journals <- fuzzy_clean %>%
  count(Journal, sort = TRUE) %>%
  slice_head(n = 6) %>%
  pull(Journal)

fuzzy_clean_sorted <- fuzzy_clean %>%
  mutate(top6 = Journal %in% top_journals) %>%
  arrange(desc(top6), Journal)

view(fuzzy_clean_sorted)

fuzzy_donut <- fuzzy_clean_sorted %>%
  filter(Journal %in% top_journals) %>%
  mutate(status = ifelse(Still_Live == 1, "Still live", "Retracted")) %>%
  count(Journal, Year, status) %>%
  group_by(Journal, Year) %>%
  mutate(
    fraction = n / sum(n),
    ymax = cumsum(fraction),
    ymin = lag(ymax, default = 0)
  ) %>%
  ungroup()

ggplot(fuzzy_donut,
       aes(ymax = ymax, ymin = ymin,
           xmax = 4, xmin = 3,
           fill = status)) +
  geom_rect(color = "white") +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  facet_wrap(~ Journal) +
  theme_void() +
  labs(fill = "Paper status")






donut_year_df <- fuzzy_clean %>%
  # first filter the rows you want
  filter(
    !is.na(Year),      # remove NA years
    Year >= 2019,      # only from 2019 onwards
    Year != 2026       # exclude 2026
  ) %>%
  # create status column
  mutate(
    status = ifelse(Still_Live == 1, "Still live", "Retracted")
  ) %>%
  # count number of papers per Year and status
  count(Year, status) %>%
  group_by(Year) %>%
  mutate(
    fraction = n / sum(n),
    ymax = cumsum(fraction),
    ymin = lag(ymax, default = 0),
    label = paste0(round(fraction * 100, 1), "%")
  ) %>%
  ungroup()

# Basic donut plot
ggplot(donut_year_df,
       aes(ymax = ymax, ymin = ymin,
           xmax = 4, xmin = 3,
           fill = status)) +
  geom_rect(color = "white") +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  facet_wrap(~ Year) +
  theme_void() +
  labs(fill = "Paper status")

# Donut plot with labels
ggplot(donut_year_df,
       aes(ymax = ymax, ymin = ymin,
           xmax = 4, xmin = 3,
           fill = status)) +
  geom_rect(color = "white") +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  facet_wrap(~ Year) +
  theme_void() +
  geom_text(
    aes(x = 3.5, y = (ymin + ymax) / 2, label = label),
    size = 3
  ) +
  labs(fill = "Paper status")






library(dplyr)
library(ggplot2)
library(patchwork)
library(ggtext)    # for rich text in labels

# Prepare data (filtered)
donut_year_df <- fuzzy_clean %>%
  filter(
    !is.na(Year),
    Year >= 2019,
    Year != 2026
  ) %>%
  mutate(
    status = ifelse(Still_Live == 1, "Still live", "Retracted")
  ) %>%
  count(Year, status) %>%
  group_by(Year) %>%
  mutate(
    fraction = n / sum(n),
    ymax = cumsum(fraction),
    ymin = lag(ymax, default = 0),
    label = paste0("<b>", round(fraction*100,1), "%</b>")
  ) %>%
  ungroup()

# Donut plot with gradient fill and bold labels
donut_plot <- ggplot(donut_year_df,
                     aes(ymax = ymax, ymin = ymin,
                         xmax = 4, xmin = 3,
                         fill = status)) +
  geom_rect(color = "white", size = 0.5) +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  facet_wrap(~ Year) +
  theme_void() +
  geom_richtext(aes(x = 3.5, y = (ymin + ymax)/2, label = label),
                size = 3, fill = NA, label.color = NA) +
  scale_fill_manual(values = c(
    "Still live" = "#5097A4",
    "Retracted" = "#CD5555")) +
  labs(fill = "Paper status") +
  theme(strip.text = element_text(face="bold", size=12)) +
  ggtitle("Paper Status by Year (Donut Charts)")

# Bar chart with patterned fill and labels
bar_plot <- donut_year_df %>%
  ggplot(aes(x = factor(Year), y = n, fill = status)) +
  geom_col(color = "white", width = 0.7) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), color = "white") +
  scale_fill_manual(values = c(
    "Still live" = "#5097A4", 
    "Retracted" = "#CD5555")) +
  theme_minimal(base_size = 12) +
  labs(
    x = "Year",
    y = "Number of Papers",
    fill = "Paper Status",
    title = "Total Papers per Year"
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(face="bold")
  )

combined_plot <- donut_plot / bar_plot + 
  plot_layout(heights = c(2, 1)) +
  plot_annotation(
    title = "Research on fuzzy logic gets retracted — a lot.",
    subtitle = "Donut charts show fractions, bar chart shows counts",
    caption = "Data: fuzzy_clean dataset",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12),
      plot.caption = element_text(size = 10, color = "gray40")
    )
  )

combined_plot

