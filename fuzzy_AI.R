library(tidyverse)
library(stringr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(ggtext)

setwd('~/Desktop/r/fuzzy')
retractions <- read_csv("retraction_watch-2.csv")
view(retractions)

# counting all AI retractions ever, (9090/68337 as of Jan 2026)

nrow(retractions)
count_aided <- sum(grepl("\\bComputer-Aided\\b", retractions$Reason, ignore.case = TRUE))
count_generated <- sum(grepl("\\bComputer-Generated\\b", retractions$Reason, ignore.case = TRUE))
print(paste("Computer-Aided:", count_aided))
print(paste("Computer-Generated:", count_generated))

# cleaning the Retraction Watch data, separating by year and for "fuzzy."

filtered_retractions <- retractions %>%
    mutate(RetractionDate = as.Date(RetractionDate, format = "%m/%d/%Y"))

year_retractions <- filtered_retractions %>%
  mutate(Year = format(as.Date(RetractionDate, "%m/%d/%Y"), "%Y"))

fuzzy_retractions <- year_retractions %>%
  filter(str_detect(Title, regex("fuzzy", ignore_case = TRUE)))

fuzzy_retractions <- fuzzy_retractions %>% select(-Author, -URLS, -RetractionNature, -ArticleType, -Paywalled)

view(fuzzy_retractions)

#seeing about what countries

country_summary <- fuzzy_retractions %>%
  group_by(Country) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# country chart by retractions

country_summary %>%
  head(10) %>%
  ggplot(aes(x = reorder(Country, count), y = count)) +
  geom_bar(stat = "identity", fill = "#5097A4") +
  geom_text(aes(label = count), hjust = -0.2, family = "mono", size = 3) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Retracted Papers on Fuzzy Logic by Country (Top 10)",
       x = NULL,
       y = "Number of Retractions") +
  theme_minimal() +
  theme(text = element_text(family = "mono"),
        axis.text = element_text(family = "mono"),
        plot.title = element_text(family = "mono"),
        axis.title.y = element_text(vjust = 4))

# fuzzy retractions on AI/computer papers

computer_papers <- fuzzy_retractions[grepl("computer", fuzzy_retractions$Subject, ignore.case = TRUE), ]
nrow(computer_papers)

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
  geom_point() +
  labs(title = "Fuzzy Logic Paper Retractions Over Time", 
       y = "Papers Retracted", color = "Retractions") +
  theme_minimal() +
  theme(text = element_text(family = "mono"),
        axis.text = element_text(family = "mono"),
        plot.title = element_text(family = "mono"),
        legend.text = element_text(family = "mono"))


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

# donut plot with gradient fill and bold labels

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
  theme(
    strip.text = element_text(face="bold", size=12),
    text = element_text(family = "mono"),) +
  ggtitle("Paper Status by Year")

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
    axis.text.x = element_text(face="bold"),
    text = element_text(family = "mono"),
  )

combined_plot <- donut_plot / bar_plot + 
  plot_layout(heights = c(2, 1)) +
  plot_annotation(
    title = "Research on the fringe theory of fuzzy logic gets retracted — a lot.",
    subtitle = "And, most retractions regarding are due to AI use, the very concept fuzzy logic was once thought to inform.",
    caption = "Data: Retraction Watch, PubMed",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 9),
      plot.caption = element_text(size = 8)
    ) 
  )  

combined_plot

