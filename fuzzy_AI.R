library(tidyverse)
library(stringr)
library(dplyr)
library(tidyr)
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


#final plot

pie_df <- fuzzy_clean %>%
  filter(!is.na(Year), Year >= 2019, Year != 2026) %>%
  mutate(status = ifelse(Still_Live == 1, "Still live", "Retracted")) %>%
  count(Year, status) %>%
  complete(
    Year,
    status = c("Still live", "Retracted"),
    fill = list(n = 0)
  ) %>%
  group_by(Year) %>%
  mutate(
    total = sum(n),
    fraction = ifelse(total > 0, n / total, 0),
    ymax = cumsum(fraction),
    ymin = lag(ymax, default = 0),
    mid = (ymax + ymin) / 2
  ) %>%
  ungroup() %>%
  mutate(
    radius = 1,
    xmin = 0,
    xmax = radius,
    label_x = radius * 0.6
  )

# Ensure correct year order
pie_df$Year <- factor(pie_df$Year, levels = 2019:2025)

ggplot(pie_df,
       aes(ymax = ymax,
           ymin = ymin,
           xmax = xmax,
           xmin = xmin,
           fill = status)) +
  
  geom_rect(color = "white", size = 0.4) +
  coord_polar(theta = "y") +
  facet_wrap(~ Year, nrow = 2) +
  
  geom_text(
    aes(x = label_x, y = mid, label = ifelse(n > 0, n, "")),
    color = "white",
    fontface = "bold",
    size = 3
  ) +
  
  scale_fill_manual(values = c(
    "Still live" = "#5097A4",
    "Retracted" = "#CD5555"
  )) +
  
  theme_void() +
  labs(
    title = "Research on fuzzy logic gets retracted — a lot.",
    subtitle = "Most retractions are due to AI use, the very concept the \nfringe theory of fuzzy logic was once thought to inform.",
    caption = "Data: Retraction Watch, PubMed",
    fill = "Paper Status"
  ) +
  
  theme(
    strip.text = element_text(face = "bold", size = 12),
    text = element_text(family = "Futura"),
    plot.title = element_text(
      face = "bold",
      size = 13,  
      margin = margin(b = 8)
    ),
    plot.subtitle = element_text(
      size = 11,
      margin = margin(b = 20)
    ),
    plot.margin = margin(40, 60, 40, 60),
    legend.position = c(.8, .3),  
    legend.justification = c(0, 0.5),
  )
