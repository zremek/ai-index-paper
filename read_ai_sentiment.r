library(tidyverse)

# data source: AI-Related Social Media Conversations Net Sentiment
# Values (not Scores) https://d3i91vx6n7fixv.cloudfront.net/ 

# AI-Related Social Media Conversations Net Sentiment: The overall sentiment of social media conversations about AI in a given country, calculated as the difference between positive and negative mentions. A net sentiment score of +100 means that all conversations are positive; a score of -100 means that all conversations are negative. Source: Quid, 2023. Data last updated in March 2024.

wide_sntm <- read_delim(
  "values-AI-Rel-Social-Media-Conv-Net-Sentiment.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)

sntm <- pivot_longer(wide_sntm, !cntry, names_to = "year", values_to = "net_sentiment")

sntm$year <- as.numeric(sntm$year)

ggplot(sntm, aes(x = year, y = net_sentiment)) + 
  geom_point() +
  geom_line() + 
  facet_wrap(~cntry) + 
  scale_x_continuous(breaks = 2020:2023) + 
  labs(title = "AI-Related Social Media Conversations Net Sentiment, Quid")
