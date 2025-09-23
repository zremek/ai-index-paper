library(tidyverse)

# data source: Figure 8.1.2 https://hai.stanford.edu/ai-index/2025-ai-index-report 

fig812 <- read_delim(
  "fig-812-more-benef-than-draw-data.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)

ggplot(fig812, aes(cntry, pct, fill = factor(year))) + geom_col(position = "dodge") + coord_flip()

wide_fig812 <- pivot_wider(fig812, names_from = year, values_from = pct)

y3 <- na.omit(wide_fig812)

dim(y3)

ggplot(fig812, aes(x = year, y = pct)) + 
  geom_point() +
  geom_line() + 
  facet_wrap(~cntry) + 
  scale_x_continuous(breaks = 2022:2024) + 
  labs(title = "Products and services using AI have more benefits\nthan drawbacks [% 'Agree'], Ipsos")

ggplot(fig812, aes(x = year, y = pct, colour = cntry)) + 
  geom_point() +
  geom_line() + 
  scale_x_continuous(breaks = 2022:2024)

