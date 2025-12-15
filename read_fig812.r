library(tidyverse)

# data source: Figure 8.1.2 https://hai.stanford.edu/ai-index/2025-ai-index-report 

fig812 <- read_delim(
  "fig-812-more-benef-than-draw-data.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)

# new IPSOS report has data from 2025 
# https://www.ipsos.com/sites/default/files/ct/publication/documents/2025-06/Ipsos-AI-Monitor-2025.pdf 



cntry_2025 <- c("Indonesia", "Thailand", "Singapore", "Mexico", "Malaysia", "Peru", 
  "South Korea", "India", "Turkey", "South Africa", "Chile", "Colombia", 
  "Brazil", "Argentina", "Italy", "Spain", "Poland", "Germany", "Hungary", 
  "Ireland", "Japan", "Australia", "Switzerland", "Netherlands", 
  "Great Britain", "Sweden", "United States", "Belgium", "France", "Canada")

vals_2025 <- c(85, 78, 73, 70, 70, 69, 67, 67, 66, 64, 63, 58, 58, 57, 53, 53, 
               50, 49, 48, 48, 48, 47, 45, 43, 43, 43,42, 41, 41, 40)

ipsos_2025 <- tibble(cntry = cntry_2025, 
                year = 2025, 
                pct = vals_2025)

ipsos <- bind_rows(ipsos_2025, fig812)

ggplot(fig812, aes(cntry, pct, fill = factor(year))) + geom_col(position = "dodge") + coord_flip()
ggplot(ipsos, aes(cntry, pct, fill = factor(year))) + geom_col(position = "dodge") + coord_flip()


wide_fig812 <- pivot_wider(fig812, names_from = year, values_from = pct)

y3 <- na.omit(wide_fig812)

dim(y3)

ggplot(fig812, aes(x = year, y = pct)) + 
  geom_point() +
  geom_line() + 
  facet_wrap(~cntry) + 
  scale_x_continuous(breaks = 2022:2024) + 
  labs(title = "Products and services using AI have more benefits\nthan drawbacks [% 'Agree'], Ipsos")

# 22-25
ggplot(ipsos, aes(x = year, y = pct)) + 
  geom_point() +
  geom_line() + 
  facet_wrap(~cntry) + 
  scale_x_continuous(breaks = 2022:2025) + 
  labs(title = "Products and services using AI have more benefits\nthan drawbacks [% 'Agree'], Ipsos")

ggplot(fig812, aes(x = year, y = pct, colour = cntry)) + 
  geom_point() +
  geom_line() + 
  scale_x_continuous(breaks = 2022:2024)

# better 22-25?
ggplot(ipsos, mapping = aes(x = reorder(cntry, pct, FUN = median, na.rm = T),
                            y = pct, 
                            colour = factor(year), 
                            group = cntry)) + 
  geom_point() + 
  geom_line() +
  coord_flip() +
  labs(title = "Products and services using AI have more benefits\nthan drawbacks [% 'Agree'], Ipsos")




