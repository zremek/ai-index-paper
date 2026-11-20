library(tidyverse)

# data source: Figure 8.1.2 https://hai.stanford.edu/ai-index/2025-ai-index-report 

fig812 <- read_delim(
  "fig-812-more-benef-than-draw-data.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)

# 2021 data 

ipsos_2021 <- tibble(
  cntry = c(
    "China",
    "Saudi Arabia",
    "India",
    "Peru",
    "Mexico",
    "Malaysia",
    "Colombia",
    "Chile",
    "South Korea",
    "Turkey",
    "Brazil",
    "South Africa",
    "Argentina",
    "Spain",
    "Russia",
    "Italy",
    "Hungary",
    "Poland",
    "Japan",
    "Sweden",
    "Belgium",
    "Great Britain",
    "Australia",
    "Germany",
    "United States",
    "Netherlands",
    "Canada",
    "France"
  ),
  year = 2021,
  pct = c(
    78,
    76,
    71,
    70,
    65,
    65,
    64,
    63,
    62,
    60,
    57,
    57,
    55,
    53,
    53,
    50,
    49,
    48,
    42,
    40,
    38,
    38,
    37,
    37,
    35,
    33,
    32,
    31
  )
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

ipsos <- bind_rows(ipsos_2025, fig812, ipsos_2021)

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

# 21-25
ggplot(ipsos, aes(x = year, y = pct)) + 
  geom_point() +
  geom_line() + 
  geom_hline(yintercept = 50, linetype = "dashed", colour = "gray50") +
  facet_wrap(~cntry) + 
  scale_x_continuous(breaks = 2021:2025) + 
  theme_classic() +
  labs(title = "Products and services using AI have more benefits\nthan drawbacks [% 'Agree'], Ipsos")

ggplot(fig812, aes(x = year, y = pct, colour = cntry)) + 
  geom_point() +
  geom_line() + 
  scale_x_continuous(breaks = 2022:2024)

# better 21-25?
ggplot(ipsos, mapping = aes(x = reorder(cntry, pct, FUN = median, na.rm = T),
                            y = pct, 
                            colour = factor(year), 
                            group = cntry)) + 
  geom_point() + 
  geom_line() +
  coord_flip() +
  labs(title = "Products and services using AI have more benefits\nthan drawbacks [% 'Agree'], Ipsos")

# another 21-25?
ggplot(ipsos, mapping = aes(x = year, y = pct, colour = cntry)) +
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = 2021:2025)
  labs(title = "Products and services using AI have more benefits\nthan drawbacks [% 'Agree'], Ipsos")


# add 2026 - /Users/remek/Code/ai-index-paper/Ipsos-AI-Monitor-2026.pdf 

  cntry_2026 <- c("China",
                  "India",
                  "Indonesia",
                  "Thailand",
                  "Peru",
                  "Malaysia",
                  "Singapore",
                  "South Korea",
                  "Mexico",
                  "South Africa",
                  "Turkey",
                  "Colombia",
                  "Chile",
                  "Argentina",
                  "Brazil",
                  "Spain",
                  "Italy",
                  "Israel",
                  "Japan",
                  "Hungary",
                  "Ireland",
                  "Poland",
                  "Sweden",
                  "New Zealand",
                  "Australia",
                  "Belgium",
                  "Great Britain",
                  "France",
                  "United States",
                  "Netherlands",
                  "Germany",
                  "Canada")
  
  
  
  vals_2026 <- c(85, 
                 80,
                 77,
                 74,
                 71,
                 70,
                 68,
                 67,
                 66,
                 66,
                 65,
                 63,
                 60,
                 56,
                 56,
                 54,
                 53,
                 51,
                 50,
                 50,
                 47,
                 47,
                 47,
                 46,
                 43,
                 41,
                 38,
                 38,
                 38,
                 38,
                 37,
                 34)
  
  ipsos_2026 <- tibble(cntry = cntry_2026, 
                       year = 2026, 
                       pct = vals_2026)
  

  ipsos21_26 <- bind_rows(ipsos_2026, ipsos_2025, fig812, ipsos_2021)

  ipsos21_26 |> count(cntry) |> print(n = Inf) 
  
  
  
  # 21-26 #### 
  ggplot(ipsos21_26, aes(x = year, y = pct)) + 
    geom_point() +
    geom_line() + 
    geom_hline(yintercept = 50, linetype = "dashed", colour = "gray50") +
    facet_wrap(~cntry) + 
    theme_classic() +
    labs(title = "Products and services using AI have more benefits\nthan drawbacks [% 'Agree'], Ipsos")
  
  
  ipsos21_26 |> na.omit() |> count(cntry) |> print(n = Inf)
  
  
  ipsos21_26 |> na.omit() |> count(cntry) |> filter(n == 6)|> print(n = Inf)
  
  wider21_26 <- ipsos21_26 |> na.omit() |> pivot_wider(names_from = year, values_from = pct)
  wider21_26 <- wider21_26 |> 
    mutate(
      complete_cntry = complete.cases(pick(everything()))
      )

ipsos_complete <- left_join(ipsos21_26, 
                            wider21_26 |> select(cntry, complete_cntry))
  
  # 21-26 + complete_cntry #### 
  ggplot(ipsos_complete, aes(x = year, y = pct, colour = complete_cntry)) + 
    geom_point() +
    geom_line() + 
    geom_hline(yintercept = 50, linetype = "dashed", colour = "gray50") +
    facet_wrap(~cntry) + 
    theme_classic() +
    labs(title = "Products and services using AI have more benefits\nthan drawbacks [% 'Agree'], Ipsos")
  
save(ipsos_complete, file = "ipsos_complete.RData")

