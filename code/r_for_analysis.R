library(tidyverse)

# Load the data first
gapminder_data <- read_csv("data/gapminder_data.csv")

# Summarize
summarize(gapminder_data, mean(lifeExp))

# Piping function %>% (Ctrl+Shift+M)
# (%>%(Alt+-) 
gapminder_data %>%
  summarize(average=mean(lifeExp)) 

gapminder_data %>%
  summarize(avg_popsize=mean(pop), min_popsize=min(pop), max_popsize=max(pop), max_year=min(year)) 
  
# Filter function to select rows
gapminder_data %>%
  filter(year==2007) %>% 
  summarize(average=mean(lifeExp),avg_popsize=mean(pop), min_popsize=min(pop), max_popsize=max(pop), max_year=max(year)) 

gapminder_data %>%
  filter(year==1952) %>% 
  summarize(average=mean(gdpPercap)) 

# Group by function
gapminder_data %>%
  group_by(year) %>%
  summarize(average=mean(lifeExp))

summary_data <- gapminder_data %>%
  group_by(continent) %>%
  summarize(average=mean(lifeExp))

# Mutate function
gapminder_data %>%
  mutate(pop_in_mil=pop/1000000) 

# Select function
gapminder_data %>%
  select(year,pop)
# Drop only continent and keep other columns
gapminder_data %>%
  select(-continent)

gapminder_data %>%
  select(-pop, -gdpPercap)

gapminder_data %>%
  select(country,continent, year, lifeExp)

# Transform the data: pivot_wider() 
gapminder_data %>%
  select(country, continent, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp, names_prefix="yr_" )

# Exercise: Create new data
Americas_data_2007 <- 
gapminder_data %>%
  filter(continent=="Americas", year==2007) %>% 
  select(-continent, -year)

## Read a new file
co2_emissions <-read_csv("data/co2-un-data.csv", skip=2,
         col_names = c("region", "country", "year", "series", "value", "footnotes", "source")) %>%  
  select(country, year, series, value) %>%  
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita")) %>%  
  pivot_wider(names_from=series, values_from=value) %>% 
  filter(year==2005) %>%  
  select(-year) %>% 
  mutate(country=recode(country,
                        "Bolivia (Plurin. State of)" = "Bolivia",
                        "United States of America" = "United States",
                        "Venezuela (Boliv. Rep. of)" = "Venezuela"))

# Join tables
anti_join(gapminder_data, co2_emissions, by="country")


gapminder_data <- read_csv("data/gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent) %>%
  mutate(country = recode(country, "Puerto Rico" = "United States")) %>%
  group_by(country) %>%
  summarize(lifeExp = sum(lifeExp * pop)/sum(pop), gdpPercap = sum(gdpPercap * pop)/sum(pop), pop = sum(pop))

anti_join(gapminder_data, co2_emissions, by="country")

gapminder_co2 <- inner_join(gapminder_data, co2_emissions, by="country")

ggplot(gapminder_co2, aes(x=gdpPercap, y=per_capita)) +
  geom_point() +
  labs(x="GDP (per capita)",
       y="CO2 emitted (per capita)",
       title="There is a strong association between a nation's GDP \nand the amount of CO2 it produces"
  ) +
  geom_smooth(method="lm")
