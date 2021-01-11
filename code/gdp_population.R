# Load tidyverse packages

library(tidyverse)

# Read in data
gapminder_1997 <- read.csv("data/gapminder_1997.csv")
View(gapminder_1997)

?read.csv

read.csv(file="gapminder_1997.csv")

# Function
round(3.1415)
round(3.1415,5)

round(x = 3.1415)
round(x = 3.1415, digits = 2)
round(digits = 2, x = 3.1415)
round(2, 3.1415)

# Make a plot
ggplot(data = gapminder_1997) +
  aes(x= gdpPercap, y=lifeExp, color=continent, size=pop/100000, shape = continent) +
  labs(title="Do people in wealthy countries live longer?", x="GDP Per Capita", y="Life Expectancy", size="Population (in millions)") +
  geom_point() +
  scale_color_brewer(palette = "Set3") 

# Or can be done like below
ggplot(data = gapminder_1997, aes(x= gdpPercap, y=lifeExp, color=continent, size=pop/100000, shape = continent)) +
  labs(title="Do people in wealthy countries live longer?", x="GDP Per Capita", y="Life Expectancy", size="Population (in millions)") +
  geom_point() +
  scale_color_brewer(palette = "Set1") 

# Different color palettes
RColorBrewer::display.brewer.all()

# Read in the full data
gapminder_data <- read_csv(file="gapminder_data.csv")

dim(gapminder_data)

ggplot(data=gapminder_data) +
  aes(x=year, y=lifeExp, color=continent, group=country) +
  geom_line()

# Plot categorical variables
ggplot(data=gapminder_1997) +
  aes(x=continent, y=lifeExp) +
  labs(x="Continent", y="Life Expectancy(years)",title="Life expectancy by continent(1997)")+
  geom_boxplot()+
  geom_jitter(aes(size=pop))+
  geom_violin(alpha=0.5, color='darkolivegreen', fill='deepskyblue3') 
  sample(colors(), size = 100)
  
 awesome_pic<- ggplot(gapminder_1997)+
    aes(x=lifeExp)+
    geom_histogram(bins=20, color='green', fill='royalblue')+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=0.5))
 
 ggsave(filename = "figures/awesome_pic.jpg", plot=awesome_pic, height = 8, width = 12, units = "in")
