library('tidyverse')
library('gapminder')
library('maps')
source('map_country.R')





plot1 <- gapminder %>%
  ggplot(aes(y=lifeExp, fill = continent)) + 
  geom_boxplot() + 
  scale_fill_manual(values = continent_colors) 



latest_year <- max(gapminder$year)

# boxplots by continent latest year
ggplot(gapminder[gapminder$year == latest_year, ], aes(x = continent, y = gdpPercap)) +
  geom_boxplot() +
  labs(title = "GDP per Capita by Continent (Latest Year)", y = "GDP per Capita") +
  theme_bw()
ggsave('gdp_box_plots.png',width=10, height=6)
ggplot(gapminder[gapminder$year == latest_year, ], aes(x = continent, y = lifeExp)) +
  geom_boxplot() +
  labs(title = "Life Expectancy by Continent (Latest Year)", y = "Life Expectancy") +
  theme_bw()
ggsave('life_box_plots.png',plot=plot1, width=10, height=6)

# mean GDP and life expectancy time series
gapminder_mean <- gapminder %>%
  group_by(continent, year) %>%
  summarize(mean_gdp = mean(gdpPercap), mean_lifeExp = mean(lifeExp))

ggplot(gapminder_mean, aes(x = year, y = mean_gdp, color = continent)) +
  geom_line() +
  labs(title = "Mean GDP per Capita over Time by Continent", y = "Mean GDP per Capita") +
  theme_bw()
ggsave('gdp_time_series.png', width=10, height=6)

ggplot(gapminder_mean, aes(x = year, y = mean_lifeExp, color = continent)) +
  geom_line() +
  labs(title = "Mean Life Expectancy over Time by Continent", y = "Mean Life Expectancy") +
  theme_bw()
ggsave('life_time_series.png', width=10, height=6)

# scatterplot population size by year

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, size = pop, + scale_x_continuous(breaks = NULL), + scale_y_continuous(breaks=NULL))) +
  geom_point() +
  facet_wrap(~year) +
  labs(title = "Life Expectancy vs GDP per Capita by Year", x = "GDP per Capita", y = "Life Expectancy") +
  theme_bw() 
ggsave('scatter_plot1.png', width=10, height=10)

# scatterplot with log gdp per capita
ggplot(gapminder, aes(x = log(gdpPercap), y = lifeExp, size = pop, + scale_x_continuous(breaks = NULL), + scale_y_continuous(breaks=NULL))) +
  geom_point() +
  facet_wrap(~year) +
  labs(title = "Life Expectancy vs Log(GDP per Capita) by Year", x = "Log(GDP per Capita)", y = "Life Expectancy") +
  theme_bw()
ggsave('scatter_plot2.png', width=10, height=10)