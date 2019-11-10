library(tidyverse)
data(mpg)

ggplot(data = mpg) +
  geom_point(mapping = aes(
    x = hwy,
    y = displ,
    color = class
  )) +
  labs(
    title = "Fuel efficiency vs. engine size",
    x = "Fuel Efficiency",
    y = "Engine Size"
  ) +
  facet_wrap(~ class)
  
facet_grid(cyl ~ class)

glimpse(mpg)
table(mpg$class)

ggplot(data = mpg) +
  geom_bar(aes(
    x = class,
    fill = class
    ))

ggplot(mpg) +
  geom_boxplot(aes(y = hwy, x = class))
