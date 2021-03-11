devtools::install_github("allisonhorst/palmerpenguins")
library(palmerpenguins)
library(tidyverse)
summary(penguins)
head(penguins)
penguins
plot(penguins)
table(penguins$island)
plot(penguins$species,penguins$bill_depth_mm)
a<-penguins
penguins %>%
  count(species) %>%
  ggplot(aes(x = n, y = fct_reorder(species, n))) +
  geom_col()

