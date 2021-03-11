devtools::install_github("allisonhorst/palmerpenguins")
library(palmerpenguins)
summary(penguins)
head(penguins)
penguins
plot(penguins)
table(penguins$island)
plot(penguins$species,penguins$bill_depth_mm)
plot(penguins$species,penguins$bill_length_mm)
a<-penguins
library(tidyverse)
glimpse(penguins)

1:10 %>% sum()
1:3 %in% c(1,3,5)

#filter와 subset은 같다??

penguins %>% filter(bill_length_mm <=100,bill_length_mm>=30)
penguins %>% select(species, bill_length_mm, bill_depth_mm) %>% filter(species=="Gentoo") %>% mutate(bill_length_mm = ceilings(bill_length_mm)) %>% arrange(bill_length_mm, desc(bill_depth_mm))
penguins %>% 
  filter(species %in% c("Chinstrap", "Adelie"), island == "Dream")
#How to write the following using the above logical operator?

penguins %>% 
  filter((species == "Chinstrap" | species == "Adelie"), island == "Dream")

  