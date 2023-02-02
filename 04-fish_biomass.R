library(tidyverse)
library(readxl)

ltem <- readRDS("r_course/data/LTEM_database.RDS")

db <- ltem  |>  
  mutate(A_ord = as.numeric(A_ord), 
         B_pen = as.numeric(B_pen),
         Quantity = as.numeric(Quantity ),
         Size=as.numeric(Size),
         Area= as.numeric(Area),
         Biomass = (Quantity * A_ord * (Size^B_pen))/(Area * 100)) |>  
  mutate(TrophicGroup = factor(TrophicGroup, 
                               levels = c("Piscivoro", 
                                          "Carnivoro", 
                                          "Herbivoro", 
                                          "Zooplanctivoro")), 
         Region = factor(Region),
         TrophicLevelF = cut(as.numeric(TrophicLevel), 
                             breaks = c(2, 2.5, 3, 3.5, 4, 4.6), 
                             labels = c("2-2.5", "2.5-3", "3-3.5", "3.5-4", "4-4.5"), 
                             right = FALSE)
  )




db |> 
  filter(Region %in% c("La Paz", "Loreto", "Los Cabos", "Cabo Pulmo")) |> 
  group_by(Region, TrophicGroup) |> 
  summarise(Biomass= mean(Biomass2)) |> 
  ggplot(aes(x=Region, y=Biomass, fill=Region))+
  geom_violin(alpha=0.5, trim = F)+
  geom_boxplot(alpha=0.5, fill=NA, aes(col=Region), size=0.2)+
  geom_jitter(aes(col=TrophicGroup), alpha= 0.5)





  