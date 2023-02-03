
# Librerias ---------------------------------------------------------------
library(tidyverse)
library(readxl)


# Load Data ---------------------------------------------------------------

ltem <- readRDS("data/LTEM_database.RDS")
ltem <- read_xlsx("data/LTEM_database.xlsx")


# Biomass -----------------------------------------------------------------

glimpse(ltem)

db <- ltem  |>  
  mutate(A_ord = as.numeric(A_ord), 
         B_pen= as.numeric(B_pen),
         Quantity = as.numeric(Quantity ),
         Size=as.numeric(Size),
         Area= as.numeric(Area),
         Biomass = (Quantity * A_ord* (Size^B_pen))/(Area * 100)) |>  
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
  filter(!is.na(TrophicGroup)) |>                 
  group_by(Region, TrophicGroup) |> 
  summarise(Biomass= mean(Biomass)) |> 
ggplot(aes(fill=TrophicGroup, y=Biomass, x=Region)) +
        
geom_bar(position="fill", stat="identity", col="#4287f5") +
        theme_classic()+
        labs(x="", fill="Trophic Group", y="Biomass (ton/ha)")
        theme()




  