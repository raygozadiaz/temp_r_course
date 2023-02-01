##DATA WRANGLING = Acomodo, organización y limpieza de nuestras bases de datos

#Cargar nuestras librerías
library(readxl)
library(tidyverse)

#Importar nuestro dataset
data <- read_xlsx("r_course/data/LTEM_database.xlsx", sheet=1)




##Explorar nuestro dataset

#Nos arroja la primera o última parte del dataframe
head(data)
tail(data)

#Un vistazo a nuestro df: varibles y valores
glimpse(data)

#Nos genera un string de los nombres de las columnas
colnames(data)

#Encontrar valores únicos dentro de una varible
unique(data$Label)
unique(data$Depth)
unique(data$Year)

#El rango en que se distribuyen los valores de la varible
range(data$Size, na.rm = T)
range(data$Quantity)

##Data wrangling

#Clases de datos
head(data)

#Corregir la clases de las variables
data <-  data %>% 
  mutate(Year= factor(Year, levels=c(1998:2022)),
         Month= factor(Month, levels=c(1:12)),
         Depth= factor(Depth, levels=c(5,20)),
         Size= as.character(Size)
         
  )
head(data)
data <-  data %>% 
  mutate(Year= factor(Year, levels=c(1998:2022)),
         Month= factor(Month, levels=c(1:12)),
         Depth= factor(Depth, levels=c(5,20)),
         Size= as.numeric(Size)
         
  )


#Filtrar observaciones de acuerdo a su valor
#Filter()

#Filtrar todos los valores cuya Cantidad (Quantity) sea mayor o igual a 20
m <- filter(data, Quantity>= 20)

#De m, filtrar solo los datos del año 2017
y2017 <- filter(m, Year==2017)

#Aplicar varios filtros a la vez

m2017 <- filter(data, Year==2017, Quantity>= 20)


#Operadores lógicos

# > Mayor que
# < Menor que
# >= Mayor o igual
# <= Menor o igual
# != Diferente de
# == Igual que
# & es "y"
# | es "ó"
# ! es "no"

#Observaciones de monitoreo del año 1998 o 2019
filter(data, Year==1998 | Year==2019)
#De otra forma...
filter(data, Year %in% c(1998, 2019))

#Combinando Variables e intervalos

b <- filter(data, !(Size <= 8 & Quantity < 20))





#Cambiar el orden de las filas
#arrange()
ordered <- arrange(data, Year, Month, Size)

# Orden Descendente
desc <- arrange(data, Year, Month, desc(Size))


# Seleccionar variables (columnas)
# select()
select(data, Year, Month, Day)

select(data, Label, Depth:Quantity)

select(data, -(Year:IDSpecies))

#Se puede modificar el orden de las columnas
select(data, Label, main.id, Year:Quantity)


#Cambiar el nombre de una variable
#rename()

rename(data, MainID = main.id)



#Añadir nuevas variables (columnas)
#mutate()

range(data$Size)

new_data <- mutate(data, 
       total_size = Size * Quantity)

#Se pueden combinar todas las funciones usando %>% 


cleaned <- data|> 
  mutate(Year= factor(Year, levels=c(1998:2022)),
         Month= factor(Month, levels=c(1:12)),
         Depth= factor(Depth, levels=c(5,20)),
         Size= as.numeric(Size)) |> 
  filter(Year %in% c(1998, 2000, 2017, 2019)) |> 
  arrange(Year, Month, Day, Size, desc(Quantity)) |> 
  mutate(
          size_category= ifelse(Size<=25, "Tiny", NA),
          size_category= ifelse(Size >25 & Size<=50, "Small", size_category),
          size_category= ifelse(Size >50 & Size<=75, "Medium", size_category),
          size_category= ifelse(Size >75 & Size<=100, "Large", size_category),
          total_size=Size*Quantity,
          size_category= factor(size_category, levels=c("Tiny", "Small", "Medium","Large"))
  )
         
#Summaries o resúmenes
#summarise()
summarise(data, mean_size= mean(Size, na.rm=TRUE))

#Se pueden agrupar los summaries

monthly_observations <- data %>%
  group_by(Label, Year, Month, Size) %>% 
  summarise(observations= mean(Quantity))

  












