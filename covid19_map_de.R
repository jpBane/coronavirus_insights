# Pakete laden
library()

# Germany on a state level
rki_states <- read_csv("covid19_rki_states.csv")

tail(rki_states)

# Subset the EU spatial data to obtain only Germany
test <- subset(nuts0.spdf, id == "DE")
test2 <- subset(nuts1.spdf, str_detect(id, "DE"))

plot(test)
plot(test2)
plot(nuts3.spdf)

rki_counties <- read_csv("RKI_Corona_Landkreise.csv")

head(rki_counties)

rki_counties$NUTS

rki_counties %>% 
  filter(NUTS == "DED2E")

par(mar = c(0,1,2,1))
choroLayer(spdf = nuts3.spdf, 
           df = rki_counties, 
           dfid = "NUTS", 
           var = "cases7_per_100k", 
           col = carto.pal("red.pal", n1 = 9),
           method = "quantile",
           legend.title.txt = "Legend",
           legend.pos = "right",
           legend.frame = T)
layoutLayer(title = "Confirmed Covid-19 cases in Germany",
            frame = F,
            col = "white",
            coltitle = "black",
            author = paste("Data in this map from:"),
            north = T,
            scale = 100)

# Warum fehlen die neuen Bundesländer?
rki_counties$NUTS

rki_counties %>% 
  filter(is.na(NUTS))

# Es fehlen nur einige NUTS in Berlin. Stimmen die Bezeichnungen nicht?
# Beispiel Bautzen: DED2C
# Covid-19 Zahlen
rki_counties %>% 
  filter(NUTS == "DED2C")
# Kartendaten
subset(nuts3.spdf, id == "DED2C") # gibt es offensichtlich nicht
subset(nuts3.spdf, id == "DE146") # Biberach gibt es

nuts3.spdf@data %>% 
  filter(str_detect(id, "DED2"))

# Warum passt das was im Osten nicht zusammen? Wie alt sind die Kartendaten?
nuts3.spdf@data %>% 
  filter(id == "DE919")
# nicht 2016
nuts3.spdf@data %>% 
  filter(id == "DEE11")

# Welche NUTS Versionen haben die RKI Daten?
rki_counties %>% 
  filter(NUTS == "DE91C")
# mindestens 2016, weil DE91C vorher nicht existiert hat (war DE915 bzw. DE919)

library(eurostat)

map_de <- eurostat_geodata_60_2016 %>% 
  filter(CNTR_CODE == "DE",
         LEVL_CODE == 3)

str(map_de)

plot(map_de)