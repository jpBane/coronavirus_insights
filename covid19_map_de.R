# Pakete laden
library(readr)
library(stringr)
library(dplyr)
library(cartography)
library(ggplot2)

# RKI Daten je Bundesland importieren
rki_states <- read_csv("covid19_rki_states.csv")

tail(rki_states)

# Zusammenführen mit Kartendaten schwierig -> keine gemeinsame Variable
# Direkt mit den Daten auf Landkreisebene arbeiten. Da gibt es NUTS in den RKI Daten. 

# Kann ich das EU Map Shapefile aus dem cartography-Paket verwenden und einfach Deutschland filtern?
bundesgrenzen <- subset(nuts0.spdf, id == "DE")
plot(bundesgrenzen)

landesgrenzen <- subset(nuts1.spdf, str_detect(id, "DE"))
plot(landesgrenzen)

bezirke <- subset(nuts2.spdf, str_detect(id, "DE"))
plot(bezirke)

landkreise <- subset(nuts3.spdf, str_detect(id, "DE"))
plot(landkreise)

# Das geht offensichtlich

# RKI Daten je Landkreis importieren
rki_counties <- read_csv("RKI_Corona_Landkreise.csv")

head(rki_counties)


# -----------------
# Visualisierung
# -----------------
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

# Das Filtern wäre gar nicht nötig gewesen, weil nur NUTS gefüllt sind, die auch Daten enthalten. 
# Warum fehlen da im Osten so viele Landkreise?


# Welche NUTS sind in den RKI Daten enthalten?
rki_counties$NUTS

rki_counties %>% 
  filter(NUTS == "DE91C")
# Die NUTS Version ist mindestens 2016, weil DE91C vorher nicht existierte (war vorher DE915 bzw. DE919)

# Welche NUTS Version haben die Kartendaten?
landkreise@data %>% 
  filter(id == "DE91C")
# nicht 2016
landkreise@data %>% 
  filter(id == "DE915")

landkreise@data %>% 
  filter(id == "DEE11")

landkreise@data %>% 
  filter(id == "DEE01")
# nicht 2003, weil DEE01 anstatt DEE11
# es könnte die 2006, 2010 oder 2013 NUTS Version sein. 

# Wie komme ich an ein aktuelles Shapefile? (mindestens 2016)
library(eurostat)

# Diese Variante könnte auch bei Koehler funktionieren
map_de <- eurostat_geodata_60_2016 %>% 
  filter(CNTR_CODE == "DE",
         LEVL_CODE == 3)

# Für diese Variante muss erst die entsprechende URL auf die Whitelist
map_de2 <- get_eurostat_geospatial(output_class = "spdf",
                                   resolution = "60",
                                   nuts_level = "3",
                                   year = "2016")

plot(map_de2)

str(map_de)

plot(map_de)
# Warum kommt da so ein Quatsch raus? -> vermutlich weil er alle Spalten verwendet. 
# Probieren wir es einfach mit der finalen Funktion

par(mar = c(0,1,2,1))
choroLayer(spdf = map_de2, 
           df = rki_counties, 
           dfid = "NUTS", 
           var = "cases7_per_100k", 
           col = carto.pal("red.pal", n1 = 12),
           method = "pretty",
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

# Berlin funktioniert noch nicht

rki_counties %>% 
  filter(str_detect(GEN, "Berlin")) %>% 
  select(GEN, BEZ, cases7_per_100k, NUTS)
