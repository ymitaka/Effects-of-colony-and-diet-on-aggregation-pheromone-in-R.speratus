### Mapping colony locations


## Before running this codes, you need to download the GML format data from the website of the "National Spatial Planning and Regional Policy Bureau, MLIT of Japan" 
# The whole Japan map: N03-20210101_GML.zip (https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N03-2025.html)
# Lake map: W09-05_GML.zip (https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-W09-2005.html)


## Using "ggplot2" package

library(tidyverse)
library(ggplot2)
library(ggrepel)
library(sf)
library(mapdata)
library(NipponMap)
library(rmapshaper)

map <- read_sf("N03-20210101_GML/N03-21_210101.shp")
lake <- read_sf("W09-05_GML/W09-05-g_Lake.shp")

# Extract the data of the Kinki and Tokai areas 
map2 <- map	%>% 
		filter(N03_001=="福井県" | N03_001=="長野県" | N03_001=="岐阜県" | N03_001=="愛知県" | N03_001=="静岡県" | 
		N03_001=="三重県" | N03_001=="滋賀県" | N03_001=="奈良県" | N03_001=="和歌山県" | N03_001=="大阪府" | 
		N03_001=="京都府" | N03_001=="兵庫県") %>%
		ms_dissolve(field="N03_001") %>% 
		ms_simplify(keep = 0.1, keep_shapes = TRUE)

# Extract the GPC data of Biwa Lake
biwa <- lake[lake$W09_001=="琵琶湖", ]
st_crs(biwa) = "+proj=longlat +datum=WGS84"
biwa <- ms_dissolve(biwa, field="W09_001", copy_fields=c("W09_001")) 

# Read colony location data
data <- read.csv("Used colony locations.csv", header=T)

p <- ggplot() + geom_sf(data = map2) + 
	geom_sf(data = ms_innerlines(map2), color="white") + 
	geom_sf(data = biwa, fill="white", color="white") + 
	theme_light() + 
	geom_point(data = data, aes(x = lng, y = lat), size = 1) + 
	geom_text_repel(data = data, aes(x = lng, y = lat, label=Colony), min.segment.length = 0, segment.color = "gray50", size=3) + 
	xlim(c(135.2, 137.5)) + ylim(c(34.5, 35.8)) + 
	labs(x="Longitude", y="Latitude")
plot(p)


# Draw the whole Japan map
map_japan <- maps::map("japan", plot = FALSE, fill = TRUE) %>%
  st_as_sf() %>%
  rename(pref_name = ID) %>% 
  mutate(pref_name = str_to_title(pref_name))

p1 <- ggplot() + 
		geom_sf(data = map_japan) + 
		coord_sf(crs=sf::st_crs("EPSG:3857"),default_crs = sf::st_crs("EPSG:4326")) + 
		theme_bw()
dev.new(width=8, height=7)
plot(p1)
