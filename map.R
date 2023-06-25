
library(ggplot2)
library(sf)
library(geojsonsf)
library(RColorBrewer)


cfps2014comm_1%>%
  group_by(provcd14)%>%
  select(provcd14,clan)%>%
  dplyr::summarise(clan_sum=sum(clan))->df2
d <- as.data.frame(attributes(df2$provcd14)$labels)
d2 <- rownames(d)
d3 <- cbind(d,d2)
d4 <- d3[-c(1:5),]
colnames(d4) <- c("provcd14","label")
d5 <- merge(df2,d4,by = "provcd14")
colnames(d5) <- c("provcd14","clan","name")

# table(cfps2014comm$clan)
map_china = read_sf("https://geo.datav.aliyun.com/areas_v2/bound/100000_full.json")

API_pre = "http://xzqh.mca.gov.cn/data/"

China = st_read(dsn = paste0(API_pre, "quanguo.json"), 
                stringsAsFactors=FALSE) 

st_crs(China) = 4326

China_line = st_read(dsn = paste0(API_pre, "quanguo_Line.geojson"), 
                     stringsAsFactors=FALSE)

st_crs(China_line) = 4326
gjx <- China_line[China_line$QUHUADAIMA == "guojiexian",]


province_mid <- read.csv("https://raw.githubusercontent.com/slyang-cn/data/slyangcn/province.csv")

map_china_clan <- left_join(map_china,d5,by="name")
map_china_clan

library(tidyverse)

palette <- colorRampPalette(RColorBrewer::brewer.pal(9,name = 'Set1'))(length(unique(map_china_clan$clan)))
colourCount = length(unique(map_china_clan$clan))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

showtext::showtext_auto()
ggplot(map_china_clan)+
  geom_sf(aes(fill=factor(clan)))+
  geom_sf(data = gjx)+
  scale_fill_manual(values = getPalette(colourCount))+
  theme_bw()
ggsave("map_clan.png")
library(mapchina)
head(china)






