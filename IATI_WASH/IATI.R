####DFID IATI flows to WASH, 2001 to 2025####

list.of.packages <- c("data.table","splitstackshape","ggplot2","viridis")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("H:/My Documents/H drive - Current work/WASH")

left = function(text, num_char) {
  substr(text, 1, num_char)
}

WASH <- c(
  "14015",
  "14050",
  "14030",
  "14040",
  "14010",
  "14020",
  "14081",
  "14032",
  "14021",
  "14031",
  "14022"
)

DFID <- setDT(read.csv("DFID.csv",na.strings=""))
DFID[is.na(sector.percentage),sector.percentage:="100"]
DFID[,year:=left(DFID$budget.period.end.date,4)]
DFID <- cSplit(DFID,c("sector.code","sector.percentage"),sep=";",direction="long")


DFID_sector_year <- DFID[sector.code %in% WASH,.(value=sum(sector.percentage/100*budget.value)),by=.(sector.code,year)]
DFID_year <- DFID[sector.code %in% WASH,.(value=sum(sector.percentage/100*budget.value)),by=.(year)]

ggplot(DFID_sector_year,aes(x=year,y=value/1000000,group=as.character(sector.code),fill=as.character(sector.code)))+
  geom_area(alpha=0.9,size=0.5,colour="white")+
  scale_fill_viridis(discrete = T)

ggplot(DFID_year, aes(x=year,y=value/1000000, group=1))+
  geom_line()+
  geom_point()

DFID_sector_year_wide <- dcast(DFID_sector_year, sector.code~year)
fwrite(DFID_sector_year_wide,"sector and year for DFID.csv")
fwrite(DFID_year,"WASH by year for DFID.csv")
