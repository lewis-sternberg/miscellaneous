####DFID, NORAD and SIDA IATI flows to WASH, 2001 to 2025####
#Lewis Sternberg

list.of.packages <- c("data.table","splitstackshape","ggplot2","viridis","stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("C:/git/miscellaneous/IATI_WASH")

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

IATI <- setDT(read.csv("IATI.csv",na.strings=""))
IATI[is.na(sector.percentage),sector.percentage:="100"]
IATI[,year:=left(IATI$budget.period.end.date,4)]
IATI <- cSplit(IATI,c("sector.code","sector.percentage"),sep=";",direction="long")
org <- c("GB-","SE-","NO-") #DFID, SIDA, NORAD

for(i in org){
IATI_ORG <- IATI[iati.identifier %like% paste0("^",i,"*")]
IATI_ORG_sector_year <- IATI_ORG[sector.code %in% WASH,.(value=sum(sector.percentage/100*budget.value)),by=.(sector.code,year)]
IATI_ORG_year <- IATI_ORG[sector.code %in% WASH,.(value=sum(sector.percentage/100*budget.value)),by=.(year)]

ggplot(IATI_ORG_sector_year,aes(x=year,y=value/1000000,group=as.character(sector.code),fill=as.character(sector.code)))+
  geom_area(alpha=0.9,size=0.5,colour="white")+
  scale_fill_viridis(discrete = T)+
  ggtitle(i)

ggplot(IATI_ORG_year, aes(x=year,y=value/1000000, group=1))+
  geom_line()+
  geom_point()+
  ggtitle(i)

IATI_ORG_sector_year_wide <- dcast(IATI_ORG_sector_year, sector.code~year)
fwrite(IATI_ORG_sector_year_wide,paste0("sector and year for, ",i,".csv"))
fwrite(IATI_ORG_year,paste0("WASH by year for, ",i,".csv"))
}