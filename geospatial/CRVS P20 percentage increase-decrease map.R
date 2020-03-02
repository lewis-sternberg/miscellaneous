required.packages <- c("dplyr","ggplot2","data.table","tmap","sf","leaflet")
lapply(required.packages, require, character.only=T)


####data####
names <- read.csv("C:/map_name.csv")
names(names)[1] <-"iso3"
names <- unique(names)

data("World")

####using most recent and least recent years with daa####
plot_dat <- World
plot_dat <- plot_dat[plot_dat$continent != "Antarctica",]
#plot_dat <- plot_dat[plot_dat$continent == "Africa",] #for Africa only map
plot_dat <- plot_dat[,c(1,16)]
names(plot_dat)[1] <- "iso3"
dat <- fread("C:/git/MICS_recode/output/dhs.mics BR global p20.csv",na.string="NA") #change based on chosen data
dat <- dat[,c("iso3","povcal_year","P20")]
dat <- dat[complete.cases(dat)]
dat[,min:=min(povcal_year),by=.(iso3)][,max:=max(povcal_year),by=.(iso3)]
dat <- dat[dat[,povcal_year==min|povcal_year==max,by=.(iso3)]$V1==TRUE,]
change <- dat %>%
  group_by(iso3)%>%
  mutate(change=((P20 - lag(P20))/lag(P20))*100) %>%
  mutate(old=(lag(P20)*100)) %>%
  mutate(new=(P20*100))
change <- change[complete.cases(change),]
change$povcal_year <- NULL
change$P20 <- NULL

plot_dat <- left_join(plot_dat,change,by="iso3")
plot_dat <- left_join(plot_dat,names, by="iso3")

plot_dat <- plot_dat[,c(7,1:6,8:length(plot_dat))]


#full leaflet map percentage change
pal <- colorBin(
  palette = "YlOrRd",
  na.color="#d0cccf",
  bins = c(-100,-50,-0.00000000000001,0.00000000000001,50,100,Inf)
)
plot_dat_t <- st_transform(plot_dat, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

leaflet(data= plot_dat_t) %>%
  setView(0, 0,zoom=1) %>%
  
  
  addPolygons(color=pal(plot_dat_t$change)
              ,fillOpacity = 1
              ,stroke=F
              ,smoothFactor=0.2
              ,popup=paste0(
               sub(paste0("<b>Country</b>: <br/>"),"",paste0("<b>Country</b>: ",plot_dat_t$map_name,"<br/>")),
                sub(paste0("<b>Birth Registration NA: </b>NA%<br/>"),"",paste0("<b>Birth Registration ", plot_dat_t$min,": </b>",round(plot_dat_t$`old`),"%","<br/>")),
               sub(paste0("<b>Birth Registration NA: </b>NA%<br/>"),"",paste0("<b>Birth Registration ", plot_dat_t$max,": </b>",round(plot_dat_t$`new`),"%","<br/>")),
                sub(paste0("<b>Change: </b>NA%<br/>"),"",paste0("<b>Change: </b>",round(plot_dat_t$change),"%","<br/>"))
              )) %>%
  
  addPolylines(
    color="#eeeeee",
    weight=0.5,
    opacity=1,
    smoothFactor=0.2)%>%
  
  addLegend(
    "bottomright"
    , pal=pal
    , values = plot_dat$change
    , opacity = 1
    , title="Percentage change in BR within P20 <br/> (2000-2015 or closest data available)"
   ,labFormat = labelFormat(suffix="%")
    ,na.label = "No Data"
  ) %>%
  # addTiles(attribution = "Development Initiatives") %>%
  removeLayersControl()

####P20 Raw data map####
#uses data from above
pal <- colorBin(
  palette = "YlOrRd",
  na.color="#d0cccf",
  bins = c(0,20,40,60,80,100)
)
plot_dat_t <- st_transform(plot_dat, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

leaflet(data= plot_dat_t) %>%
  setView(0, 0,zoom=1) %>%
  
  
  addPolygons(color=pal(plot_dat_t$new)
              ,fillOpacity = 1
              ,stroke=F
              ,smoothFactor=0.2
              ,popup=paste0(
                sub(paste0("<b>Country</b>: <br/>"),"",paste0("<b>Country</b>: ",plot_dat_t$map_name,"<br/>")),
                sub(paste0("<b>Birth Registration NA: </b>NA%<br/>"),"",paste0("<b>Birth Registration ", plot_dat_t$max,": </b>",round(plot_dat_t$`new`),"%","<br/>"))
              )) %>%
  
  addPolylines(
    color="#eeeeee",
    weight=0.5,
    opacity=1,
    smoothFactor=0.2)%>%
  
  addLegend(
    "bottomright"
    , pal=pal
    , values = plot_dat$change
    , opacity = 1
    , title="P20 birth registration (most recent data year)"
    ,labFormat = labelFormat(suffix="%")
    ,na.label = "No Data"
  ) %>%
  # addTiles(attribution = "Development Initiatives") %>%
  removeLayersControl()



####Using 2000 and 2015 years, if either is NA then sohws as NA####
plot_dat <- World
plot_dat <- plot_dat[plot_dat$continent != "Antarctica",]
#plot_dat <- plot_dat[plot_dat$continent == "Africa",] #for Africa only map
plot_dat <- plot_dat[,c(1,16)]
names(plot_dat)[1] <- "iso3"
dat <- fread("C:/git/MICS_recode/output/dhs.mics BR global p20.csv",na.string="NA") #change based on chosen data
dat <- dat[,c("iso3","povcal_year","P20")]
dat <- dcast(dat, iso3~povcal_year, value.var="P20")
setDT(dat)
dat[,change:=(`2015`-`2000`)/`2000`, by=iso3]
#dat[,change:=(`2018`-`2000`), by=iso3]
dat<- dat[,.(iso3,`2000`,`2015`,change)]
dat[,2:length(dat)] <- dat[,2:length(dat)]*100
plot_dat <- left_join(plot_dat,dat,by="iso3")
plot_dat <- left_join(plot_dat,names, by="iso3")

plot_dat <- plot_dat[,c(5,1:4,6:length(plot_dat))]
#pallette
pal <- colorBin(
  palette = "YlOrRd",
  na.color="#d0cccf",
  bins = c(-100,-50,-0.00000000000001,0.00000000000001,50,100,Inf)
)


#full leaflet map
plot_dat_t <- st_transform(plot_dat, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

leaflet(data= plot_dat_t) %>%
  setView(0, 0,zoom=1) %>%
  
  
  addPolygons(color=pal(plot_dat_t$change)
              ,fillOpacity = 1
              ,stroke=F
              ,smoothFactor=0.2
              ,popup=paste0(
                sub(paste0("<b>Country</b>: <br/>"),"",paste0("<b>Country</b>: ",plot_dat_t$map_name,"<br/>")),
                sub(paste0("<b>Birth Registration 2000: </b>NA%<br/>"),"",paste0("<b>Birth Registration 2000: </b>",round(plot_dat_t$`2000`),"%","<br/>")),
                sub(paste0("<b>Birth Registration 2015: </b>NA%<br/>"),"",paste0("<b>Birth Registration 2015: </b>",round(plot_dat_t$`2015`),"%","<br/>")),
                sub(paste0("<b>Change: </b>NA%<br/>"),"",paste0("<b>Change: </b>",round(plot_dat_t$change),"%","<br/>"))
              )) %>%
  
  addPolylines(
    color="#eeeeee",
    weight=0.5,
    opacity=1,
    smoothFactor=0.2)%>%
  
  addLegend(
    "bottomright"
    , pal=pal
    , values = plot_dat$change
    , opacity = 1
    , title="Change in BR for P20 2000-2015"
     ,labFormat = labelFormat(suffix="%")
    ,na.label = "No Data"
  ) %>%
  # addTiles(attribution = "Development Initiatives") %>%
  removeLayersControl()



