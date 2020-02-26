required.packages <- c("dplyr","ggplot2","data.table","tmap","sf","leaflet")
lapply(required.packages, require, character.only=T)


####data####
names <- read.csv("C:/map_name.csv")
names(names)[1] <-"iso3"
names <- unique(names)

data("World")

plot_dat <- World
plot_dat <- plot_dat[plot_dat$continent != "Antarctica",]
#plot_dat <- plot_dat[plot_dat$continent == "Africa",] #for Africa only map
plot_dat <- plot_dat[,c(1,16)]
names(plot_dat)[1] <- "iso3"
dat <- fread("C:/git/MICS_recode/output/dhs.mics BR national_only (one col).csv",na.string="NA") #change based on chosen data
dat <- dcast(dat, iso3~povcal_year, value.var="value")
setDT(dat)
dat[,change:=(`2018`-`2000`)/`2000`, by=iso3]
#dat[,change:=(`2018`-`2000`), by=iso3]
dat<- dat[,.(iso3,`2000`,`2018`,change)]
dat[,2:length(dat)] <- dat[,2:length(dat)]*100
plot_dat <- left_join(plot_dat,dat,by="iso3")
plot_dat <- left_join(plot_dat,names, by="iso3")

plot_dat <- plot_dat[,c(5,1:4,6:length(plot_dat))]

####pallette####
pal <- colorBin(
  palette = "YlOrRd",
  na.color="#d0cccf",
  bins = c(-100,-50,-0.00000000000001,0.00000000000001,50,100,Inf)
)


#####full leaflet map####
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
                sub(paste0("<b>Birth Registration 2018: </b>NA%<br/>"),"",paste0("<b>Birth Registration 2018: </b>",round(plot_dat_t$`2018`),"%","<br/>")),
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
    , title="Change 2000-2018"
   # ,labFormat = labelFormat(suffix="%")
    ,na.label = "No Data"
  ) %>%
  # addTiles(attribution = "Development Initiatives") %>%
  removeLayersControl()
