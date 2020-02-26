required.packages <- c("dplyr","ggplot2","data.table","tmap","sf","leaflet")
lapply(required.packages, require, character.only=T)

####most recent data year####
#using national data from Birth reg aggregation.R
years <-read.csv("C:/git/MICS_recode/output/dhs.mics BR national (survey year).csv")
years <- unique(years[,1:3])
years$new <- gsub("dhs|mics|\\.5","",years$survey_year)
years$new <- str_trim(years$new,side="both")
right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

left = function(text, num_char) {
  substr(text, 1, num_char)
}

years$right <- right(years$new,4)
years$left <- left(years$new,4)
setDT(years)
years[left>right,latest:=left]
years[left<right,latest:=right]
years[left==right,latest:=left]
years <- years[,c(1:2,7)]


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
dat <- dat[dat$povcal_year=="2015",]

plot_dat <- left_join(plot_dat,dat[,c(1,3)],by="iso3")
plot_dat <- left_join(plot_dat,names, by="iso3")

extra_dat <- read.csv("C:/git/MICS_recode/output/dhs.mics BR national.csv")
extra_dat <- extra_dat[extra_dat$povcal_year=="2015",]

plot_dat <- left_join(plot_dat,extra_dat[,-2], by=c("iso3"))


plot_dat$value <- plot_dat$value*100
plot_dat$P20 <- plot_dat$P20*100
plot_dat$Not.P20 <- plot_dat$Not.P20*100
plot_dat$Male <- plot_dat$Male*100
plot_dat$Rural <- plot_dat$Rural*100
plot_dat$Urban <- plot_dat$Urban*100

years_2015 <- years[povcal_year==2015,]
plot_dat <- left_join(plot_dat,years_2015[,c(1,3)],by=c("iso3"))
plot_dat <- plot_dat[,c(3,1,2,4:length(plot_dat))]



####map####
pal <- colorBin(
  palette = "YlOrRd",
  na.color="#d0cccf",
  bins = c(0,20,40,60,80,100)
)
# ####tmap and leaflet####
# tmap_leaflet(tm_shape(plot_dat)+
#                tm_polygons("value",palette = "YlOrRd",
#                            legend.show=FALSE,
#                            popup.vars=c("Registration (Latest Data)"="value","P20 (Latest Data)"="P20", "Non-P20 (Latest Data)"="Not.P20", "Data Year"="latest"),
#                            popup.format=list(digits=1,width=30,suffix="%")
#                            )+
#            #    tm_text("latest")+
#          
#                            
#              tm_basemap(NULL)
#              ) %>%
#   setView(0, 0,zoom=1) %>%
#   addLegend(
#     "bottomright"
#     , pal=pal
#     , values = plot_dat$Male
#     , opacity = 1
#     , title="Reg (Latest Data)"
#     ,labFormat = labelFormat(suffix="%")
#     ,na.label = "No Data"
#   ) %>%
# removeLayersControl()# %>%
#  # addTiles(attribution = "Development Initiatives")
# 
# ####static####
# tmap_mode(mode="plot")
# tm_shape(plot_dat)+
#   tm_polygons("value",palette = "YlOrRd",legend.show=FALSE)+
#   tm_basemap(NULL)+
#   tm_layout(frame=FALSE)
  


#####full leaflet map####
plot_dat_t <- st_transform(plot_dat, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

leaflet(data= plot_dat_t) %>%
  setView(0, 0,zoom=1) %>%
  
  
  addPolygons(color=pal(plot_dat_t$value)
              ,fillOpacity = 1
              ,stroke=F
              ,smoothFactor=0.2
              ,popup=paste0(
                sub(paste0("<b>Year of data</b>: NA<br/>"),paste0("<center><b>No data</b></center>"),paste0("<b>Year of data</b>: ",plot_dat_t$latest,"<br/>")),
                sub(paste0("<b>Country</b>: <br/>"),"",paste0("<b>Country</b>: ",plot_dat_t$map_name,"<br/>")),
                sub(paste0("<b>Birth Registration: </b>NA%<br/>"),"",paste0("<b>Birth Registration: </b>",round(plot_dat_t$value),"%","<br/>")),
                sub(paste0("<b>P20: </b>NA%<br/>"),"",paste0("<b>P20: </b>",round(plot_dat_t$P20),"%","<br/>")),
                sub(paste0("<b>Not P20: </b>NA%<br/>"),"",paste0("<b>Not P20: </b>",round(plot_dat_t$Not.P20),"%","<br/>")),
                sub(paste0("<b>Rural: </b>NA%<br/>"),"",paste0("<b>Rural: </b>",round(plot_dat_t$Rural),"%","<br/>")),
                sub(paste0("<b>Urban: </b>NA%<br/>"),"",paste0("<b>Urban: </b>",round(plot_dat_t$Urban),"%","<br/>"))
                
              )) %>%
  
  addPolylines(
    color="#eeeeee",
    weight=0.5,
    opacity=1,
    smoothFactor=0.2)%>%
  
  addLegend(
    "bottomright"
    , pal=pal
    , values = plot_dat$Male
    , opacity = 1
    , title="Registration (Latest Data)"
    ,labFormat = labelFormat(suffix="%")
    ,na.label = "No Data"
  ) %>%
# addTiles(attribution = "Development Initiatives") %>%
  removeLayersControl()

