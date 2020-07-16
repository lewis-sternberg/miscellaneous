#### Bangaldesh - Nexus ####
#Taken from Dean Breed's Cameroon Nexus work
list.of.packages <- c("rgdal","leaflet","data.table","ggplot2","tmap","sf","tmaptools","countrycode","openxlsx","grDevices","reshape2","anytime")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

memory.limit(size=36000)

if(Sys.info()[["user"]]=="lewiss"){
  prefix <- "C:" 
}else{
  prefix <- "~"
}
wd = paste0(prefix,"/git/iati-geo")
setwd(wd)

red_low <- "#f8c1b2"
red_mid <- "#f0826d"
red_high <- "#bc2629"
red_highest <-"#8f1b13"
disb = c("E", "D", "3", "4")

# Read in IATI data

agg <- read.csv("output/iati_unfiltered_agg.csv")
agg = subset(agg, secondary_reporter %in% c("0","false"))
agg = subset(agg, usd_disbursement > 0)
agg = subset(agg,budget_or_transaction=="Transaction")
agg$transaction_date = anydate(agg$transaction_date)
agg$month = month(agg$transaction_date)
agg$year = year(agg$transaction_date)
agg <- subset(agg,(transaction_type %in% disb) & (year>=2015) &(year<=2020))

country="Bangladesh"

iso2=countrycode(country,"country.name","iso2c")
iso3=countrycode(country,"country.name","iso3c")

agg=data.frame(subset(agg,agg$recipient_country_code==iso2))
donors <- c("gac-amc","worldbank","asdb","unicef"
            #,"unhcr"
            ,"undp","ec-devco","bmz")
all.entries<-data.table()
for(donor in donors){
  message(donor)
  agg.donor <- subset(agg,publisher==donor)
  # Separate data into old-style coordinates, new-style coordinates and no coordinates
  
  #location_names = subset(agg,is.na(location_coordinates_lat) & is.na(location_point_pos))
  #location_names = subset(agg.donor,is.na(location_coordinates_lat) & is.na(location_point_pos))
  # v1_points = subset(agg,location_coordinates_lat!="")
  # v1_points$lat=v1_points$location_coordinates_lat
  # v1_points$long=v1_points$location_coordinates_long
  
  #v2_points = subset(agg,location_point_pos!="")
  v2_points = subset(agg.donor,location_point_pos!="")
  split_points = colsplit(v2_points$location_point_pos, " ", c("lat", "long"))
  v2_points$lat = as.numeric(split_points$lat)
  v2_points$long = as.numeric(split_points$long)
  v2_points = subset(v2_points,!is.na(lat) & !is.na(long))
  v2_points = subset(v2_points,abs(lat)<=80 & abs(long)<=180)
  
  all_transactions <- v2_points
  all_transactions <- data.table(all_transactions)
  #all_transactions <- data.table(subset(all_transactions,humanitarian_scope %in% c("",0,"false")))
  all_transactions[, `:=`(number_rows_per_trans = .N) , by = .(iati_identifier,usd_disbursement,transaction_date,month,year,sector_code,activity_scope,humanitarian_scope)]
  all_transactions$usd_disbursement <- all_transactions$usd_disbursement/all_transactions$number_rows_per_trans
  all_transactions[,c("number_rows_per_trans")]=NULL
  
  all_transactions.dev=all_transactions
  
  hold.data=data.frame()
  for(level in c("1","2")){
    message(level)
    for (this.year in c(2015:2019)){
      message(this.year)
      country="Bangladesh"
      all_transactions.year <- subset(all_transactions.dev,all_transactions.dev$year == this.year)
      #all_transactions.year <- all_transactions.dev
      
      iso2=countrycode(country,"country.name","iso2c")
      iso3=countrycode(country,"country.name","iso3c")
      regions <- rgdal::readOGR(dsn = paste0("output/country-analysis/maps/",country,"/gadm36_",iso3,"_",level,".shp"),layer = paste0("gadm36_",iso3,"_",level))
      regions@data[[paste0("NAME_",level)]] <- iconv(regions@data[[paste0("NAME_",level)]], "UTF8", "ASCII//TRANSLIT")
      
      
      # Extract points from IATI dataset for transaction activities - overall
      
      entries=data.frame(subset(all_transactions.year,(all_transactions.year$recipient_country_code==iso2)&!(is.na(all_transactions.year$long))))
      coordinates(entries)<- ~ long + lat
      proj4string(entries) <- proj4string(regions)
      entries=data.table(cbind(as.data.frame(entries),over(entries,regions)))[,.(total_spent=sum(usd_disbursement, na.rm=TRUE)),by=.(get(paste0("NAME_",level)))]
     # entries=data.table(cbind(as.data.frame(entries),over(entries,regions)))[,.(total_spent=sum(usd_disbursement, na.rm=TRUE)),by=.(NAME_1)]
      names(entries)=c("region","total_spent")
      entries=subset(entries,!is.na(entries$region))
      dat <- entries
      dat$year <- this.year
      dat$donor <- donor
      dat$level <- level
      dat[level=="1"]$level <- "Region"
      dat[level=="2"]$level <- "District"
      all.entries <- rbind(all.entries,dat)
      
      
      # Read in populations
      
      # population=read.csv(file="output/populations/Subnational-Population_csv/Subnational-PopulationData.csv",na.strings="",as.is=T)
      # population=population[which(substr(population$Country.Code,1,3)==iso3),]
      # population=subset(population,population$Indicator.Name=="Population, total")
      # population$region=substr(population$"ï..Country.Name",nchar(country)+3,nchar(population$"ï..Country.Name"))
      # population=population[which(population$region!=""),]
      # population=population[,c("region","X2016")]
      # names(population)=c("region","population")
      # entries=merge(population,entries, by="region")
      # entries$total_spent_per_cap=round(entries$total_spent/entries$population,2)
      
      
      # Set color palettes
      palette_CA <- c(red_low,red_mid,red_high,red_highest)
      
      colnames(regions@data)[which(names(regions@data) == paste0("NAME_",level))] <- "region"
      merged <- merge(regions,entries,by = "region")
      merged$total_spent_usd.mn <- merged$total_spent/1000000
      merged$total_spent_percentage <- 100*merged$total_spent/sum(merged$total_spent)
      merged$year.of.data = this.year
      #hold.data=rbind(merged@data,hold.data)
      
      # Graphs
      if(level==1){
        breaks <- c(250,500,750,1000)
      }else{
        breaks <- c(0,15,30,60,120)
      }
      spend <- tmap::tm_shape(merged) + 
        tmap::tm_polygons(col = "total_spent_usd.mn",breaks = breaks ,palette = palette_CA,
                          title = paste0("Total spend (USD millions) by region in ",country)) +
        tmap::tm_layout(  legend.text.size = 1,
                          legend.bg.alpha = 1,
                          legend.outside = T,
                          legend.outside.position = "left",
                          frame = F)
      spend
      #tmap::tmap_save(spend,paste0("output/country-analysis/maps/",country,"/output/cam1_",this.year,".png"))
      #tmap::tmap_save(spend,paste0("output/country-analysis/maps/",country,"/output/cam1_",this.year,".svg"))
      tmap::tmap_save(spend,paste0("output/country-analysis/maps/",country,"/output/BDG",level,"_",donor,"_",this.year,".png"))
    }
  }
  cat("\n")
}
fwrite(all.entries,file=paste0("output/country-analysis/maps/",country,"/output/all_data.csv"))

# spend_per_cap <- tmap::tm_shape(merged) + 
#   tmap::tm_polygons(col = "total_spent_per_cap",breaks=c(0,20,50,100,200),palette = palette_CA,
#                     title = paste0("Total spend per capita (USD) by region in ",country)) +
#   tmap::tm_layout(  legend.text.size = 1,
#                     legend.bg.alpha = 1,
#                     legend.outside = T,
#                     legend.outside.position = "left",
#                     frame = F)
# spend_per_cap
# tmap::tmap_save(spend_per_cap,paste0("output/country-analysis/maps/",country,"/output/cam2_",this.year,".png"))
# tmap::tmap_save(spend_per_cap,paste0("output/country-analysis/maps/",country,"/output/cam2_",this.year,".svg"))
# 
# spend_per_cap_percent <- tmap::tm_shape(merged) + 
#   tmap::tm_polygons(col = "total_spent_percentage",breaks = c(0,5,10,50,100),palette = palette_CA,
#                     title = paste0("Total proportional spend by region in ",country)) +
#   tmap::tm_layout(  legend.text.size = 1,
#                     legend.bg.alpha = 1,
#                     legend.outside = T,
#                     legend.outside.position = "left",
#                     frame = F)
# spend_per_cap_percent
# tmap::tmap_save(spend_per_cap_percent,paste0("output/country-analysis/maps/",country,"/output/cam3_",this.year,".png"))
# tmap::tmap_save(spend_per_cap_percent,paste0("output/country-analysis/maps/",country,"/output/cam3_",this.year,".svg"))
# #}
# write.csv(hold.data,"output/merged.csv")
