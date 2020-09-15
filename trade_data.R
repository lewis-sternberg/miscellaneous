get.Comtrade <- function(url="http://comtrade.un.org/api/get?"
                         ,maxrec=10000
                         ,type="C"
                         ,freq="A"
                         ,px="HS"
                         ,ps="now"
                         ,r
                         ,p
                         ,rg="all"
                         ,cc="TOTAL"
                         ,fmt="json"
)
{
  string<- paste(url
                 ,"max=",maxrec,"&" #maximum no. of records returned
                 ,"type=",type,"&" #type of trade (c=commodities)
                 ,"freq=",freq,"&" #frequency
                 ,"px=",px,"&" #classification
                 ,"ps=",ps,"&" #time period
                 ,"r=",r,"&" #reporting area
                 ,"p=",p,"&" #partner country
                 ,"rg=",rg,"&" #trade flow
                 ,"cc=",cc,"&" #classification code
                 ,"fmt=",fmt        #Format
                 ,sep = ""
  )

  if(fmt == "csv") {
    raw.data<- read.csv(string,header=TRUE)
    return(list(validation=NULL, data=raw.data))
  } else {
    if(fmt == "json" ) {
      raw.data<- try(rjson::fromJSON(file=string),silent=T)
      if(raw.data=="Error in file(con, \"r\") : cannot open the connection\n"){
        return("Error, wait 1 hour")
      }else{
      data<- raw.data$dataset
      validation<- unlist(raw.data$validation, recursive=TRUE)
      ndata<- NULL
      if(length(data)> 0) {
        var.names<- names(data[[1]])
        data<- as.data.frame(t( sapply(data,rbind)))
        ndata<- NULL
        for(i in 1:ncol(data)){
          data[sapply(data[,i],is.null),i]<- NA
          ndata<- cbind(ndata, unlist(data[,i]))
        }
        ndata<- as.data.frame(ndata)
        colnames(ndata)<- var.names
      }
      return(list(validation=validation,data =ndata))
    }
  }
}
}

####get list of reporter ID and remove non-country####
dat <- fread(file="C:/git/miscellaneous/reporterAreas.json",fill=T,quote="",header=F)
dat[grepl("\\}|\\{|\\]|\\[|more",dat$V1)] <- NA
dat <- dat[complete.cases(dat)]
id <- dat[grepl("\"id\"",dat$V1)]
name <- dat[grepl("\"text\"",dat$V1)]
reporters <- cbind(id,name)
names(reporters) <- c("id","name")
reporters$id <- gsub("[[:punct:]]|id","",reporters$id)
reporters$name <- gsub("[[:punct:]]|text","",reporters$name)
reporters$id <- trimws(reporters$id,"both")
reporters$name <- trimws(reporters$name,"both")
not_needed <- c("EU28","Fmr","All","before","So African Customs Union","Other")
reporters <- reporters[!grepl(paste0(not_needed,collapse="|"),reporters$name)]


####run loop for all countries and years in groups of 3####
years <- c("2018")
comtrade <- data.table()
for(reporter in reporters$id){
  message(paste0("reporter: ",reporter))
  for(year in years){
    message(paste0("    years: ",year))
    Sys.sleep(1) #allowing 1 second gap between query, so not to get error
    tmp <- get.Comtrade(r=reporter, freq="A", ps=year, rg="2", fmt="json",type="C",p="all",px="S4",cc="AG2")
    if(tmp!="Error, wait 1 hour"){
      comtrade <- rbind(comtrade,tmp$data,fill=T)
    }else{
      while(tmp=="Error, wait 1 hour"){
        message("Waiting 500 seconds...") #API only allows 100 queries per 1hr, often lets you back in less
        Sys.sleep(500)
        tmp <- get.Comtrade(r=reporter, freq="A", ps=year, rg="2", fmt="json",type="C",p="all",px="S4",cc="AG2")
      }
      comtrade <- rbind(comtrade,tmp$data,fill=T)
    }
    rm(tmp)
  }
}

####data specs####

###top level export data all years###
#years <- c("2010,2011,2012","2013,2014,2015","2016,2017,2018","2019,2020")
#get.Comtrade(r=reporter, freq="A", ps=year, rg="2", fmt="json",type="C",p="all",px="S4",cc="AG1")

###dive in the export types for 2018###
#years <- c("2018")
#get.Comtrade(r=reporter, freq="A", ps=year, rg="2", fmt="json",type="C",p="all",px="S4",cc="AG2")