ggplot(data=deflator,aes(x=country,y=2000))+
  geom_bar(stat="identity")


add_figure(
  data=deflator,
  logo_path="C:/git/covid-tracking/input/DI logo pink.png",
  figure_number="1",
  report_name ="Tracking aid flows to the Covid-19 response",
  chapter="",
  title = "Commitments from bilateral donors",
  source = "Development Iniatives based on IATI",
  notes= "",
  long_description = "this is the long",
  geographical_info ="Global",
  author = "Dean Breed and Lewis Sternberg"
)

add_figure <- function(data,logo_path,figure_number,report_name,chapter,title,messaging,source,notes,long_description,geographical_info,author,graph_dir){
  figure <- paste0("Figure ",figure_number)
  #add DI logo and set cell size
  openxlsx::addWorksheet(excel_output,figure)
  img <-logo_path
  openxlsx::insertImage(excel_output, figure, img, startRow = 1, startCol = 1, width = 7.45, height = 1.4, units="cm")
  setColWidths(excel_output,figure,cols=c(1,2),widths=c(33,47.29))
  setRowHeights(excel_output,figure,rows=1,heights=51)
  
  #add metadata
  openxlsx::writeData(excel_output,figure,report_name, startCol=1,startRow=2,rowNames=F)
  openxlsx::writeData(excel_output,figure,chapter, startCol=1,startRow=3,rowNames=F)
  openxlsx::writeData(excel_output,figure,figure, startCol=1,startRow=4,rowNames=F)
  
  meta <- cbind(
    c("Title","Messaging title","Source:","Notes:","Long description:","Geographical information:","Author:"),
    c(title,messaging,source,notes,long_description,geographical_info,author)
                )
  openxlsx::writeData(excel_output,figure,meta, startCol=1,startRow=5,rowNames=F,colNames=F)
  
  #add data
  openxlsx::writeData(excel_output,figure,data, startCol=1,startRow=14,rowNames=F)
  
  #add plot - saves most recnet plot
  graph_output <- graph_dir
  openxlsx::insertImage(excel_output,figure,graph_output,width=6,height=3,,startCol=ncol(data)+2,startRow=14)
}

create_excel <- function(){
  if(exists("excel_output")){rm(excel_output)}
  excel_output <- openxlsx::createWorkbook()
  openxlsx::modifyBaseFont(excel_output, fontSize = 11, fontColour = "black", fontName = "arial")
}

save_excel <- function(path){
  openxlsx::saveWorkbook(excel_output,path,overwrite=T)
}


if()