##### Defining global objects####
# source functions
source("2_load_libraries.R")
boundaries=readShapeSpatial("data/boundaries3.shp")
centroid=readShapeSpatial("data/centroid.shp")
lable=centroid$ID %>% as.character()
labelname=rev(seq(31,47)) %>% paste0(.,"°N")
daily=read.csv("data/UI_data_daily.csv")%>% mutate(date=as.Date(paste(year,month,day,sep="-")))
weekly=read.csv("data/UI_data_weekly.csv")%>% mutate(date=as.Date(paste(year,month,day,sep="-")))
monthly=read.csv("data/UI_data_monthly.csv")%>% mutate(date=as.Date(paste(year,month,day,sep="-")))
yearly=read.csv("data/UI_data_yearly.csv")%>% mutate(date=as.Date(paste(year,month,day,sep="-")))
latitudes=data.frame(zone=seq(1,17),lats=rev(seq(31,47)))
smooth=c("daily","weekly","monthly","yearly")


ui <- dashboardPage(skin="black",
                    dashboardHeader(
                      title = "West Coast Upwelling Indices",
                      titleWidth = 300
                    ),
                    dashboardSidebar(
                      width = 300,
                      sidebarMenu(id = 'sidebarmenu',
                      selectInput("smoother","Select averaging window",smooth,width = "100%"),
                      sliderInput("slider1","Select year range",min=as.Date("1980","%Y"),max=as.Date("2016","%Y"),value=c(as.Date("1980","%Y"),as.Date("2016","%Y")),timeFormat = "%Y",width = "100%"),
                      div(style="text-align:center",downloadButton("downloadData", label = h6(style="color:black","Download")))
                      )),
                    
   dashboardBody(
     fluidRow(
             column(h5("Click on a map pin to see its index timeseries"),width=4,leafletOutput("map",height = 800)),
             column(h1(" "),width = 8,plotOutput("CUTI")),
             column(h5(" "),width = 8,plotOutput("BEUTI"))
)
   ))


server <- shinyServer(function(input, output) {

  data=reactive({
    if(input$smoother=="daily"){data=daily}
    if(input$smoother=="weekly"){data=weekly}
    if(input$smoother=="monthly"){data=monthly}
    if(input$smoother=="yearly"){data=yearly}
     data=data %>% filter(date>=as.Date(input$slider1[1],format="%Y") & date<=as.Date(input$slider1[2],format="%Y"))
    return(data)
  })
    
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("West_Coast_Upwelling_Indices_",input$smoother, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    })
  
  output$map <- renderLeaflet({
    lmap <- leaflet()
    lmap <- addProviderTiles(lmap, "CartoDB.Positron",options = providerTileOptions(noWrap = TRUE))
    lmap <- addPolylines(lmap,data=boundaries,color="black",weight=1.5)
    lmap <- addCircleMarkers(lmap,data=centroid,label = labelname,layerId=lable,radius = 1.5,color = "black",opacity = 1,fillColor = "black",fillOpacity=1,labelOptions = labelOptions(noHide = T,textOnly = T,textsize = 8,direction = "left"))
    })
  

  observeEvent(input$map_marker_click,{
    
    click=input$map_marker_click
    id=click$id
    a=latitudes[id,2] %>% as.character()
    lat=click$lat
    lon=click$lng
    
    # if(length(id)>0){leafletProxy("map") %>% removeMarker(layerID="newMarker")}
    leafletProxy("map") %>% addCircleMarkers(lng=lon,lat=lat,color="red",layerId="newMarker",group="circles",opacity = 1,fillColor = "red",fillOpacity=1,radius=4)
    
    indexCUTI=NULL
    indexCUTI=paste0("^CUTI",input$map_marker_click$id,"$")
    indexCUTI=grep(indexCUTI,names(daily))
    indexBEUTI=NULL
    indexBEUTI=paste0("^BEUTI",input$map_marker_click$id,"$")
    indexBEUTI=grep(indexBEUTI,names(daily))

  output$CUTI <- renderPlot({
    plot=ggplot()+geom_line(data=data(),aes(x=date,y=data()[,indexCUTI]))
    plot=plot+ggtitle(paste0("Coastal Upwelling Transport Index, ",a,"°N: ",input$smoother))+labs(x="Year")+  labs(y=expression(paste(m^2,s^-1)))+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(axis.text = element_text(size=10),axis.title = element_text(size=10),plot.title = element_text(size=10))
    plot=plot+theme(legend.background = element_blank())+theme(legend.text=element_text(size=10))+ theme(legend.key=element_blank()) +theme(legend.position="none")+scale_y_continuous(expand = c(0, 0))+scale_x_date(date_breaks="year",date_labels = "%y",date_minor_breaks = "months",expand = c(0,0))
    plot2=plot
    plot2
  })

  output$BEUTI <- renderPlot({
    plot=ggplot()+geom_line(data=data(),aes(x=date,y=data()[,indexBEUTI]))
    plot=plot+ggtitle(paste0("Biologically Effective Upwelling Transport Index, ",a,"°N: ",input$smoother))+labs(x="Year")+labs(y=expression(paste(mmol,phantom(x),s^-1,m^-1)))+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(axis.text = element_text(size=10),axis.title = element_text(size=10),plot.title = element_text(size=10))
    plot=plot+theme(legend.background = element_blank())+theme(legend.text=element_text(size=10))+ theme(legend.key=element_blank()) +theme(legend.position="none")+scale_y_continuous(expand = c(0, 0))+scale_x_date(date_breaks="year",date_labels = "%y",date_minor_breaks = "months",expand = c(0,0))
    plot2=plot
    plot2
  })
  
  })
  
})


shinyApp(ui = ui, server = server)