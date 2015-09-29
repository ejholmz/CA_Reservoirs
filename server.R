library(shiny)
# library(leaflet)
library(scales)
library(dplyr)
library(foreign)
library(ggplot2)
library(htmlwidgets)

shinyServer(function(input, output) {
  
  ## Interactive Map ##
  
  # Create the map
  mymap = leaflet() %>%  
    addTiles(
      urlTemplate = "//{s}.tiles.mapbox.com/v3/eholmz.h3ldj3nf/{z}/{x}/{y}.png",
      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    ) %>%
    setView(-120.8, 38.2, 6)
  
  output$map <- renderLeaflet(mymap)
  
  # Map symbology
  observe({    
    
#     radius <- sqrt(cares$max) / max(sqrt(cares$max)) * 30000
#     radius2 <- sqrt(cares$storage) / max(sqrt(cares$max)) * 30000
    radius <- sqrt(cares$max/pi) * 750
    radius2 <- sqrt(cares$storage/pi) * 750
    
    leafletProxy("map", data = cares) %>%
      clearShapes() %>%
      
      addCircles(~Longitude, ~Latitude, radius=radius2, layerId=~Reservoir,
                 stroke=FALSE, fillOpacity=0.3, fillColor="blue") %>%
      addCircles(~Longitude, ~Latitude, radius=radius, layerId=~code,
                 stroke=FALSE, fillOpacity=0.2, fillColor="red")
    
  })
  
  ##Reactive expression that subsets data within map bounds
  resInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(cares[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(cares,
           Latitude >= latRng[1] & Latitude <= latRng[2] &
             Longitude >= lngRng[1] & Longitude <= lngRng[2])
  })
  
  output$Histogram <- renderPlot({
    if (nrow(resInBounds()) == 0)
      return(NULL)
    
    hist(resInBounds()$perc, main = "Histogram of % storage capacity", 
         xlab = "Percent of Capacity", ylab = "Count", xlim = c(0,100),  breaks = seq(0,100,10),
         col = "skyblue")
  })

  output$Hist <- renderPlot({
    if (nrow(resInBounds()) == 0)
      return(NULL)
    
    par(oma = c(0,0,0,2))
    barplot(sum(resInBounds()$max),ylim = c(0, sum(resInBounds()$max)), ylab = "Storage (1000 Acre-feet)")
    par(new = T)
    barplot(sum(resInBounds()$storage), ylim = c(0, sum(resInBounds()$max)), col = "blue", yaxt = "n")
    abline(h = sum(resInBounds()$avg.daily), col = "red", lwd = 4)
    axis(side = 4, at = seq(0,1,.2)*sum(resInBounds()$max), labels = c("0%","20%","40%","60%","80%", "100%"))
    text(x = .45,y = sum(resInBounds()$avg.daily) + .05*sum(resInBounds()$max), col = "red", cex = 1.5,
         labels = paste(signif(sum(resInBounds()$avg.daily)/sum(resInBounds()$max)*100, 3), "%",sep = ""))
    text(x = .95,y = sum(resInBounds()$storage) + .05*sum(resInBounds()$max), col = "blue", cex = 1.5,
         labels = paste(signif(sum(resInBounds()$storage)/sum(resInBounds()$max)*100, 3), "%",sep = ""))
    
  })
  
  # Show a popup at the given location
  showResPopup <- function(code, lat, lng) {
    selectedRes <- cares[cares$code == code,]
    content <- as.character(tagList(
      tags$h4("Percent of Capacity:", as.integer(selectedRes$perc)),
      tags$strong(HTML(sprintf("%s, %s",
                               selectedRes$Reservoir, selectedRes$code 
      ))), tags$br(),
      sprintf("Storage: %s%s", format(selectedRes$storage,big.mark=",",scientific=FALSE), " Acre Feet"), tags$br(),
      sprintf("Capacity: %s%s", format(selectedRes$max,big.mark=",",scientific=FALSE), " Acre Feet"), tags$br(),
      sprintf("As of Date: %s",selectedRes$stor_date), tags$br(),
      sprintf("Elevation: %s%s",selectedRes$Elevation, " ft")
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = code)
  }
  
  # When map is clicked, show a popup with Reservoir info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showResPopup(event$id, event$lat, event$lng)
    })
  })
  
  ## Time Series Graphs ##
  
  output$Res <- renderPlot({p <- ggplot(resdata[resdata$Reservoir == input$selectRes,], aes(x=stor_date,y=storage)) + theme_bw() + 
                              theme(legend.direction= "horizontal", legend.position=c(.5,0.1), legend.key = element_blank(),
                                    legend.background = element_rect(colour = "black"), legend.text=element_text(size=12), axis.text.x = element_text(angle=45, hjust = .85)) + 
                              geom_line(aes(x=stor_date,y=avg.daily),alpha=.3, linetype="dashed") +
                              geom_area(fill="blue",alpha=.15) + 
                              geom_line(color = "navy",size=.75) + 
                              geom_hline(aes(yintercept = max), color= "red") +
                              scale_x_date(breaks = date_breaks(ifelse(as.numeric(difftime(input$range[2],input$range[1],units = "weeks")) > 76,"1 years","1 months")),
                                           labels=date_format(ifelse(as.numeric(difftime(input$range[2],input$range[1],units = "weeks")) > 76,"%Y","%b-%Y")),limits = input$range) +
                              labs(list(title=paste("Reservoir Storage at",input$selectRes), y= "Reservoir Storage (1000 Acre-Feet)", x = "Date"))
                            
                            if(input$check == TRUE){
                              if(input$level == TRUE){return(p + geom_vline(aes_string(xintercept = as.numeric(input$vline))) +
                                                               geom_hline(aes_string(yintercept = resdata[resdata$Reservoir == input$selectRes & resdata$stor_date == input$vline,"storage"])))}
                              else{return(p + geom_vline(aes_string(xintercept = as.numeric(input$vline))))}
                            }
                            
                            else{return(p)}
                            
  })
  
  output$Current <- renderText(paste("Data for", input$selectRes, "available up to", max(resdata[resdata$Reservoir == input$selectRes,"stor_date"])))
  
  output$vlinetext <- renderText({
    if(input$check == TRUE){return(paste("Reservoir storage at", input$selectRes, "on", input$vline, "was", 
                                         resdata[resdata$Reservoir == input$selectRes & resdata$stor_date == input$vline,"storage"][1], 
                                         "thousand acre feet"))}
    else{return(NULL)}
  })
  
})
