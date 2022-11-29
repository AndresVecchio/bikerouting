library(lubridate)
library(dplyr)
library(ggplot2)
library(sf)
library(leaflet)
library(sp)
library(tidyverse)
library(RColorBrewer)
library(rgdal)
library(terra)



#Recorridos

#recorridos <- read.csv("trips_2021.csv", stringsAsFactors = FALSE, encoding = "UTF-8")


#conteo <- recorridos %>%
#    filter(nombre_estacion_destino != "" |nombre_estacion_origen != "") %>%
#    filter(nombre_estacion_destino != "nuevo" |nombre_estacion_origen != "nuevo") %>%
#    select(nombre_estacion_origen, nombre_estacion_destino, id_estacion_origen, id_estacion_destino)%>%
#    group_by(nombre_estacion_origen, nombre_estacion_destino)  %>%
#    count() 

#conteosave = conteo %>% 
#    rename(nombreO = nombre_estacion_origen,
#           nombreD = nombre_estacion_destino)%>% 
#    mutate(RUTA = paste(nombreO,"a", nombreD),
#           DESTINO = nombreD)

Estaciones = st_read("CABA_comunas.geojson.txt")


#conteoV2 = left_join(Estaciones, conteosave, by= c("nombre"="nombreD")) 


#st_write(conteoV2, "ViajesTotales22.shp")




#Ruteo


#estacionesFormato2 <- read.csv("nuevas-estaciones-bicicletas-publicas (2).csv", encoding = "UTF-8")

#Estaciones_geo2 = st_as_sf(estacionesFormato2, wkt = "WKT", crs = 4326)

#separated_coord <- Estaciones_geo2 %>%
#    mutate(long = unlist(map(Estaciones_geo2$WKT,1)),
#           lat = unlist(map(Estaciones_geo2$WKT,2)))


#estaciones_cord = separated_coord %>% 
#    st_drop_geometry()

#select_routing = conteo %>% 
#    filter(nombre_estacion_origen == "001 - FACULTAD DE DERECHO")

#Unido = left_join(select_routing, estaciones_cord, by = c("nombre_estacion_origen"="nombre"))

#Unido2 = Unido %>% 
#    mutate(o_lat = lat, o_long = long) %>% 
#    select(nombre_estacion_origen, o_lat, o_long, nombre_estacion_destino, n)

#Unido3 = left_join(Unido2, estaciones_cord, by = c("nombre_estacion_destino"="nombre"))

#Unido4 = Unido3 %>% 
#    mutate(d_lat = lat, d_long = long) %>% 
#    select(nombre_estacion_origen, o_lat, o_long, nombre_estacion_destino, d_lat, d_long, n) %>% 
#    drop_na()



#ruteo_a_facultad <- function(nombre_estacion_origen, o_lat, o_long, nombre_estacion_destino, d_lat, d_long) {
#    
#    ruta <- osrmRoute(src = c( o_lat, o_long),
#                      dst = c( d_lat, d_long), 
#                      returnclass = "sf",
#                      overview = "full",
#                      osrm.profile = "bike")
#    
#   cbind(ORIGEN = nombre_estacion_origen, DESTINO = nombre_estacion_destino, ruta)
    
#}

#library(osrm)


#ruteo_facultad <- list(Unido4$nombre_estacion_origen, 
#                       Unido4$o_long,
#                       Unido4$o_lat,
#                       Unido4$nombre_estacion_destino,
#                       Unido4$d_long,
#                       Unido4$d_lat)

#ruteo_centrico_ejemplo <- pmap(ruteo_facultad, ruteo_a_facultad) %>%
#    reduce(rbind)

#ruteo_centrico_ejemplo <-mutate(ruteo_centrico_ejemplo, RUTA = paste(ORIGEN,"a", DESTINO)) 


#conteo_ruta = mutate(conteo, RUTA = paste(nombre_estacion_origen  ,"a", nombre_estacion_destino)) 



#ruteo_centrico_ejemplo2 = left_join(ruteo_centrico_ejemplo, conteo_ruta)

#st_write(ruteo_centrico_ejemplo2, "ruteo_centrico_ejemplo2.shp")


#ruteo_centrico_ejemplo3 = ruteo_centrico_ejemplo2 %>% 
#    rename(nombreO = nombre_estacion_origen,
#           nombreD = nombre_estacion_destino)
    
    
#st_write(ruteo_centrico_ejemplo3,"ruteo_centrico_ejemplo3.shp")

ruteo_centrico_ejemplo33 = st_read("Ruteo_Completo_Combinado.shp")


ruteo_centrico_ejemplo44 = ruteo_centrico_ejemplo33


ruteo_centrico_ejemplo44 = ruteo_centrico_ejemplo44 %>% 
    mutate(duration = round(ruteo_centrico_ejemplo44$duration, digits = 2),
           distance = round(ruteo_centrico_ejemplo44$distance, digits = 2))



conteoV22 = st_read("ViajesTotales22.shp")

conteoV22 = conteoV22 %>% 
    rename(ORIGEN = nombre)



#Labels



#addCircles(data = Origen ,color = "purple",
#           fillColor = "purple",
#           fillOpacity = 1, radius = 100,
#           group = "Origen") %>% 


#hideGroup("Año de construccion bicisendas") %>% 
#    hideGroup("Ruteo") %>% 
#    hideGroup("Estaciones")%>%


ruteo_centrico_ejemplo44 = ruteo_centrico_ejemplo44 %>% 
    arrange(desc(distance))


Opciones1 = ruteo_centrico_ejemplo44 %>% 
    arrange(ORIGEN)

Opciones = Opciones1$ORIGEN %>%
    unique



#App



# Define UI for application that draws a histogram
ui <- bootstrapPage(tags$style(type = "text/css", "html, body, .leaflet {width:100%; height:100%}"),
                    leafletOutput("map", width = "100%", height = "100%"),
                    # position and properties of the time slider
                    absolutePanel(top = 50, left = 200, draggable = TRUE,
                                  # slider title, step increments, and ticks
                                  selectizeInput("select", "Selecciones Estacion de Origen:", Opciones)))

# shiny server input/output
server <- function(input, output, session) {
    
    filteredData <- reactive({
        ruteo_centrico_ejemplo44 %>%
            filter(ORIGEN == input$select[]) })
        
    filteredData2 = reactive({ conteoV22 %>%
            filter(nombreO == input$select[],
                   DESTINO != input$select[]) })
        
    filteredData3 = reactive({ Estaciones %>%
            filter(nombre == input$select[])  })
        
    addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.8){
            colorAdditions <- paste0(colors,"; border-radius: 50%;; width:", sizes, "px; height:", sizes, "px")
           labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
            
          return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity))
        
    }
    output$map <- renderLeaflet({
        leaflet() %>% 
            addProviderTiles(providers$CartoDB.Positron  , group = "OSM")%>%
            addProviderTiles(providers$Esri.WorldImagery, group = "Satelite") %>%
            fitBounds(lng1 = min(-58.51593), 
                      lat1 = min(-34.67402), 
                      lng2 = max(-58.38615), 
                      lat2 = max(-34.53993)) %>% 
            addLayersControl(
                baseGroups = c("OSM","Satelite"),
                overlayGroups = c("Ruteo","Estaciones"), 
                options = layersControlOptions(collapsed = TRUE))
    })
    observe({
        
        
        labelsestaciones = sprintf(
            "<strong> De %s</strong>  <br/> Viajes al Destino: %s",
            filteredData2()$RUTA, filteredData2()$n
        ) %>% lapply(htmltools::HTML)
        
        
        labelsrecorridos = sprintf(
            "<strong> Ruta: %s</strong><br/> Duracion: %s <br/> Distancia[KM]: %s <br/> Viajes: %s",
            filteredData()$RUTA, filteredData()$duration,  filteredData()$distance, filteredData()$n
        ) %>% lapply(htmltools::HTML)  
        
        
        labelsorigen = sprintf(
            "<strong> Origen:</strong> %s",
            filteredData3()$nombre
        ) %>% lapply(htmltools::HTML)
        
        distancebins <-c(0, 2, 4, 6, 8, 10 , 12,14)
        #distancePal <- colorBin("plasma", bins = risk.bins, na.color = "#aaff56")
        
        
        binsRuteo = filteredData2()$distance %>%
            unique
        
        palRuteo <- colorBin(palette = "BuPu",domain = distancebins)
        
        binsOrigen = filteredData3()$nombre %>%
            unique
        
        palOrigen <-  colorFactor("black",domain = binsOrigen)
        
        
        
        leafletProxy("map") %>% 
            clearShapes() %>% 
            clearControls() %>% 
            addMapPane("Origen", zIndex = 420) %>%  
            addMapPane("Estaciones", zIndex = 410) %>% 
            addMapPane("Recorridos", zIndex = 400) %>% 
            addPolylines(data = filteredData(),
                         opacity = 0.8,
                         color = ~palRuteo(filteredData()$distance),
                         label = labelsrecorridos,
                         weight = log(filteredData()$n*2),
                         group = "Ruteo",
                         options = pathOptions(pane = "Recorridos")) %>% 
            addCircles(data = filteredData2(), 
                       color = "black",
                       label = labelsestaciones,
                       radius = log(filteredData2()$n)*25, 
                       weight = 2,
                       fillColor = "grey",
                       fillOpacity = 0.6,
                       group = "Estaciones",
                       options = pathOptions(pane = "Estaciones")) %>% 
            addCircles(data = filteredData3(), 
                   color = "red",
                   radius = 200, 
                   weight = 2,
                   fillColor = "black",
                   fillOpacity = 0.9,
                   group = "Origen",
                   label = labelsorigen,
                   options = pathOptions(pane = "Origen")) %>% 
            addLegendCustom(colors = "black", labels = "Origen", sizes = 20) %>% 
            addLegendCustom(colors = "grey", labels = "Destinos", sizes = 20) %>% 
            addLegend( pal = palRuteo,
                       values = filteredData2()$distance, 
                       opacity = 0.6, 
                       title = "Distancia en Km",
                       group = "Ruteo",
                       layerId = 3) 
        
    }) 
}

# Run the application 
shinyApp(ui = ui, server = server)

#clearShapes() %>% 