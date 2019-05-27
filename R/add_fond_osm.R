add_fond_osm <-
function(map)
  {
    msg_error1 <- NULL
    
    if(any(!any(class(map) %in% "leaflet"),!any(class(map) %in% "htmlwidget"))) msg_error1 <- "La carte doit etre un objet leaflet / "
    
    if(any(!is.null(msg_error1)))
    {
      stop(simpleError(msg_error1))
    }
    
    j <- NULL
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        j <- c(j,i)
      }
    }
    map$x$calls <- map$x$calls[-j[c(1,2)]]
    
    map$x$calls[[4]]$args[[4]]$weight <- 2
    
    map <- addTiles(map,
                    urlTemplate = "//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
                    attribution = "OCEANIS - <a href=\"http://www.insee.fr\">INSEE</a>")
    
    return(map)
  }