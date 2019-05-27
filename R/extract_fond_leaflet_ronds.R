extract_fond_leaflet_ronds <-
function(map)
  {
    # On recupere les index du leaflet concernant les differents affichages (objets carte, legende, titre ou source)
    idx_carte <- NULL
    idx_carte_ronds <- NULL
    idx_titre <- NULL
    idx_source <- NULL
    idx_legende_ronds <- NULL
    
    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[3]]$nom_couche == "carte_ronds") idx_carte <- c(idx_carte,i)
      }
      if(map$x$calls[[i]]$method %in% "addControl")
      {
        if(map$x$calls[[i]]$args[4]=="map-title") idx_titre <- i
        if(map$x$calls[[i]]$args[4]=="map-source") idx_source <- i
      }
      if(map$x$calls[[i]]$method %in% "addCircles")
      {
        if(map$x$calls[[i]]$args[[5]]$nom_couche=="carte_ronds") idx_carte_ronds <- c(idx_carte_ronds,i)
      }
      
      if(map$x$calls[[i]]$method %in% "addCircles")
      {
        if(map$x$calls[[i]]$args[[5]]$nom_couche=="legende_ronds") idx_legende_ronds <- c(idx_legende_ronds,i)
      }
      
      if(!is.null(idx_legende_ronds)) # la legende existe
      {
        if(map$x$calls[[i]]$method %in% "addMarkers")
        {
          if(map$x$calls[[i]]$args[[5]]$nom_couche=="legende_ronds") idx_legende_ronds <- c(idx_legende_ronds,i)
        }
        
        if(map$x$calls[[i]]$method %in% "addPolylines")
        {
          if(map$x$calls[[i]]$args[[3]]$nom_couche=="legende_ronds") idx_legende_ronds <- c(idx_legende_ronds,i)
        }
      }
    }
    
    if(is.null(idx_legende_ronds))
    {
      return(NULL)
    }else
    {
      list_fonds <- list()
      nom_fonds <- c()
      l <- 1
      
      for(i in 1:length(idx_carte))
      {
        aa <- lapply(1:length(map$x$calls[[idx_carte[i]]]$args[[1]]), function(x) lapply(c(1:length(map$x$calls[[idx_carte[i]]]$args[[1]][[x]])), function(y) st_polygon(list(as.matrix(map$x$calls[[idx_carte[i]]]$args[[1]][[x]][[y]][[1]])))))
        
        bb <- st_sf(geometry=st_sfc(NULL),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84")
        for(j in 1:length(aa))
        {
          bb <- rbind(bb,st_sf(geometry=st_sfc(st_multipolygon(lapply(1:length(aa[[j]]), function(x) aa[[j]][[x]]))),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84"))
        }
        bb <- bb[-1,]
        
        if(any(substring(map$x$calls[[idx_carte[i]]]$args[[5]],1,3) %in% "<b>"))
        {
          cc <- substring(map$x$calls[[idx_carte[i]]]$args[[5]],25,nchar(map$x$calls[[idx_carte[i]]]$args[[5]])-12)
          fond <- cbind(LIBELLE=cc,bb)
        }else
        {
          fond <- cbind(LIBELLE=map$x$calls[[idx_carte[i]]]$args[[5]],bb)
        }
        
        fond <- st_transform(fond,paste0("+init=epsg:",map$x$calls[[idx_carte[i]]]$args[[3]]$code_epsg))
        
        list_fonds[[l]] <- fond
        
        nom_fonds <- c(nom_fonds,map$x$calls[[idx_carte[i]]]$args[[3]]$nom_fond)
        
        l <- l+1
      }
      
      for(i in 1:length(idx_carte_ronds))
      {
        dom <- map$x$calls[[idx_carte_ronds[i]]]$args[[5]]$dom
        
        centres_ronds <- data_frame(lng=map$x$calls[[idx_carte_ronds[i]]]$args[[2]],lat=map$x$calls[[idx_carte_ronds[i]]]$args[[1]])
        aa <- apply(centres_ronds,1, function(x) st_sf(geometry=st_sfc(st_point(x),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84")))
        bb <- do.call("rbind",aa)
        cc <- st_transform(bb,paste0("+init=epsg:",map$x$calls[[idx_carte_ronds[i]]]$args[[5]]$code_epsg))
        ronds_pl <- st_buffer(cc, map$x$calls[[idx_carte_ronds[i]]]$args[[3]])
        
        col_bor <- map$x$calls[[idx_carte_ronds[i]]]$args[[6]]$color
        ronds_pl <- cbind(COL_BOR=col_bor,ronds_pl)
        col <- map$x$calls[[idx_carte_ronds[i]]]$args[[6]]$fillColor
        ronds_pl <- cbind(COL=col,ronds_pl)
        val <- as.numeric(str_replace_all(str_replace_all(substring(map$x$calls[[idx_carte_ronds[i]]]$args[[7]],str_locate_all(map$x$calls[[idx_carte_ronds[i]]]$args[[7]],">")[[1]][6]+1,nchar(map$x$calls[[idx_carte_ronds[i]]]$args[[7]])-11)," ",""),",","."))
        ronds_pl <- cbind(VAL=val,ronds_pl)
        
        fond_pos <- NULL
        fond_neg <- NULL
        
        if(nrow(ronds_pl[ronds_pl$VAL>0,])>0)
        {
          fond_pos <- ronds_pl[ronds_pl$VAL>0,]
        }
        if(nrow(ronds_pl[ronds_pl$VAL<0,])>0)
        {
          fond_neg <- ronds_pl[ronds_pl$VAL<0,]
        }
        
        if(!is.null(fond_pos))
        {
          list_fonds[[l]] <- fond_pos
          nom_fonds <- c(nom_fonds,map$x$calls[[idx_carte_ronds[i]]]$args[[5]]$nom_fond[1])
          l <- l+1
        }
        if(!is.null(fond_neg))
        {
          list_fonds[[l]] <- fond_neg
          nom_fonds <- c(nom_fonds,map$x$calls[[idx_carte_ronds[i]]]$args[[5]]$nom_fond[2])
          l <- l+1
        }
      }
      
      for(i in 1:length(idx_legende_ronds))
      {
        if(map$x$calls[[idx_legende_ronds[i]]]$method %in% "addCircles")
        {
          centres_ronds <- data_frame(lng=map$x$calls[[idx_legende_ronds[i]]]$args[[2]],lat=map$x$calls[[idx_legende_ronds[i]]]$args[[1]])
          aa <- apply(centres_ronds,1, function(x) st_sf(geometry=st_sfc(st_point(x),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84")))
          bb <- do.call("rbind",aa)
          cc <- st_transform(bb,paste0("+init=epsg:",map$x$calls[[idx_legende_ronds[i]]]$args[[5]]$code_epsg))
          ronds_pl <- st_buffer(cc, map$x$calls[[idx_legende_ronds[i]]]$args[[3]])
          
          ronds_pl_leg <- ronds_pl
          val <- c(map$x$calls[[idx_legende_ronds[i]]]$args[[7]][1],map$x$calls[[idx_legende_ronds[i]]]$args[[7]][2])
          ronds_pl_leg <- cbind(VAL=val,ronds_pl_leg)
          
          list_fonds[[l]] <- ronds_pl_leg
          nom_fonds <- c(nom_fonds,map$x$calls[[idx_legende_ronds[i]]]$args[[5]]$nom_fond)
          l <- l+1
        }
        
        if(map$x$calls[[idx_legende_ronds[i]]]$method %in% "addPolylines")
        {
          # Pour l'export Qgis en projection locale
          x1_grand_pl <- st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"X"][which.max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"Y"])]
          y1_grand_pl <- max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"Y"])
          pts1_grand_pl <- c(x1_grand_pl,y1_grand_pl)
          x2_grand_pl <- x1_grand_pl+(max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"X"])-x1_grand_pl)+(max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"X"])-min(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"X"]))/3
          y2_grand_pl <- max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"Y"])
          pts2_grand_pl <- c(x2_grand_pl,y2_grand_pl)
          ligne_grand_pl <- rbind(pts1_grand_pl,pts2_grand_pl)
          
          x1_petit_pl <- x1_grand_pl
          y1_petit_pl <- max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==2),"Y"])
          pts1_petit_pl <- c(x1_petit_pl,y1_petit_pl)
          x2_petit_pl <- x2_grand_pl
          y2_petit_pl <- max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==2),"Y"])
          pts2_petit_pl <- c(x2_petit_pl,y2_petit_pl)
          ligne_petit_pl <- rbind(pts1_petit_pl,pts2_petit_pl)
          
          lignes_pl <- st_sf(st_geometry(st_multilinestring(list(ligne_grand_pl,ligne_petit_pl))))
          lignes_pl <- st_set_crs(lignes_pl,paste0("+init=epsg:",map$x$calls[[idx_legende_ronds[i]]]$args[[3]]$code_epsg))
          
          list_fonds[[l]] <- lignes_pl
          nom_fonds <- c(nom_fonds,map$x$calls[[idx_legende_ronds[i]]]$args[[3]]$nom_fond)
          l <- l+1
        }
      }
    }
    
    return(list(list_fonds,nom_fonds,dom))
  }