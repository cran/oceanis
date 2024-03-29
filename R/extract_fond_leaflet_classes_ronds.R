extract_fond_leaflet_classes_ronds <-
function(map)
  {
    # On recupere les index du leaflet concernant les differents affichages (objets carte, legende, titre ou source)
    idx_carte <- NULL
    idx_carte_ronds <- NULL
    idx_titre <- NULL
    idx_source <- NULL
    idx_legende <- NULL
    idx_legende_ronds <- NULL

    for(i in 1:length(map$x$calls))
    {
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[3]] %in% c("carte_classes_ronds_init","carte_ronds","carte_ronds_elargi","carte_classes","carte_classes_elargi")) idx_carte <- c(idx_carte,i)
      }
      if(map$x$calls[[i]]$method %in% "addControl")
      {
        if(map$x$calls[[i]]$args[4]=="map-title") idx_titre <- i
        if(map$x$calls[[i]]$args[4]=="map-source") idx_source <- i
      }
      if(map$x$calls[[i]]$method %in% "addCircles")
      {
        if(map$x$calls[[i]]$args[[5]] %in% c("carte_ronds","carte_ronds_elargi")) idx_carte_ronds <- c(idx_carte_ronds,i)
      }
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[3]]=="legende_classes_rectangle") idx_legende <- c(idx_legende,i)
      }
      if(map$x$calls[[i]]$method %in% "addPolygons")
      {
        if(map$x$calls[[i]]$args[[3]]=="legende_ronds") idx_legende_ronds <- c(idx_legende_ronds,i)
      }
      if(!is.null(idx_legende)) # la legende de classes existe
      {
        if(map$x$calls[[i]]$method %in% "addPolygons")
        {
          if(map$x$calls[[i]]$args[[3]]=="legende_classes") idx_legende <- c(idx_legende,i)
        }
        if(map$x$calls[[i]]$method %in% "addMarkers")
        {
          if(map$x$calls[[i]]$args[[5]]=="legende_classes") idx_legende <- c(idx_legende,i)
        }
      }
      if(!is.null(idx_legende_ronds)) # la legende de ronds existe
      {
        if(map$x$calls[[i]]$method %in% "addCircles")
        {
          if(map$x$calls[[i]]$args[[5]]=="legende_ronds") idx_legende_ronds <- c(idx_legende_ronds,i)
        }
        if(map$x$calls[[i]]$method %in% "addPolylines")
        {
          if(map$x$calls[[i]]$args[[3]]=="legende_ronds") idx_legende_ronds <- c(idx_legende_ronds,i)
        }
        if(map$x$calls[[i]]$method %in% "addMarkers")
        {
          if(map$x$calls[[i]]$args[[5]]=="legende_ronds") idx_legende_ronds <- c(idx_legende_ronds,i)
        }
      }
    }

    if(is.null(idx_legende) | is.null(idx_legende_ronds))
    {
      return(NULL)
    }else
    {
      var_ronds <- map$x$calls[[idx_carte_ronds[length(idx_carte_ronds)]]]$args[[4]]$var_volume
      var_classes <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[2]]$var_ratio

      code_epsg <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[2]]$code_epsg
      emprise <- map$x$calls[[idx_carte[length(idx_carte)]]]$args[[2]]$emprise

      list_fonds <- list()
      nom_fonds <- c()
      l <- 1

      for(i in 1:length(idx_carte))
      {
        fond <- map$x$calls[[idx_carte[i]]]$args[[2]][1][[1]]
        nom_col <- names(fond)[-which(names(fond)=="geometry")]
        aa <- map$x$calls[[idx_carte[i]]]$args[[4]]$fillColor
        fond <- cbind(fond,classe=aa)
        fond <- fond[,c(nom_col,"classe","geometry")]

        bb <- lapply(1:length(unique(fond$classe)), function(x) fond[fond$classe %in% rev(unique(fond$classe))[x],"classe"] <<- x)
        rm(aa,bb)

        fond <- st_transform(fond,crs=as.numeric(code_epsg))

        list_fonds[[l]] <- fond
        rm(fond)

        nom_fonds <- c(nom_fonds,map$x$calls[[idx_carte[i]]]$args[[2]]$nom_fond)

        l <- l+1
      }

      for(i in 1:length(idx_carte_ronds))
      {
        if(length(map$x$calls[[idx_carte_ronds[1]]]$args[[4]][[2]])==4) # representation elargie
        {
          if(i==1) # couche elargie
          {
            arg_fond <- 3
            arg_donnees <- 4
          }else # couche analyse
          {
            arg_fond <- 1
            arg_donnees <- 2
          }
        }else # representation normale
        {
          arg_fond <- 1
          arg_donnees <- 2
        }

        centres_ronds <- map$x$calls[[idx_carte_ronds[i]]]$args[[4]][[2]][[arg_fond]]
        donnees <- map$x$calls[[idx_carte_ronds[i]]]$args[[4]][[2]][[arg_donnees]][,c("CODE","LIBELLE",var_ronds)]

        fond <- st_buffer(centres_ronds, map$x$calls[[idx_carte_ronds[i]]]$args[[3]])
        fond <- cbind(donnees,fond)
        fond <- st_sf(fond)

        col_bor <- map$x$calls[[idx_carte_ronds[i]]]$args[[6]]$color
        fond <- cbind(COL_BOR=col_bor,fond)
        ronds_pl <- fond[,c("CODE","LIBELLE",var_ronds,"COL_BOR","geometry")]
        rm(fond)

        list_fonds[[l]] <- ronds_pl
        nom_fonds <- c(nom_fonds,map$x$calls[[idx_carte_ronds[i]]]$args[[4]]$nom_fond)
        l <- l+1
      }

      if(!is.null(idx_titre))
      {
        titre <- substr(map$x$calls[[idx_titre]]$args[1],505,nchar(map$x$calls[[idx_titre]]$args[1])-7)
      }else
      {
        titre <- ""
      }

      if(!is.null(idx_source))
      {
        source <- substr(map$x$calls[[idx_source]]$args[1],379,nchar(map$x$calls[[idx_source]]$args[1])-7)
      }else
      {
        source <- ""
      }

      if(!is.null(idx_legende))
      {
        label <- NULL
        palette <- NULL
        for(i in 1:length(idx_legende))
        {
          if(i==length(idx_legende))
          {
            titre_leg <- map$x$calls[[idx_legende[i]]]$args[[11]]
          }else
          {
            if(map$x$calls[[idx_legende[i]]]$method %in% "addMarkers")
            {
              label <- c(label,map$x$calls[[idx_legende[i]]]$args[[11]])
            }
            if(map$x$calls[[idx_legende[i]]]$method %in% "addPolygons")
            {
              if(map$x$calls[[idx_legende[i]]]$args[[3]] != "legende_classes_rectangle")
              {
                palette <- c(palette,map$x$calls[[idx_legende[i]]]$args[4][[1]]$fillColor)
              }
            }
          }
        }
        table_classe <- data.frame(classe=c(length(label):1),label=label,couleurs=palette, stringsAsFactors = F)
      }

      if(!is.null(idx_legende_ronds))
      {
        for(i in 1:length(idx_legende_ronds))
        {
          if(map$x$calls[[idx_legende_ronds[i]]]$method %in% "addCircles")
          {
            centres_ronds <- data.frame(lng=map$x$calls[[idx_legende_ronds[i]]]$args[[2]],lat=map$x$calls[[idx_legende_ronds[i]]]$args[[1]])
            aa <- apply(centres_ronds,1, function(x) st_sf(geometry=st_sfc(st_point(x),crs=4326)))
            bb <- do.call("rbind",aa)
            cc <- st_transform(bb,crs=as.numeric(map$x$calls[[idx_legende_ronds[i]]]$args[[4]]$code_epsg))
            dd <- st_buffer(cc, map$x$calls[[idx_legende_ronds[i]]]$args[[3]])

            val <- c(map$x$calls[[idx_legende_ronds[i]]]$args[[7]][1],map$x$calls[[idx_legende_ronds[i]]]$args[[7]][2])
            ronds_pl_leg <- cbind(ETI_VAL=val,dd)

            list_fonds[[l]] <- ronds_pl_leg
            nom_fonds <- c(nom_fonds,map$x$calls[[idx_legende_ronds[i]]]$args[[4]]$nom_fond)
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

            nom_fonds <- c(nom_fonds,map$x$calls[[idx_legende_ronds[i]]]$args[[2]]$nom_fond)
            l <- l+1
          }
        }
      }

      return(list(list_fonds,nom_fonds,titre,source,table_classe,titre_leg,var_classes,var_ronds,emprise))
    }
  }
