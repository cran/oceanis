plot_classes_ronds <-
function(data,fondMaille,fondSousAnalyse=NULL,fondSurAnalyse=NULL,idData,varVolume,varRatio,rayonRond=NULL,rapportRond=NULL,methode="kmeans",nbClasses=3,bornes=NULL,precisionLegRonds=0,precisionLegClasses=1,dom="0",fondChx=NULL,titreLegRonds="",titreLegClasses="",xLegRonds=NULL,yLegRonds=NULL,xLegClasses=NULL,yLegClasses=NULL,titreCarte="",sourceCarte="",etiquettes=NULL,stylePalette="defaut",palettePos=NULL,paletteNeg=NULL,colBorder="white",colBorderRonds="#303030")
  {
    options("stringsAsFactors"=FALSE)
    
    # Verification des parametres
    
    msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10<-msg_error11<-msg_error12<-msg_error13<-msg_error14<-msg_error15<-msg_error16<-msg_error17<-msg_error18<-msg_error19<-msg_error20<-msg_error21<-msg_error22<-msg_error23<-msg_error24<-msg_error25<-msg_error26<-msg_error27<-msg_error28<-msg_error29<-msg_error30<-msg_error31<-msg_error32<-msg_error33<-msg_error34<-msg_error35<-msg_error36 <- NULL
    
    if(any(class(data)!="data.frame")) msg_error1 <- "Les donnees doivent etre dans un data.frame / "
    if(any(!any(class(fondMaille) %in% "sf"),!any(class(fondMaille) %in% "data.frame"))) msg_error2 <- "Le fond de maille doit etre un objet sf / "
    if(!is.null(fondSousAnalyse)) if(any(!any(class(fondSousAnalyse[[1]]) %in% "sf"),!any(class(fondSousAnalyse[[1]]) %in% "data.frame"))) msg_error3 <- "Les fonds a positionner en-dessous de l'analyse doivent etre une liste d'objets sf / "
    if(!is.null(fondSurAnalyse)) if(any(!any(class(fondSurAnalyse[[1]]) %in% "sf"),!any(class(fondSurAnalyse[[1]]) %in% "data.frame"))) msg_error4 <- "Les fonds a positionner au-dessus de l'analyse doivent etre une liste d'objets sf / "
    if(any(class(idData)!="character")) msg_error5 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(varVolume)!="character")) msg_error6 <- "Le nom de la variable doit etre de type caractere / "
    if(any(class(varRatio)!="character")) msg_error7 <- "Le nom de la variable doit etre de type caractere / "
    if(!is.null(rayonRond)) if(any(class(rayonRond)!="numeric")) msg_error8 <- "La variable doit etre de type numerique / "
    if(!is.null(rapportRond)) if(any(class(rapportRond)!="numeric")) msg_error9 <- "La variable doit etre de type numerique / "
    if(any(class(methode)!="character")) msg_error10 <- "La nom de la methode doit etre de type caractere / "
    if(any(class(nbClasses)!="numeric")) msg_error11 <- "La variable doit etre de type numerique / "
    if(!is.null(bornes)) if(any(class(bornes)!="numeric")) msg_error12 <- "La variable doit etre un vecteur numerique / "
    if(any(class(precisionLegRonds)!="numeric")) msg_error13 <- "La variable precisionLegRonds doit etre de type numerique / "
    if(any(class(precisionLegClasses)!="numeric")) msg_error14 <- "La variable precisionLegClasses doit etre de type numerique / "
    if(any(class(dom)!="character")) msg_error15 <- "La valeur doit etre de type caractere ('0', '971', '972', '973', '974' ou '976') / "
    if(!is.null(fondChx)) if(any(!any(class(fondChx) %in% "sf"),!any(class(fondChx) %in% "data.frame"))) msg_error16 <- "Le fond des chx doit etre un objet sf / "
    if(any(class(titreLegRonds)!="character")) msg_error17 <- "Le titre de la legende doit etre de type caractere / "
    if(any(class(titreLegClasses)!="character")) msg_error18 <- "Le titre de la legende doit etre de type caractere / "
    if(!is.null(xLegRonds)) if(any(class(xLegRonds)!="numeric")) msg_error19 <- "La variable xLegRonds doit etre de type numerique / "
    if(!is.null(yLegRonds)) if(any(class(yLegRonds)!="numeric")) msg_error20 <- "La variable yLegRonds doit etre de type numerique / "
    if(!is.null(xLegClasses)) if(any(class(xLegClasses)!="numeric")) msg_error21 <- "La variable xLegClasses doit etre de type numerique / "
    if(!is.null(yLegClasses)) if(any(class(yLegClasses)!="numeric")) msg_error22 <- "La variable yLegClasses doit etre de type numerique / "
    if(any(class(titreCarte)!="character")) msg_error23 <- "Le titre de la carte doit etre de type caractere / "
    if(any(class(sourceCarte)!="character")) msg_error24 <- "La source de la carte doit etre de type caractere / "
    if(!is.null(etiquettes)) if(!any(class(etiquettes) %in% "character" | class(etiquettes) %in% "data.frame")) msg_error25 <- "La table des etiquettes peut etre soit un vecteur caractere soit un data.frame (voir aide) / "
    if(any(class(stylePalette)!="character")) msg_error26 <- "Le style de la palette doit etre de type caractere ('InseeFlash', 'InseeAnalyse', 'InseeDossier', 'InseePremiere' ou 'defaut') / "
    if(!is.null(palettePos)) if(any(class(palettePos)!="character")) msg_error27 <- "La palette des classes doit etre un vecteur de type caractere / "
    if(!is.null(paletteNeg)) if(any(class(paletteNeg)!="character")) msg_error28 <- "La palette des classes doit etre un vecteur de type caractere / "
    if(any(class(colBorder)!="character")) msg_error29 <- "La couleur de la bordure doit etre de type caractere (nommee ou hexadecimal) / "
    
    if(length(names(data))<2) msg_error30 <- "Le tableau des donnees n'est pas conforme. Il doit contenir au minimum une variable identifiant et la variable a representer / "
    if(length(names(fondMaille))<3) msg_error31 <- "Le fond de maille n'est pas conforme. La table doit contenir au minimum une variable identifiant, une variable libelle et la geometry / "
    
    if(!any(names(data) %in% idData))  msg_error32 <- "La variable identifiant les donnees n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% varVolume))  msg_error33 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!any(names(data) %in% varRatio))  msg_error34 <- "La variable a representer n'existe pas dans la table des donnees / "
    if(!methode %in% c("kmeans","fisher","jenks","quantile")) msg_error35 <- "Le nom de la methode doit etre 'kmeans', 'fisher', 'jenks' ou 'quantile' / "
    if(!dom %in% c("0","971","972","973","974","976")) msg_error36 <- "La variable dom doit etre '0', '971', '972', '973', '974' ou '976' / "
    
    if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),
           !is.null(msg_error5),!is.null(msg_error6),!is.null(msg_error7),!is.null(msg_error8),
           !is.null(msg_error9),!is.null(msg_error10),!is.null(msg_error11),!is.null(msg_error12),
           !is.null(msg_error13),!is.null(msg_error14),!is.null(msg_error15),!is.null(msg_error16),
           !is.null(msg_error17),!is.null(msg_error18),!is.null(msg_error19),!is.null(msg_error20),
           !is.null(msg_error21),!is.null(msg_error22),!is.null(msg_error23),!is.null(msg_error24),
           !is.null(msg_error25),!is.null(msg_error26),!is.null(msg_error27),!is.null(msg_error28),
           !is.null(msg_error29),!is.null(msg_error30),!is.null(msg_error31),!is.null(msg_error32),
           !is.null(msg_error33),!is.null(msg_error34),!is.null(msg_error35),!is.null(msg_error36)))
    {
      stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,
                              msg_error9,msg_error10,msg_error11,msg_error12,msg_error13,msg_error14,msg_error15,msg_error16,
                              msg_error17,msg_error18,msg_error19,msg_error20,msg_error21,msg_error22,msg_error23,msg_error24,
                              msg_error25,msg_error26,msg_error27,msg_error28,msg_error29,msg_error30,msg_error31,msg_error32,
                              msg_error33,msg_error34,msg_error35,msg_error36)))
    }
    
    if(!is.null(fondChx))
    {
      centroid <- "chx"
    }else
    {
      centroid <- "centroid"
    }
    
    names(data)[names(data)==idData] <- "CODE"
    names(fondMaille)[1] <- "CODE"
    names(fondMaille)[2] <- "LIBELLE"
    if(!is.null(fondSousAnalyse)) 
    {
      for(i in 1:length(fondSousAnalyse))
      {
        names(fondSousAnalyse[[i]])[1] <- "CODE"
        names(fondSousAnalyse[[i]])[2] <- "LIBELLE"
        fondSousAnalyse[[i]]$LIBELLE<-iconv(fondSousAnalyse[[i]]$LIBELLE,"latin1","utf8")
      }
    }
    if(!is.null(fondSurAnalyse)) 
    {
      for(i in 1:length(fondSurAnalyse))
      {
        names(fondSurAnalyse[[i]])[1] <- "CODE"
        names(fondSurAnalyse[[i]])[2] <- "LIBELLE"
        fondSurAnalyse[[i]]$LIBELLE<-iconv(fondSurAnalyse[[i]]$LIBELLE,"latin1","utf8")
      }
    }
    fondMaille$LIBELLE<-iconv(fondMaille$LIBELLE,"latin1","utf8")
    if(titreLegRonds!="")
    {
      titreLegRonds<-iconv(titreLegRonds,"latin1","utf8")
    }
    if(titreLegClasses!="")
    {
      titreLegClasses<-iconv(titreLegClasses,"latin1","utf8")
    }
    if(titreCarte!="")
    {
      titreCarte<-iconv(titreCarte,"latin1","utf8")
    }
    if(sourceCarte!="")
    {
      sourceCarte<-iconv(sourceCarte,"latin1","utf8")
    }
    
    code_epsg <- switch(dom, #DOM
                        "0"="2154",# Lambert 93
                        "971"="32620",# UTM 20 N
                        "972"="32620",# UTM 20 N
                        "973"="2972",# UTM 22 N
                        "974"="2975",# UTM 40 S
                        "976"="4471")# UTM 38 S
    
    if(is.null(palettePos) & is.null(paletteNeg))
    {
      palette <- recup_palette(stylePalette)
      inseePos <- palette[[1]]
      inseeNeg <- palette[[2]]
    }else
    {
      inseePos <- NULL
      inseeNeg <- NULL
      stylePalette <- NULL
      if(!is.null(palettePos)) inseePos <- palettePos
      if(!is.null(paletteNeg)) inseeNeg <- paletteNeg
    }
    
    # ANALYSE EN RONDS
    
    # Calcul du rayon du rond max
    
    #Aire totale du territoire d'etude
    aire_territoire <- as.numeric(sum(st_area(fondMaille[fondMaille$CODE %in% data[,"CODE"],]))) #Superficie du territoire
    #valeur max de la serie de donnees
    suppressWarnings(max_var <- max(data[data[,"CODE"] %in% fondMaille$CODE,varVolume], na.rm = TRUE))
    
    serie <- data[data[,"CODE"] %in% fondMaille$CODE,varVolume]
    serie <- serie[!is.na(serie)]
    #on ramene la serie a un quotient fonction de la valeur max
    quotient <- serie/max_var
    #somme des carres
    somme_quotient <- sum(quotient^2)
    #calcul de la surface max du rond le plus grand
    max_surface_rond <- (aire_territoire/(7*somme_quotient))
    #calcul du rayon max du rond le plus grand
    max_rayon_metres <- sqrt(max_surface_rond/pi)
    
    if(!is.null(rayonRond))
    {
      if(rayonRond>max_rayon_metres)
      {
        stop(simpleError(paste0("Le rayon du rond le plus grand est trop eleve et ne permet pas de respecter la regle semiologique des 1/7eme. Le rayon max est ",round(max_rayon_metres,2)," metres.")))
      }
    }
    
    if(is.null(rayonRond) & is.null(rapportRond))
    {
      rayonRond <- max_rayon_metres/1.25
      rapportRond <- (pi*(rayonRond)^2)/max_var
    }
    if(!is.null(rayonRond) & is.null(rapportRond))
    {
      rapportRond <- (pi*(rayonRond)^2)/max_var
    }
    if(is.null(rayonRond) & !is.null(rapportRond)) #Calcul du rayon a partir du rapport
    {
      rayonRond <- round(sqrt((rapportRond*max_var)/pi),0)
    }
    
    # Analyse
    analyse_ronds <- k_ronds(fondMaille,NULL,names(fondMaille)[1],data,"CODE",varVolume,F,centroid,fondChx)
    
    if(is.null(analyse_ronds))
    {
      stop(simpleError("La maille ne correspond pas au niveau geographique du fichier de donnees. Veuillez svp choisir une maille adaptee ou modifier le fichier de donnees."))
    }
    
    fond_points <- analyse_ronds[[1]]
    rayons_ronds <-  rayonRond*sqrt(analyse_ronds$donnees[,varVolume]/max_var)
    fond_ronds <- st_buffer(fond_points, rayons_ronds)
    fond_ronds <- st_sf(CODE=analyse_ronds$donnees$CODE,LIBELLE=analyse_ronds$donnees$LIBELLE,VAR=analyse_ronds$donnees[,varVolume],geometry=st_sfc(st_geometry(fond_ronds),crs=st_crs(fondMaille)))
    names(fond_ronds) <- c("CODE","LIBELLE",varVolume,"geometry")
    
    if(is.null(xLegRonds) | is.null(yLegRonds))
    {
      xLegRonds <- st_bbox(fondMaille)$xmax-(st_bbox(fondMaille)$xmax-st_bbox(fondMaille)$xmin)/10
      yLegRonds <- st_bbox(fondMaille)$ymax-(st_bbox(fondMaille)$ymax-st_bbox(fondMaille)$ymin)/10
    }
    
    points_L93 <- data.frame(xLeg=xLegRonds,yLeg=yLegRonds,stringsAsFactors = FALSE)
    
    list_points <- apply(points_L93,1, function(x) st_sf(geometry=st_sfc(st_point(x),crs=paste0("+init=epsg:",code_epsg))))
    fond_points_L93 <- do.call("rbind",list_points)
    fond_points_WGS84 <- st_transform(fond_points_L93,paste0("+init=epsg:4326 +proj=longlat +ellps=WGS84"))
    
    fond_points_WGS84 <- data.frame(st_coordinates(fond_points_WGS84))
    names(fond_points_WGS84) <- c("lng","lat")
    
    ronds_leg <- construction_ronds_legende(fond_points_WGS84$lng,fond_points_WGS84$lat,st_crs(fondMaille)[1]$epsg,rayonRond)
    
    ronds_pl_leg <- ronds_leg[[2]]
    
    # Pour l'export Qgis en projection locale
    x1_grand_pl <- st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"X"][which.max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"Y"])]
    y1_grand_pl <- max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"Y"])
    pts1_grand_pl <- c(x1_grand_pl,y1_grand_pl)
    x2_grand_pl <- x1_grand_pl+(max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"X"])-x1_grand_pl)+(max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"X"])-min(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"X"]))/3
    y2_grand_pl <- max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==1),"Y"])
    pts2_grand_pl <- c(x2_grand_pl,y2_grand_pl)
    ligne_grand_pl <- rbind(pts1_grand_pl,pts2_grand_pl,pts2_grand_pl,pts1_grand_pl)
    
    x1_petit_pl <- x1_grand_pl
    y1_petit_pl <- max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==2),"Y"])
    pts1_petit_pl <- c(x1_petit_pl,y1_petit_pl)
    x2_petit_pl <- x2_grand_pl
    y2_petit_pl <- max(st_coordinates(ronds_pl_leg)[which(st_coordinates(ronds_pl_leg)[,4]==2),"Y"])
    pts2_petit_pl <- c(x2_petit_pl,y2_petit_pl)
    ligne_petit_pl <- rbind(pts1_petit_pl,pts2_petit_pl,pts2_petit_pl,pts1_petit_pl)
    
    gr <- as.matrix(st_coordinates(ronds_pl_leg[1,])[,c("X","Y")])
    pr <- as.matrix(st_coordinates(ronds_pl_leg[2,])[,c("X","Y")])
    fond_leg_gr <- st_polygon(list(gr,ligne_grand_pl))
    fond_leg_pr <- st_polygon(list(pr,ligne_petit_pl))
    
    fond_leg_gr <- st_sf(geometry=st_sfc(st_geometry(fond_leg_gr),crs=st_crs(fondMaille)))
    fond_leg_pr <- st_sf(geometry=st_sfc(st_geometry(fond_leg_pr),crs=st_crs(fondMaille)))
    fond_leg_ronds <- rbind(fond_leg_gr,fond_leg_pr)
    
    fond_leg_ronds <- st_sf(VAR=c(max_var,max_var/3),geometry=st_sfc(st_geometry(fond_leg_ronds),crs=st_crs(fondMaille)))
    names(fond_leg_ronds) <- c(varVolume,"geometry")
    
    # ANALYSE EN CLASSES
    
    data[,varRatio] <- round(data[,varRatio],precisionLegClasses)
    
    analyse_classes <- merge(fondMaille[,c("CODE","geometry")],data,by="CODE")
    
    max <- max(as.data.frame(analyse_classes)[,varRatio])
    min <- min(as.data.frame(analyse_classes)[,varRatio])
    
    if(is.null(bornes))
    {
      suppressWarnings(bornes_analyse <- classIntervals(as.numeric(as.data.frame(analyse_classes)[,varRatio]),nbClasses,style=methode,rtimes=10,intervalClosure="left"))
      
      if(!is.null(stylePalette))
      {
        carac_bornes <- calcul_bornes(as.data.frame(analyse_classes),bornes_analyse,varRatio,nbClasses,methode,stylePalette=stylePalette)
      }else
      {
        carac_bornes <- calcul_bornes(as.data.frame(analyse_classes),bornes_analyse,varRatio,nbClasses,methode,stylePalette=stylePalette,palettePos=palettePos,paletteNeg=paletteNeg)
      }
      bornes <- carac_bornes[[1]]
      bornes[1] <- max(as.numeric(as.data.frame(analyse_classes)[,varRatio]))
      bornes_sansext <- bornes[-1]
      bornes_sansext <- bornes_sansext[-length(bornes_sansext)]
      bornes_sansext <- sort(bornes_sansext, decreasing = TRUE)
      bornes <- c(max,bornes_sansext,min)
      bornes <- round(bornes,precisionLegClasses)
      
      pal_classes <- carac_bornes[[2]]
    }else
    {
      bornes_sansext <- sort(bornes, decreasing = TRUE)
      bornes <- unique(c(max,bornes_sansext,min))
      bornes <- round(bornes,precisionLegClasses)
      
      if(min<0 & max>=0) # Si - et +
      {
        pal_classes_pos <- inseePos[(length(inseePos)-length(bornes[bornes>0])+1):length(inseePos)]
        pal_classes_neg <- inseeNeg[1:length(bornes[bornes<0])]
        pal_classes <- c(pal_classes_pos,pal_classes_neg)
      }
      if(min>=0) # Si +
      {
        if(length(bornes)>=(length(inseePos)+1))
        {
          pal_classes <- inseePos
        }else
        {
          pal_classes <- inseePos[-c(1:(length(inseePos)-length(bornes)+1))] # On enleve les couleurs fonces inutiles
        }
      }
      if(max<0) # Si -
      {
        if(length(bornes)>=(length(inseeNeg)+1))
        {
          pal_classes <- inseeNeg
        }else
        {
          pal_classes <- inseeNeg[c(1:(length(bornes)-1))] # On enleve les couleurs fonces inutiles
        }
      }
    }
    
    if(is.null(pal_classes)) pal_classes <- "grey"
    
    pal_classes[is.na(pal_classes)] <- "grey"
    palette<-colorBin(palette=rev(pal_classes), domain=0:100, bins=bornes, na.color="grey")
    col <- palette(as.data.frame(analyse_classes)[,varRatio])
    
    analyse_classes <- cbind(as.data.frame(analyse_classes)[,-ncol(analyse_classes)],PALETTE=col,geometry=analyse_classes$geometry)
    
    ff <- lapply(1:length(pal_classes), function(x) analyse_classes[analyse_classes$PALETTE %in% rev(pal_classes)[x],"classe"] <<- x)
    rm(ff)
    analyse_classes <- analyse_classes[,c(1:(ncol(analyse_classes)-2),ncol(analyse_classes),ncol(analyse_classes)-1)]
    
    fond_classes <- st_as_sf(analyse_classes)
    
    x_marge <- (st_bbox(fondMaille)$xmax-st_bbox(fondMaille)$xmin)/20
    y_marge <- (st_bbox(fondMaille)$ymax-st_bbox(fondMaille)$ymin)/20
    
    if(is.null(xLegClasses) | is.null(yLegClasses))
    {
      xLegClasses <- st_bbox(fondMaille)$xmax-(st_bbox(fondMaille)$xmax-st_bbox(fondMaille)$xmin)/10
      yLegClasses <- st_bbox(fondMaille)$ymin+(st_bbox(fondMaille)$ymax-st_bbox(fondMaille)$ymin)/5
    }
    x_large <- (st_bbox(fondMaille)$xmax-st_bbox(fondMaille)$xmin)/20
    y_large <- x_large/1.5
    
    rectangle <- matrix(c(xLegClasses-x_large,yLegClasses,xLegClasses,yLegClasses,xLegClasses,yLegClasses-y_large,xLegClasses-x_large,yLegClasses-y_large,xLegClasses-x_large,yLegClasses),ncol=2, byrow=TRUE)
    fond_leg_classes <- st_sf(geometry=st_sfc(st_polygon(list(rectangle))),crs=st_crs(fondMaille))
    for(i in 2:length(pal_classes))
    {
      rectangle <- matrix(c(xLegClasses-x_large,yLegClasses-(i-1)*x_large,xLegClasses,yLegClasses-(i-1)*x_large,xLegClasses,(yLegClasses-(i-1)*x_large)-y_large,xLegClasses-x_large,(yLegClasses-(i-1)*x_large)-y_large,xLegClasses-x_large,yLegClasses-(i-1)*x_large),ncol=2, byrow=TRUE)
      rectangle <- st_sf(geometry=st_sfc(st_polygon(list(rectangle))),crs=st_crs(fondMaille))
      fond_leg_classes <- rbind(fond_leg_classes,rectangle)
    }
    
    label_rectangle <- NULL
    for(i in 1:length(pal_classes))
    {
      if(i==1)
      {
        label_rectangle <- c(label_rectangle,paste0(format(round(bornes[i+1],precisionLegClasses), big.mark=" ",decimal.mark=",",nsmall=0)," et plus"))
      }else if(i==length(pal_classes))
      {
        label_rectangle <- c(label_rectangle,paste0("Moins de ", format(round(bornes[i],precisionLegClasses), big.mark=" ",decimal.mark=",",nsmall=0)))
      }else
      {
        label_rectangle <- c(label_rectangle,paste0("De ", format(round(bornes[i+1],precisionLegClasses), big.mark=" ",decimal.mark=",",nsmall=0)," a moins de ", format(round(bornes[i],precisionLegClasses), big.mark=" ",decimal.mark=",",nsmall=0)))
      }
    }
    
    x_marge <- (st_bbox(fondMaille)$xmax-st_bbox(fondMaille)$xmin)/20
    y_marge <- (st_bbox(fondMaille)$ymax-st_bbox(fondMaille)$ymin)/20
    
    if(!is.null(etiquettes))
    {
      tableEtiquettes <- table_etiquettes(fondMaille,etiquettes)
    }
    
    par(mai=c(0,0,0,0))
    plot(st_geometry(fondMaille),xlim=c(st_bbox(fondMaille)$xmin,st_bbox(fondMaille)$xmax+x_marge*3),ylim=c(st_bbox(fondMaille)$ymin,st_bbox(fondMaille)$ymax+y_marge*3),border=colBorder)
    
    if(!is.null(fondSousAnalyse))
    {
      for(i in 1:length(fondSousAnalyse))
      {
        names_fond <- names(as.data.frame(fondSousAnalyse[[i]]))
        if(any(names_fond %in% "COL")) colFond <- unique(as.data.frame(fondSousAnalyse[[i]])[,"COL"]) else colFond <- "transparent"
        if(any(names_fond %in% "BORDER")) colBorder <- unique(as.data.frame(fondSousAnalyse[[i]])[,"BORDER"]) else colBorder <- "black"
        if(any(names_fond %in% "EPAISSEUR")) epaisseur <- unique(as.data.frame(fondSousAnalyse[[i]])[,"EPAISSEUR"]) else epaisseur <- 1
        plot(st_geometry(fondSousAnalyse[[i]]),col=colFond,border=colBorder,lwd=epaisseur,add=T)
      }
    }
    
    plot(st_geometry(fondMaille),col="transparent",border=colBorder,add=T)
    
    for(i in 1:(length(bornes)-1))
    {
      suppressWarnings(plot(fond_classes[between(as.data.frame(fond_classes)[,varRatio],bornes[i+1],bornes[i]),],add=T,col=pal_classes[i],border=colBorder,lwd=1))
    }
    
    for(i in 1:length(pal_classes))
    {
      suppressWarnings(plot(st_geometry(fond_leg_classes[i,]),add=T,col=pal_classes[i],border="black",lwd=1))
      text(max(st_coordinates(fond_leg_classes[i,])[,1])+y_large/2,mean(st_coordinates(fond_leg_classes[i,])[,2]),labels=label_rectangle[i],cex=0.9,adj=0)
    }
    
    text(min(st_coordinates(fond_leg_classes[1,])[,1]),max(st_coordinates(fond_leg_classes[1,])[,2])+y_large,labels=titreLegClasses,cex=1,adj=0)
    
    plot(st_geometry(fond_ronds),border=colBorderRonds,add=T)
    
    plot(st_geometry(fond_leg_ronds),add=T,col="transparent",border="black")
    text(pts2_grand_pl[1]+1000,pts2_grand_pl[2],labels=round(max_var,precisionLegRonds),cex=0.9,adj=0)
    text(pts2_petit_pl[1]+1000,pts2_petit_pl[2],labels=round(max_var/3,precisionLegRonds),cex=0.9,adj=0)
    
    text(min(st_coordinates(fond_leg_ronds[1,])[,1]),max(st_coordinates(fond_leg_ronds[1,])[,2])+(st_bbox(fondMaille)$xmax-st_bbox(fondMaille)$xmin)/20,labels=titreLegRonds,cex=1,adj=0)
    
    if(!is.null(fondSurAnalyse))
    {
      for(i in 1:length(fondSurAnalyse))
      {
        names_fond <- names(as.data.frame(fondSurAnalyse[[i]]))
        if(any(names_fond %in% "COL")) colFond <- unique(as.data.frame(fondSurAnalyse[[i]])[,"COL"]) else colFond <- "transparent"
        if(any(names_fond %in% "BORDER")) colBorder <- unique(as.data.frame(fondSurAnalyse[[i]])[,"BORDER"]) else colBorder <- "black"
        if(any(names_fond %in% "EPAISSEUR")) epaisseur <- unique(as.data.frame(fondSurAnalyse[[i]])[,"EPAISSEUR"]) else epaisseur <- 1
        plot(st_geometry(fondSurAnalyse[[i]]),col=colFond,border=colBorder,lwd=epaisseur,add=T)
      }
    }
    
    if(!is.null(etiquettes))
    {
      for(i in 1:nrow(tableEtiquettes))
      {
        text(tableEtiquettes[i,"X"],tableEtiquettes[i,"Y"],labels=tableEtiquettes[i,"LIBELLE"],cex=tableEtiquettes[i,"TAILLE"],col=tableEtiquettes[i,"COL"],font=tableEtiquettes[i,"FONT"])
      }
    }
    
    if(titreCarte!="")
    {
      text(((st_bbox(fondMaille)$xmax+x_marge*3)-st_bbox(fondMaille)$xmin)/2,st_bbox(fondMaille)$ymax+y_marge*3,labels=titreCarte)
    }
    
    if(sourceCarte!="")
    {
      text(((st_bbox(fondMaille)$xmax+x_marge*3)-st_bbox(fondMaille)$xmin)/6,st_bbox(fondMaille)$ymin,labels=sourceCarte,cex=0.7)
    }
    
    message(simpleMessage(paste0("[INFO] Le rayon du rond le plus grand est = ",rayonRond," metres")))
    message(simpleMessage(paste0("[INFO] Le rapport du rond le plus grand est = ",rapportRond," metres")))
    message(simpleMessage(paste0("[INFO] Les coordonnees de la legende des ronds sont x = ",round(xLegRonds,2)," metres ; y = ",round(yLegRonds,2)," metres")))
    message(simpleMessage(paste0("[INFO] Les coordonnees de la legende des classes sont x = ",round(xLegClasses,2)," metres ; y = ",round(yLegClasses,2)," metres")))
    
    return(fond_ronds)
  }