library(openxlsx)
library(sp)
library(rgdal)
options(stringsAsFactors = FALSE)
setwd("C:/Users/gjbowen/Dropbox/HypoMirror/WIDB/wiDB_data_staging/to_work_on")

#File to read
ifile = "Isoforensics_dbMatch.xlsx"

#Read in sites
sites = read.xlsx(ifile, sheet = "Sites")

#Read in countries, use high res for initial matching
cty = readOGR("../aux_data/TM_WORLD_BORDERS-0.3.shp")

#Make sites spatial; assumes 1) all sites have lat/lon, 2) lat/lon are WGS84
sites.sp = SpatialPointsDataFrame(data.frame(sites$Longitude, sites$Latitude),
                                  data = sites, proj4string = crs(cty))

#Spatial intersect gets map values for each point
mcs = over(sites.sp, cty)
#Use this to track whether the site has already been resolved, allowing
#stop/restart for developers
mcs$RES = rep(FALSE)

#Count the number of missing or mis-matched values
ct = 0
for(i in seq_along(sites.sp)){
  if(is.na(sites.sp$Country[i]) | is.na(mcs$ISO2[i])){
    ct = ct + 1
  }else if(sites.sp$Country[i] != mcs$ISO2[i]){
    ct = ct + 1
  }
}

#Does the user want to review the sites?
g = readline(paste(ct, "sites to review. Continue (y/n)? "))

#If so, here we go...
if(g == "y" | g == "Y"){
  #Read in countries, use low res for map display
  cty = readOGR("../aux_data/TM_WORLD_BORDERS_SIMPL-0.3.shp")
  
  for(i in seq_along(sites.sp)){
    #Grab the map code for the site
    map.code = mcs$ISO2[i]
    #If there's no map code for the coordinates set to "MISSING"
    if(is.na(map.code)){map.code = "MISSING"}
    
    #If there is a country code for the site or
    #If the country code doesn't match the map
    if(!mcs$RES[i] & (is.na(sites.sp$Country[i]) | 
       sites.sp$Country[i] != map.code)){
      cat("Point or country may be incorrect\n")
      print(sites.sp@data[i, c(2,3,4,7,8,9)])
      
      #If the site's country code is not ISO2 complient
      if(!(sites.sp$Country[i] %in% cty$ISO2)){
        #Plot point and matching country
        plot(cty, xlim=c(max(sites.sp$Longitude[i] - 30, -180),
                         min(sites.sp$Longitude[i] + 30, 180)),
             ylim=c(max(sites.sp$Latitude[i] - 30, -90), 
                    min(sites.sp$Latitude[i] + 30, 90)))
        points(sites.sp[i,], pch=21, bg="red")
        
        #If there is no map code for location
        if(map.code == "MISSING"){
          cat("Unknown coutry code. No new code available.\n")
          f = readline("Delete (d), type new code (t), or return to keep. ")
          if(f == "d"){
            sites.sp$Country[i] = NA
            annotate(sites.sp$Site_Comments[i], "unknown country code deleted")
          }else if(f == "t"){
            c = readline("Type 2-letter ISO code: ")
            sites.sp$Country[i] = c
            annotate(sites.sp$Site_Comments[i], "country code manually edited")
          }
          
          #If there is a map code for the location
        }else{
          cat("Unknown country code. Replace with", map.code, "?\n")
          f = readline("Replace (r), type new code (t), or return to keep. ")
          if(f == "r"){
            sites.sp$Country[i] = map.code
            sites.sp$Site_Comments[i] = annotate(sites.sp$Site_Comments[i],
                                                 "country code updated based on lat/lon coordinates")
          }else if(f == "t"){
            c = readline("Type 2-letter ISO code: ")
            sites.sp$Country[i] = c
            annotate(sites.sp$Site_Comments[i], "country code manually edited")
          }
        }
        
      #If the code is complient but doesn't match  
      }else{
        #Report map code
        cat("Map code is", map.code, "\n")
        #Report country name equivalent to site code
        cind = match(sites.sp$Country[i], cty$ISO2)
        if(!is.na(cind)){
          cat("Country name for site code is", cty$NAME[cind], "\n")
        }
        #Get coordinates of country matching code for plotting
        cty.bb = bbox(cty[cty$ISO2 == sites.sp$Country[i],])
        #Plot point and matching country
        plot(cty, xlim=c(max(min(cty.bb[1,1], sites.sp$Longitude[i], 
                             sites.sp$Longitude[i] - 20), -180),
                         min(max(cty.bb[1,2], sites.sp$Longitude[i], 
                             sites.sp$Longitude[i] + 20), 180)),
             ylim=c(max(min(cty.bb[2,1], sites.sp$Latitude[i],
                        sites.sp$Latitude[i] - 20), -90), 
                    min(max(cty.bb[2,2], sites.sp$Latitude[i], 
                        sites.sp$Latitude[i] + 20), 90)))
        plot(cty[cty$ISO2 == sites.sp$Country[i],], col="grey", add=TRUE)
        points(sites.sp[i,], pch=21, bg="red")
        #Prompt for changes
        cat("Fix common problems: 
             invert latitude (a), 
             invert longitude (o), 
             invert both (b), 
             swap lat/lon (s), 
             type lat/lon (t),
             type country code (c),
             use country code from map (u)?\n")
        f = readline("Selection (or return for no action): ")
        #If latitude is inverted fix and annotate
        if(f == "a"){
          sites.sp$Latitude[i] = -sites.sp$Latitude[i]
          sites.sp$Site_Comments[i] = annotate(sites.sp$Site_Comments[i],
                                               "latitude sign inverted")
          #If longitude is inverted fix and annotate
        }else if(f == "o"){
          sites.sp$Longitude[i] = -sites$Longitude[i]
          sites.sp$Site_Comments[i] = annotate(sites.sp$Site_Comments[i],
                                               "longitude sign inverted")
        }else if(f == "b"){
          sites.sp$Latitude[i] = -sites.sp$Latitude[i]
          sites.sp$Longitude[i] = -sites$Longitude[i]
          sites.sp$Site_Comments[i] = annotate(sites.sp$Site_Comments[i],
                                               "latitude and longitude signs inverted")
          #If both are inverted fix and annotate
        }else if(f == "s"){
          sites.sp$Latitude[i] = sites.sp$Longitude[i]
          sites.sp$Longitude[i] = sites$Latitude[i]
          sites.sp$Site_Comments[i] = annotate(sites.sp$Site_Comments[i],
                                               "latitude and longitude switched")
          #If user wants to manually enter values
        }else if(f == "t"){
          lat = readline("Type latitude (S negative) or return to keep value: ")
          lon = readline("Type longidude (W negative) or return to keep value: ")
          if(lat != ""){
            sites.sp$Latitude[i] = as.numeric(lat)
          }
          if(lon != ""){
            sites.sp$Longitude[i] = as.numeric(lon)
          }
          sites.sp$Site_Comments[i] = annotate(sites.sp$Site_Comments[i],
                                               "latitude and/or longitude manually edited")
          #If country code is wrong fix it manually
        }else if(f == "c"){
          c = readline("Type new country code: ")
          sites.sp$Country[i] = c
          sites.sp$Site_Comments[i] = annotate(sites.sp$Site_Comments[i],
                                               "country code manually edited")
          #If country code is wrong fix it by using map value
        }else if(f == "u"){
          sites.sp$Country[i] = map.code
          sites.sp$Site_Comments[i] = annotate(sites.sp$Site_Comments[i],
                                               "country code updated based on lat/lon coordinates")
        }    
      }
    }
    mcs$RES[i] = TRUE
  }
}

#Output file name
ofile = sub(".xlsx", "_checkCountries.xlsx", ifile)

#Get the input file
s = getSheetNames(ifile)
shts = list()
for(i in seq(s)){
  shts[[i]] = read.xlsx(ifile, i)
}
names(shts) = s

#Replace the old site data with updated records
shts$Sites = sites.sp@data

#Write output
write.xlsx(shts, ofile)

#combines old and new comments
annotate = function(old, new){
  if(is.na(old)){
    return(new)
  }else{
    return(paste(old, new, sep = "; "))
  }
}

