library(openxlsx)
library(httr)
library(jsonlite)
options(stringsAsFactors = FALSE)
setwd("C:/Users/gjbowen/Dropbox/HypoMirror/WIDB/wiDB_data_staging/to_work_on")

#Free elevation API: https://elevation-api.io/; limit 2000 req/min from
#5km DEM w/ key
api_root = "https://elevation-api.io/api/elevation?points="
api_key = "&key=kVzeefC0JPIIcfaVaC2r-qofepiJe3"

#File to read
ifile = "Isoforensics_dbMatch_checkCountries.xlsx"

#Read in sites
sites = read.xlsx(ifile, sheet = "Sites")

#Space for values
elevs = data.frame(character(), numeric())

#Chunks of 10 sites; 10 points allowed per GET
for(i in 1:(ceiling(nrow(sites)/10))){
  #Subset sites for this chunk
  sites.sub = sites[((i-1)*10+1):min((i*10), nrow(sites)),]

  #Space for point coordinates string
  pts = character()
  
  #Build character string of coordinates
  for(j in seq(sites.sub$Site_ID)){
    pts = paste0(pts, "(", sites.sub$Latitude[j], ",", sites.sub$Longitude[j], "),")
  }
  #Remove final comma
  pts = substr(pts, 1, nchar(pts)-1)
  
  #Full GET request
  api_call = paste0(api_root, pts, api_key)
  
  #Make the request
  e = GET(api_call)
  
  #Check status
  if(e$status_code != 200){stop(paste("Error code", e$status_code, 
                                      "returned by server"))}
  #Extract data
  ep = parse_json(e)
  
  
  #Add Site_ID and elevation to data frame of results
  for(j in seq(sites.sub$Site_ID)){
    elevs = rbind(elevs, c(sites.sub$Site_ID[j], ep$elevations[[j]]$elevation))
  }
}

names(elevs) = c("Site_ID", "Elev")
elevs$Elev = as.numeric(elevs$Elev)

elevs.sub = elevs[elevs$Elev / sites$Elevation_mabsl < 0.35 &
                    elevs$Elev / sites$Elevation_mabsl > 0.25,]
sites.sub = sites[elevs$Elev / sites$Elevation_mabsl < 0.35 &
                    elevs$Elev / sites$Elevation_mabsl > 0.25,]

elevs.sub = elevs.sub[!is.na(elevs.sub$Site_ID),]
sites.sub = sites.sub[!is.na(sites.sub$Site_ID),]

#First check whether it looks like all elevations are in feet
#If so update all non-NA elevations and annotate
if(nrow(sites.sub) > 0.75 * nrow(sites)){
  yn = readline("It looks like elevation is in feet, not meters. Convert to 
                meters (y/n)? ")
  if(yn == "y" | yn == "Y"){
    sites$Elevation_mabsl = sites$Elevation_mabsl * 0.304
    for(i in seq(sites$Site_ID)){
      if(!is.na(sites$Elevation_mabsl)){
        if(!is.na(sites$Site_Comments[i])){
          sites$Site_Comments[i] = paste0(sites$Site_Comments[i], "; elevation
                                        converted from feet to meters")
        }else{
          sites$Site_Comments[i] = "elevation converted from feet to meters"
        }
      }
    }
  }
#review and update requested elevations
#need to annotate which ones were changed  
}else{
  for(i in seq(sites.sub$Site_ID)){
    print(sites.sub[i,c(2,3,4,5,9,10)])
    yn = readline("Elevation may be in feet, convert to meters (y/n)? ")
    if(yn == "y"){
      sites[sites$Site_ID == sites.sub$Site_ID[i],]$Elevation_mabsl = 
        sites.sub$Elevation_mabsl[i] * 0.304
      sites[sites$Site_ID == sites.sub$Site_ID[i],]$Site_Comments = 
        annotate(sites[sites$Site_ID == sites.sub$Site_ID[i],]$Site_Comments,
                 "elevation converted from feet to meters")
    }
  }
}

elevs.sub = elevs[abs(elevs$Elev - sites$Elevation_mabsl) > 500,]
sites.sub = sites[abs(elevs$Elev - sites$Elevation_mabsl) > 500,]

elevs.sub = elevs.sub[!is.na(elevs.sub$Site_ID),]
sites.sub = sites.sub[!is.na(sites.sub$Site_ID),]

for(i in seq(sites.sub$Site_ID)){
  print(sites.sub[i,c(1,2,3,4,5,9,10)])
  print(elevs.sub[i,])
  yn = readline("Elevation may be wrong, replace from DEM (y/n)? ")
  if(yn == "y" | yn == "Y"){
    sites[sites$Site_ID == sites.sub$Site_ID[i],]$Elevation_mabsl = 
      sites.sub$Elevation_mabsl[i] * 0.304
    sites[sites$Site_ID == sites.sub$Site_ID[i],]$Site_Comments =
      annotate(sites[sites$Site_ID == sites.sub$Site_ID[i],]$Site_Comments,
               "elevation replaced from 5km DEM")
  }
}

#Output file name
ofile = sub(".xlsx", "_checkElevations.xlsx", ifile)

#Get the input file
s = getSheetNames(ifile)
shts = list()
for(i in seq(s)){
  shts[[i]] = read.xlsx(ifile, i)
}
names(shts) = s

#Replace the old site data with updated records
shts$Sites = sites

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