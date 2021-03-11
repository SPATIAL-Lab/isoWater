library(devtools)
install_github("SPATIAL-Lab/wiDButil", force = TRUE)
library(wiDButil)
library(openxlsx)
options(stringsAsFactors = FALSE)
setwd("C:/Users/gjbowen/Dropbox/HypoMirror/WIDB/wiDB_data_staging/to_work_on")

newSites = read.xlsx("Isoforensics_2019_processed.xlsx", 1)
newSamples = read.xlsx("Isoforensics_2019_processed.xlsx", 2)
newWI = read.xlsx("Isoforensics_2019_processed.xlsx", 3)
newSamples$Collection_Date = convertToDate(newSamples$Collection_Date)

#space to store matching records
dupes = data.frame(character(), character(), character(), character(), character(), 
                   character(), character(), character(), character(), character(),
                   character())

#cycle through sites
for(i in seq(newSites$Site_ID)){
  #using i = 1361 as test
  #get site info and all new sample and wi data for that site
  site = newSites[i, c(1, 3, 4)]
  samples = newSamples[newSamples$Site_ID == site$Site_ID, c(1, 3, 4, 7, 15)]
  wi = newWI[newWI$Sample_ID %in% samples$Sample_ID, c(1, 2, 3, 4)]
  
  if(nrow(samples) > 0 & nrow(wi) > 0){
    #match based on site location and sample date range
    if(!is.na(min(samples$Collection_Date))){
      wiDB = wiDB_data(minLat = site$Latitude - 0.1, 
                       maxLat = site$Latitude + 0.1,
                       minLong = site$Longitude - 0.1, 
                       maxLong = site$Longitude + 0.1,
                       minDate = as.character(min(samples$Collection_Date)),
                       maxDate = as.character(max(samples$Collection_Date)))      
    }else{
      wiDB = NULL
    }

    
    #cycle through retreived records
    if(!is.null(wiDB)){
      wiDBdata = wiDB[[1]]
      
      wiDBdata$Collection_Date = as.Date(wiDBdata$Collection_Date)
      
      #find new samples that are potential dupes based on same date 
      samples.dup = samples[samples$Collection_Date %in% 
                              wiDBdata$Collection_Date,]
      
      #cycle through potential duplicates
      for(j in seq(samples.dup$Sample_ID)){
        
        #pull out isotope info for new samples that are potential dups
        samples.dup.wi = wi[wi$Sample_ID == samples.dup$Sample_ID[j],]
        
        #find old samples that are date matches for this new sample
        wiDB.dup = wiDBdata[wiDBdata$Collection_Date == 
                              samples.dup$Collection_Date[j],]
  
        
        #compare isotope values for old and new
        for(k in seq(samples.dup.wi$WI_Analysis_ID)){  #new wi values
          for(l in seq(wiDB.dup$Site_ID)){  #old wi values
            #if both have d2H values
            if(!is.na(samples.dup.wi$d2H[k]) & !is.na(wiDB.dup$d2H[l])){
              #if those d2H values are equal
              if(round(samples.dup.wi$d2H[k], 1) == round(wiDB.dup$d2H[l], 1)){
                #if neither has d18O values
                if(is.na(samples.dup.wi$d18O[k]) & is.na(wiDB.dup$d18O[l])){
                  #add the new wi row to the list of duplicates
                  dupes = rbind(dupes, data.frame(samples.dup[j,], 
                                                  samples.dup.wi[k,1], 
                                                  wiDB.dup[l,c(1, 6, 7, 10, 14)]))
                #If both have d18O values
                }else if(!is.na(samples.dup.wi$d18O[k]) & 
                         !is.na(wiDB.dup$d18O[l])){
                  #if those d18O values are equal
                  if(round(samples.dup.wi$d18O[k], 1) == 
                     round(wiDB.dup$d18O[l], 1)){
                    #add the new wi row to the list of duplicates
                    dupes = rbind(dupes, data.frame(samples.dup[j,], 
                                                    samples.dup.wi[k,1], 
                                                    wiDB.dup[l,c(1, 6, 7, 10, 14)]))
                  }
                }
              }
            #if neither has d2H values
            }else if(is.na(samples.dup.wi$d2H[k]) & is.na(wiDB.dup$d2H[l])){
              #If both have d18O values
              if(!is.na(samples.dup.wi$d18O[k]) & 
                       !is.na(wiDB.dup$d18O[l])){
                #if those d18O values are equal
                if(round(samples.dup.wi$d18O[k], 1) == 
                   round(wiDB.dup$d18O[l], 1)){
                  #add the new wi row to the list of duplicates
                  dupes = rbind(dupes, data.frame(samples.dup[j,], 
                                                  samples.dup.wi[k,1], 
                                                  wiDB.dup[l,c(1, 6, 7, 10, 14)]))
                }
              }
            }
          }
        }
      }
    }    
  }
}

####end of the sites loop

#review matches
names(dupes) = c("Sample_ID_new", "Site_ID_new", "Type_new", "Date_new", 
                 "Comments_new", "WI_ID_new", "Site_ID_old", "Sample_ID_old",
                 "Type_old", "Date_old", "Comments_old")
View(dupes)

#remove duplicate analyses
newWI.clean = newWI[!(newWI$WI_Analysis_ID %in% dupes$WI_ID_new),]
#now remove orphaned samples and sites
newSamples.clean = newSamples[newSamples$Sample_ID %in% newWI.clean$Sample_ID,]
newSites.clean = newSites[newSites$Site_ID %in% newSamples.clean$Site_ID,]

#for each sample with one or more wi matches, if all matches have the same
#old Sample_ID assume that any other new wi data from that new sample 
#match to the same old sample. Remove new sample and associate new wi data
#with the old sample
for(i in unique(dupes$Sample_ID_new)){
  old.ids = unique(dupes[dupes$Sample_ID_new == i,]$Sample_ID_old)
  if(length(old.ids) == 1){
    if(nrow(newSamples.clean[newSamples.clean$Sample_ID == i,]) > 0){
      newSamples.clean = newSamples.clean[newSamples.clean$Sample_ID != i,]
    }
    if(nrow(newWI.clean[newWI.clean$Sample_ID == i,]) > 0){
      newWI.clean[newWI.clean$Sample_ID == i,]$Sample_ID = old.ids
    }
  }
}

#same for sites
for(i in unique(dupes$Site_ID_new)){
  old.ids = unique(dupes[dupes$Site_ID_new == i,]$Site_ID_old)
  if(length(old.ids) == 1){
    if(nrow(newSites.clean[newSites.clean$Site_ID == i,]) > 0){
      newSites.clean = newSites.clean[newSites.clean$Site_ID != i,]
    }
    if(nrow(newSamples.clean[newSamples.clean$Site_ID == i,]) > 0){
      newSamples.clean[newSamples.clean$Site_ID == i,]$Site_ID = old.ids
    }
  }
}

sheets = list(Sites = newSites.clean, Samples = newSamples.clean,
              Water_Isotope_Data = newWI.clean)
write.xlsx(sheets, "Isoforensics_dbMatch.xlsx")
