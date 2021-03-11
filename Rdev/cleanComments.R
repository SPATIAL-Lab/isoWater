library(openxlsx)
library(sp)
library(rgdal)
options(stringsAsFactors = FALSE)
setwd("C:/Users/gjbowen/Dropbox/HypoMirror/WIDB/wiDB_data_staging/to_work_on")

#File to read
ifile = "Isoforensics_dbMatch_checkCountries_checkElevations.xlsx"

#Read in sites
sites = read.xlsx(ifile, sheet = "Sites")

#Replace some bad characters
sites$Site_Comments = gsub("\n", " ", sites$Site_Comments)
sites$Site_Comments = gsub("'", "", sites$Site_Comments)
sites$Site_Comments = gsub(",", ";", sites$Site_Comments)

#Condense internal whitespace
sc = sites$Site_Comments
for(i in 1:20){
  scn = gsub("  ", " ", sc)
  if(identical(scn, sc)){break()}
  sc = scn
}

#Add the edited comments back to the DF, also trim any trailing or leading ws
sites$Site_Comments = trimws(scn)

#Output file name
ofile = sub(".xlsx", "_cleanComments.xlsx", ifile)

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
