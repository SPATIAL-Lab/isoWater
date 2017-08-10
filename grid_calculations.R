
##### 
#Preliminaries - some/all of this need for all tranches of code below

library(sp)
library(raster)
library(rgdal)
library(ncdf4)
setwd("C:/Users/gjbowen/Dropbox/HypoMirror/WaterOrigin/gis/")

#this is the LEA projection used for all maps
gmod = raster(nrow=4036, ncol=7185, resolution=c(1000,1000), xmn=-3071826, xmx=4113174, ymn=-2486110, ymx=1549890)
pro = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
crs(gmod) = pro


##### 
#Prep precipitation layers
for(i in 1:12){
  if(i < 10){
    filler = "0"
  } else {
    filler = ""
  }
  
  #read in grids
  h_precip_raw = raster(paste0("C:/Users/gjbowen/Dropbox/Archived/Utilities/IsotopeMaps/H", filler, i, "USc.asc"))
  o_precip_raw = raster(paste0("C:/Users/gjbowen/Dropbox/Archived/Utilities/IsotopeMaps/O", filler, i, "USc.asc"))
  crs(h_precip) = "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  crs(o_precip) = "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  
  #reproject and resample
  h_precip = projectRaster(h_precip_raw, to=gmod)
  o_precip = projectRaster(o_precip_raw, to=gmod)
  
  #write out as .tif
  writeRaster(h_precip, paste0("h", filler, i, ".tif"), overwrite=TRUE)
  writeRaster(o_precip, paste0("o", filler, i, ".tif"), overwrite=TRUE)

  #cleanup after raster  
  tfd = dirname(rasterTmpFile())
  do.call(file.remove, list(list.files(tfd, full.names = TRUE)))
}


##### 
#Monthly calculations

#set theta values used in calculation of kinetic fractionation factors
theta_l = 0.5
theta_s = 1

for(i in 1:12){ #iterate over months
  if(i < 10){
    yn = "07"
    filler = "0"
    bndinx = 336 #Dec 2006 is band 324
  } else {
    yn = "06"
    filler = ""
    bndinx = 324 #Dec 2005 is band 324
  }

  #read precipitation isoscapes, correct projection info
  h_precip = raster(paste0("h", filler, i, ".tif"))/1000
  o_precip = raster(paste0("o", filler, i, ".tif"))/1000

  #read NARR airtemp, calculate alpha values using Horita and Weslowski 1994 eqns, reproject and resample  
  atemp = raster("NARR/air.2m.mon.mean.nc", band = bndinx + i)
  halpha_raw = exp((1158.8*(atemp^3 / 10^9) - 1620.1*(atemp^2 / 10^6) + 794.84*(atemp / 10^3) - 161.04 + 2.9992*(10^9 / atemp^3)) / 1000)
  oalpha_raw = exp((-7.685 + 6.7123*(10^3/atemp) - 1.6664*(10^6/atemp^2) + 0.35041*(10^9/atemp^3)) / 1000)
  halpha = projectRaster(halpha_raw, to=gmod)
  oalpha = projectRaster(oalpha_raw, to=gmod)
  
  #read in NARR relative humidity, calculate kinetic fractionation factors, reproject and resample  
  rhum_tmp = raster("NARR/rhum.2m.mon.mean.nc", band = bndinx + i)/100
  ek_h_tmp = (1-rhum_tmp)*25.0/1000
  ek_o_tmp = (1-rhum_tmp)*28.6/1000
  rhum = projectRaster(rhum_tmp, to=gmod)
  ek_h = projectRaster(ek_h_tmp, to=gmod)
  ek_o = projectRaster(ek_o_tmp, to=gmod)

  #calcualte epsilon values
  heps = (halpha - 1)
  oeps = (oalpha - 1)
  
  #atmospheric vapor isotope ratios assuming equilibrium
  h_atmos = (h_precip - heps)/halpha
  o_atmos = (o_precip - oeps)/oalpha
  
  #slope calculation for lakes, using equation 7 from Gat and Bowser 1991
  num_lakes = (h_atmos - h_precip) + (heps + ek_h * theta_l) / rhum
  denom_lakes = (o_atmos - o_precip) + (oeps + ek_o * theta_l) / rhum
  slope_lakes = num_lakes/denom_lakes

  #slope calculation for soils
  num_soils = (h_atmos - h_precip) + (heps + ek_h * theta_s) / rhum
  denom_soils = (o_atmos - o_precip) + (oeps + ek_o * theta_s) / rhum
  slope_soils = num_soils/denom_soils

  #write output
  writeRaster(slope_lakes, paste0("elsl_", yn, filler, i, ".tif"), overwrite=TRUE)
  writeRaster(slope_soils, paste0("elss_", yn, filler, i, ".tif"), overwrite=TRUE)
  
  #cleanup after raster  
  tfd = dirname(rasterTmpFile())
  do.call(file.remove, list(list.files(tfd, full.names = TRUE)))
}

##### 
#Annual aggregation

for(i in 1:12){ #loop over months
  if(i < 10){
    yn = "07"  #year code
    filler = "0"  #filler for 1 digit months
    bndinx = 336 #Dec 2006 is band 324
  } else {
    yn = "06"
    filler = ""
    bndinx = 324 #Dec 2005 is band 324
  }
  
  #read in slopes values
  slope_lakes = raster(paste0("elsl_", yn, filler, i, ".tif"))
  slope_soils = raster(paste0("elss_", yn, filler, i, ".tif"))

  #read in NARR evaporation rates, reproject and rescale
  evap_tmp = raster("NARR/evap.mon.mean.nc", band=bndinx+i)
  evap_tmp = max(evap_tmp, 0.001) #mask out negative and zero values
  evap = projectRaster(evap_tmp, to=gmod)
  
  #running sum of slope and evap values
  if(i == 1){ #initialize rasters with first month's data
    slp = slope_lakes * evap
    ssp = slope_soils * evap
    ep = evap
  } else{ #add in data from subsequent months
    slp = slp + slope_lakes * evap
    ssp = ssp + slope_soils * evap
    ep = ep + evap
  }
}

#unweight by total evaporation
sl = slp / ep
ss = ssp / ep

#write annualized evap slope output
writeRaster(sl, "elsl.tif", overwrite=TRUE)
writeRaster(ss, "elss.tif", overwrite=TRUE)
writeRaster(slp, "elslp.tif", overwrite=TRUE)
writeRaster(ssp, "elssp.tif", overwrite=TRUE)
writeRaster(ep, "evap.tif", overwrite=TRUE)

#cleanup after raster
tfd = dirname(rasterTmpFile())
do.call(file.remove, list(list.files(tfd, full.names = TRUE)))


##### 
#Do precipitation isoscape weighting

for(i in 1:12){ #loop over months
  if(i < 10){
    bndinx = 336 #Dec 2006 is band 324
    filler = "0"  #filler for 1 digit months
  } else {
    bndinx = 324 #Dec 2005 is band 324
    filler = ""
  }
  
  #read precip isotope layer, set projection
  h_precip = raster(paste0("h", filler, i, ".tif"))/1000
  o_precip = raster(paste0("o", filler, i, ".tif"))/1000

  #read NARR precipitation and evaporation grids, reproject and resample
  precip_tmp = raster("NARR/apcp.mon.mean.nc", band=bndinx+i)
  precip_tmp = max(precip_tmp, 0) #mask out negative values
  precip = projectRaster(precip_tmp, to=gmod)
  
  evap_tmp = raster("NARR/evap.mon.mean.nc", band=bndinx+i)
  evap_tmp = max(evap_tmp, 0) #mask out negative values
  evap = projectRaster(evap_tmp, to=gmod)
  
  pme = precip - evap
  pme = max(pme, 0.001)
  
  #running sums of summer, winter and mean annual P-E and isotope-weighted P-E 
  if(i == 1){ #this if/then does the annual sums
    pmes_ann = pme
    hqs_ann = pme*h_precip
    oqs_ann = pme*o_precip
  } else{
    pmes_ann = pmes_ann + pme
    hqs_ann = hqs_ann + pme*h_precip
    oqs_ann = oqs_ann + pme*o_precip
  }

  if(i > 3 & i < 10){ #this if/then does the summer sums
    if(i == 4){ 
      pmes_sum = pme
      hqs_sum = pme*h_precip
      oqs_sum = pme*o_precip
    } else{
      pmes_sum = pmes_sum + pme
      hqs_sum = hqs_sum + pme*h_precip
      oqs_sum = oqs_sum + pme*o_precip
    }
  }
  
  if(i < 4 | i > 9){ #this if/then does the winter sums
    if(i == 1){ 
      pmes_win = pme
      hqs_win = pme*h_precip
      oqs_win = pme*o_precip
    } else{
      pmes_win = pmes_win + pme
      hqs_win = hqs_win + pme*h_precip
      oqs_win = oqs_win + pme*o_precip
    }
  }
}

#unweight by P-E
hq_ann = hqs_ann / pmes_ann
oq_ann = oqs_ann / pmes_ann
hq_sum = hqs_sum / pmes_sum
oq_sum = oqs_sum / pmes_sum
hq_win = hqs_win / pmes_win
oq_win = oqs_win / pmes_win

#write weighted and unweighted grids; rescale weighted grids because TauDEM FA can't handle large numbers
writeRaster(pmes_ann/100000, "qs_ann.tif", overwrite=TRUE)
writeRaster(hqs_ann/100000, "hqs_ann.tif", overwrite=TRUE)
writeRaster(oqs_ann/100000, "oqs_ann.tif", overwrite=TRUE)
writeRaster(hq_ann, "hq_ann.tif", overwrite=TRUE)
writeRaster(oq_ann, "oq_ann.tif", overwrite=TRUE)

writeRaster(pmes_sum/100000, "qs_sum.tif", overwrite=TRUE)
writeRaster(hqs_sum/100000, "hqs_sum.tif", overwrite=TRUE)
writeRaster(oqs_sum/100000, "oqs_sum.tif", overwrite=TRUE)
writeRaster(hq_sum, "hq_sum.tif", overwrite=TRUE)
writeRaster(oq_sum, "oq_sum.tif", overwrite=TRUE)

writeRaster(pmes_win/100000, "qs_win.tif", overwrite=TRUE)
writeRaster(hqs_win/100000, "hqs_win.tif", overwrite=TRUE)
writeRaster(oqs_win/100000, "oqs_win.tif", overwrite=TRUE)
writeRaster(hq_win, "hq_win.tif", overwrite=TRUE)
writeRaster(oq_win, "oq_win.tif", overwrite=TRUE)

#cleanup after raster
tfd = dirname(rasterTmpFile())
do.call(file.remove, list(list.files(tfd, full.names = TRUE)))


##### 
#Initial DEM preparation

#prep DEM - this is an extract from hydro1k for the contiguous USA and buffer area, in unprojected WGS84
dem_tmp = raster("dem_usa")
dem = projectRaster(dem_tmp, to=gmod)
writeRaster(dem, "dem_tau.tif", NAflag=-9999)

#fill sinks
system("mpiexec -n 8 pitremove -z dem_tau.tif -fel dem_tau_fill.tif")
tmprast = raster("dem_tau_fill.tif")
plot(tmprast)

#make flow direction
system("mpiexec -n 8 D8Flowdir -fel dem_tau_fill.tif -p fd_tau.tif -sd8 slopes_tau.tif")

#unweighted flow accumulation
system("mpiexec -n 8 AreaD8 -p fd_tau.tif -ad8 fa_outs/fa_uw.tif")

#make stream network
system("mpiexec -n 8 Threshold -ssa fa_outs/fa_uw.tif -src tau_streams.tif -thresh 25")


##### 
#Snap EPA sites to streams

#first project the raw site data
sites = read.csv("epa_data.csv")
coordinates(sites) = ~Longitude+Latitude
proj4string(sites) = "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
sites_proj = spTransform(sites, CRS = pro)
writeOGR(sites_proj, ".", "epa_proj", driver="ESRI Shapefile", overwrite_layer = TRUE)

#now read in layer of lake polygons for EPA lakes and add their IDs to site data layer
nhd_poly = readOGR("NHD_selected_lake.shp")
siteval = over(sites_proj, nhd_poly[, "OBJECTID"]) #spatial join, list of OBJECTIDs ordered by sites
sites_proj$OBJECTID = siteval$OBJECTID

#find max FA value within each polygon
fa = raster("fa_outs/fa_uw.tif")
max_fa = extract(fa, nhd_poly, fun = max, na.rm = T)  #extract is flexible! use here w/ "max" to summarize
max_fa[max_fa < 25] = NA  #if no streams w/ fa of 25 cells or more in polygon we don't care
nhd_poly$max_fa = max_fa  #add information back to lake polygon layer

#now turn max FA info into raster so we can find cells w/ matching values
max_fa_rast = gmod
max_fa_rast = rasterize(nhd_poly, max_fa_rast, "max_fa")  #raster holding max_fa values from polygons
msk = fa == max_fa_rast     #find matching cells
msk = calc(msk, fun=function(x){ x[x == 0] = NA; return(x)} )   #set no match cells to NA

#make raster with lake ID values and mask it to keep only max FA cells
id_rast = gmod
id_rast = rasterize(nhd_poly, id_rast, "OBJECTID")
id_rast = mask(id_rast, msk)

#turn that raster into a SpatialPointsDataFrame
reloc = data.frame(rasterToPoints(id_rast))
colnames(reloc)[3] = "OBJECTID" #reinstate column name to keep it clean
coordinates(reloc) = ~x+y
proj4string(reloc) = pro
writeOGR(reloc, ".", "reloc/lake_points.shp", overwrite_layer = TRUE, driver="ESRI Shapefile")

#move lakes to outflow points of lake polygons where available
for(i in 1:nrow(sites_proj)){
  for(j in 1:nrow(reloc)){
    if(as.integer(sites_proj$OBJECTID[i]) == as.integer(reloc$OBJECTID[j])){
      sites_proj@coords[i,] = reloc@coords[j,]
      print(paste("updated", sites_proj$OBJECTID[i], "with", reloc$OBJECTID[j]))
    }
  }
}
writeOGR(sites_proj, ".", "epa_proj_reloc", driver="ESRI Shapefile", overwrite_layer = TRUE)

#now run TauDEM snap
system("mpiexec -n 8 moveoutletstostreams -p fd_tau.tif -src tau_streams.tif -o epa_proj_reloc.shp -om tau_epa.shp")


##### 
#Flow accumulations for isotopes and evap lines

#annual precip isotopes * q and q
system("mpiexec -n 8 AreaD8 -p fd_tau.tif -wg qs_ann.tif -ad8 fa_outs/fa_qs_ann.tif")
system("mpiexec -n 8 AreaD8 -p fd_tau.tif -wg hqs_ann.tif -ad8 fa_outs/fa_hqs_ann.tif")
system("mpiexec -n 8 AreaD8 -p fd_tau.tif -wg oqs_ann.tif -ad8 fa_outs/fa_oqs_ann.tif")

#winter precip isotopes * q and q
system("mpiexec -n 8 AreaD8 -p fd_tau.tif -wg qs_win.tif -ad8 fa_outs/fa_qs_win.tif")
system("mpiexec -n 8 AreaD8 -p fd_tau.tif -wg hqs_win.tif -ad8 fa_outs/fa_hqs_win.tif")
system("mpiexec -n 8 AreaD8 -p fd_tau.tif -wg oqs_win.tif -ad8 fa_outs/fa_oqs_win.tif")

#annual precip isotopes * q and q
system("mpiexec -n 8 AreaD8 -p fd_tau.tif -wg qs_sum.tif -ad8 fa_outs/fa_qs_sum.tif")
system("mpiexec -n 8 AreaD8 -p fd_tau.tif -wg hqs_sum.tif -ad8 fa_outs/fa_hqs_sum.tif")
system("mpiexec -n 8 AreaD8 -p fd_tau.tif -wg oqs_sum.tif -ad8 fa_outs/fa_oqs_sum.tif")

#evaporation lines and evaporation
system("mpiexec -n 8 AreaD8 -p fd_tau.tif -wg elslp.tif -ad8 fa_outs/fa_elslp.tif")
system("mpiexec -n 8 AreaD8 -p fd_tau.tif -wg elssp.tif -ad8 fa_outs/fa_elssp.tif")
system("mpiexec -n 8 AreaD8 -p fd_tau.tif -wg evap.tif -ad8 fa_outs/fa_evap.tif")

#unweight isotope accumulations, first annual
hqrast = raster("fa_outs/fa_hqs_ann.tif")
oqrast = raster("fa_outs/fa_oqs_ann.tif")
qrast = raster("fa_outs/fa_qs_ann.tif")
writeRaster(hqrast/qrast*1000, "h_ann.tif", overwrite=TRUE)
writeRaster(oqrast/qrast*1000, "o_ann.tif", overwrite=TRUE)

#then winter
hqrast = raster("fa_outs/fa_hqs_win.tif")
oqrast = raster("fa_outs/fa_oqs_win.tif")
qrast = raster("fa_outs/fa_qs_win.tif")
writeRaster(hqrast/qrast*1000, "h_win.tif", overwrite=TRUE)
writeRaster(oqrast/qrast*1000, "o_win.tif", overwrite=TRUE)

#then summer
hqrast = raster("fa_outs/fa_hqs_sum.tif")
oqrast = raster("fa_outs/fa_oqs_sum.tif")
qrast = raster("fa_outs/fa_qs_sum.tif")
writeRaster(hqrast/qrast*1000, "h_sum.tif", overwrite=TRUE)
writeRaster(oqrast/qrast*1000, "o_sum.tif", overwrite=TRUE)

#now unweight evap line slopes
elrast = raster("fa_outs/fa_elslp.tif")
esrast = raster("fa_outs/fa_elssp.tif")
erast = raster("fa_outs/fa_evap.tif")
writeRaster(elrast/erast, "el.tif", overwrite=TRUE)
writeRaster(esrast/erast, "es.tif", overwrite=TRUE)

#cleanup after raster
tfd = dirname(rasterTmpFile())
do.call(file.remove, list(list.files(tfd, full.names = TRUE)))


##### 
#Extract all this information to sampling sites!

#read files
h_ann = raster("h_ann.tif")
o_ann = raster("o_ann.tif")
q_ann = raster("fa_outs/fa_qs_ann.tif")
h_win = raster("h_win.tif")
o_win = raster("o_win.tif")
q_win = raster("fa_outs/fa_qs_win.tif")
h_sum = raster("h_sum.tif")
o_sum = raster("o_sum.tif")
q_sum = raster("fa_outs/fa_qs_sum.tif")
elloc = raster("elsl.tif")  #local lake EL for lakes data
el = raster("el.tif") #catchment ELs for rivers
es = raster("es.tif")

#stack and extract for lakes
rs = stack(h_ann, o_ann, q_ann, h_win, o_win, q_win, h_sum, o_sum, q_sum, elloc, el, es)
sites_proj = readOGR("tau_epa.shp")
rasvals = extract(rs, sites_proj)

#bundle and write
output = data.frame(sites_proj@data, rasvals)
write.csv(output, "lakes_w_extract.csv")
