#####
#Validate query input
#####
wiDB_validate = function(minLat, maxLat, minLong, maxLong, minElev, maxElev,
                         minDate, maxDate, countries, states, types, projects){
  
  qStr = ""
  
  if(!is.null(minLat)){
    if(class(minLat) != "numeric"){stop("minLat must be numeric")}
    qStr = paste0(qStr, "&minLat=", minLat)
  }
  if(!is.null(maxLat)){
    if(class(maxLat) != "numeric"){stop("maxLat must be numeric")}
    qStr = paste0(qStr, "&maxLat=", maxLat)
  }
  if(!is.null(minLong)){
    if(class(minLong) != "numeric"){stop("minLong must be numeric")}
    qStr = paste0(qStr, "&minLong=", minLong)
  }
  if(!is.null(maxLong)){
    if(class(maxLong) != "numeric"){stop("maxLong must be numeric")}
    qStr = paste0(qStr, "&maxLong=", maxLong)
  }
  if(!is.null(minElev)){
    if(class(minElev) != "numeric"){stop("minElev must be numeric")}
    qStr = paste0(qStr, "&minElev=", minElev)
  } 
  if(!is.null(maxElev)){
    if(class(maxElev) != "numeric"){stop("maxElev must be numeric")}
    qStr = paste0(qStr, "&maxElev=", maxElev)
  }
  if(!is.null(minDate)){
    if(class(minDate) != "character"){stop("minDate must be string")}
    td = c(as.numeric(substr(minDate, 1, 4)), as.numeric(substr(minDate, 6, 7)),
           as.numeric(substr(minDate, 9, 10)))
    if(NA %in% td){stop("minDate format must be YYYY-MM-DD")}
    qStr = paste0(qStr, "&minDate=", minDate)
  }
  if(!is.null(maxDate)){
    if(class(maxDate) != "character"){stop("maxDate must be string")}
    td = c(as.numeric(substr(maxDate, 1, 4)), as.numeric(substr(maxDate, 6, 7)),
           as.numeric(substr(maxDate, 9, 10)))
    if(NA %in% td){stop("maxDate format must be YYYY-MM-DD")}
    qStr = paste0(qStr, "&maxDate=", maxDate)
  }
  if(!is.null(countries)){
    if(class(countries) != "character"){stop("countries must be string")}
    countries = gsub("  ", "", countries)
    countries = gsub(" ", "", countries)
    if(length(countries > 1)){
      countries = paste0(countries, collapse = ",")
    }
    qStr = paste0(qStr, "&countries=", countries)
  }
  if(!is.null(states)){
    if(class(states) != "character"){stop("states must be string")}
    states = gsub("  ", "", states)
    states = gsub(" ", "", states)
    if(length(states > 1)){
      states = paste0(states, collapse = ",")
    }
    qStr = paste0(qStr, "&states=", states)
  }
  if(!is.null(types)){
    if(class(types) != "character"){stop("types must be string")}
    types = gsub("  ", "", types)
    types = gsub(" ", "", types)
    if(length(types > 1)){
      types = paste0(types, collapse = ",")
    }
    qStr = paste0(qStr, "&types=", types)
  }
  if(!is.null(projects)){
    if(class(projects) != "character"){stop("projects must be string")}
    projects = gsub("  ", "", projects)
    projects = gsub(" ", "", projects)
    if(length(projects > 1)){
      projects = paste0(projects, collapse = ",")
    }
    qStr = paste0(qStr, "&projects=", projects)
  }
  
  if(nchar(qStr) == 0){stop("No query arguments provided")}
  
  qStr = paste0("?", substr(qStr, 2, nchar(qStr))) 
  
  return(qStr)
}

#####
#Find sites
#####
wiDB_sites = function(minLat = NULL, maxLat = NULL, minLong = NULL, maxLong = NULL,
                      minElev = NULL, maxElev = NULL, minDate = NULL, maxDate = NULL,
                      countries = NULL, states = NULL, types = NULL, projects = NULL){

  qStr = wiDB_validate(minLat, maxLat, minLong, maxLong, minElev, maxElev,
                       minDate, maxDate, countries, states, types, projects)
  
  baseStr = "https://wateriso.utah.edu/api/v1/sites.php"
  q = paste0(baseStr, qStr)
  ua = user_agent("https://github.com/SPATIAL-Lab/isoWater")
  d = tryCatch({
    GET(q, ua)
    },
    error = function(x){
      message(conditionMessage(x))
      return(NULL)
    },
    warning = function(x){
      message(conditionMessage(x))
      return(NULL)
    }
  )

  if(is.null(d)){return()}
  
  if(d$status_code != 200){
    message(paste("Request returned error code", d$status_code))
    return(NULL)
  }

  resp = fromJSON(content(d, as = "text", encoding = "UTF-8"))
  
  if(length(resp$sites) == 0){
    warning("No sites returned")
    return(NULL)
  }
  
  return(resp$sites)
}

#####
#Obtain data
#####
wiDB_data = function(minLat = NULL, maxLat = NULL, minLong = NULL, maxLong = NULL,
                     minElev = NULL, maxElev = NULL, minDate = NULL, maxDate = NULL,
                     countries = NULL, states = NULL, types = NULL, projects = NULL, 
                     fields = NULL, tmpdir = tempdir(), clean = TRUE){
  
  qStr = wiDB_validate(minLat, maxLat, minLong, maxLong, minElev, maxElev,
                       minDate, maxDate, countries, states, types, projects)
  
  if(!dir.exists(tmpdir)){
    warning("Directory doesn't exist, trying to create")
    dir.create(tmpdir)
    if(!dir.exists(tmpdir)){stop("Unable to create directory")}
  }
  
  if(class(clean) != "logical"){stop("clean must be TRUE/FALSE")}
  
  flist = c("Site_ID", "Site_Name", "Latitude", "Longitude", "Elevation", 
            "Sample_ID", "Type", "Start_Date", "Start_Time_Zone", 
            "Collection_Date", "Collection_Time_Zone", "Phase", 
            "Depth_meters", "Sample_Comments", "d2H", "d18O", 
            "d2H_Analytical_SD", "d18O_Analytical_SD", "WI_Analysis_Source", 
            "Project_ID")
  
  if(!is.null(fields)){
    if(class(fields) != "character"){stop("fields must be a string")}
    fields = gsub("  ", "", fields)
    fields = gsub(" ", "", fields)
    fels = strsplit(fields, ",")
    fels = fels[[1]]
    for(i in 1:length(fels)){
      if(!(fels[i] %in% flist)){stop(paste("Value", i, "in fields is not a valid field name"))}
    }
    qStr = paste0(qStr, "&return=", fields)
  }
  
  baseStr = "https://wateriso.utah.edu/api/v1/download.php"
  q = paste0(baseStr, qStr)
  ua = user_agent("https://github.com/SPATIAL-Lab/isoWater")

  g = tryCatch({
    GET(q, ua)
  },
  error = function(x){
    message(conditionMessage(x))
    return(NULL)
  },
  warning = function(x){
    message(conditionMessage(x))
    return(NULL)
  }
  )
  
  if(is.null(g)){return()}
  
  if(g$status_code != 200){
    message(paste("Request returned error code", d$status_code))
    return(NULL)
  }  
  
  fn = g$headers$`content-disposition`
  fn = strsplit(fn, "=")[[1]][2]
  writeBin(g$content, paste0(tmpdir, "/", fn))

  #unzip and output .csv
  unzip(paste0(tmpdir, "/", fn), exdir = paste0(tmpdir, "/downloads"))  
  
  #get and order file list
  froot = strsplit(fn, "-")[[1]][1]
  df = paste0(tmpdir, "/downloads/", froot, "-data.csv")
  pf = paste0(tmpdir, "/downloads/", froot, "-project.csv")
  
  if(file.size(df) == 0){
    file.remove(c(paste0(tmpdir, "/", fn), df, pf))  
    warning("No records returned")
    return(NULL)
  }
  
  #read in data
  d = read.csv(df)
  
  #read in projects
  p = read.csv(pf)
  
  file.remove(paste0(tmpdir, "/", fn))  
  
  if(clean){
    file.remove(c(df, pf))
  }
  
  return(list("data" = d, "projects" = p))
}

#####
#Get field values
#####
wiDB_values = function(fields){

  if(!is.character(fields)){
    stop("fields values must be character strings")
  }
  for(i in 1:length(fields)){
    if(!(fields[i] %in% c("countries", "states", "types", "projects", 
                          "Site_ID", "Sample_ID", "WI_Analysis_ID", 
                          "Climate_ID"))){
      stop("One or more fields values not supported")
    }
  }
  
  baseStr = "https://wateriso.utah.edu/api/v1/values.php?fields="
  q = baseStr
  for(i in fields){
    q = paste0(q, i, ",")
  }
  q = substr(q, 1, nchar(q) - 1)
  ua = user_agent("https://github.com/SPATIAL-Lab/isoWater")

  d = tryCatch({
    GET(q, ua)
  },
  error = function(x){
    message(conditionMessage(x))
    return(NULL)
  },
  warning = function(x){
    message(conditionMessage(x))
    return(NULL)
  }
  )
  
  if(is.null(d)){return()}
  
  if(d$status_code != 200){
    message(paste("Request returned error code", d$status_code))
    return(NULL)
  }
  
  resp = fromJSON(content(d, as = "text", encoding = "UTF-8"))
  
  return(resp)
}
