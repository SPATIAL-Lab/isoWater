# Protocol for waterisotopes Database API

Open to Public

*Base URL:*  
`https://wateriso.utah.edu/api/v1`

## Request sites

*Usage:*  
`[GET].../sites.php?option1=sth&option2=sth&option3=sth&return=sth`

### Default response (JSON)  
````
{  
  "sites":  [  
    {  
      "Site_ID"  : STRING,  
      "Latitude" : DOUBLE,  
      "Longitude": DOUBLE  
    },  
    ...  
  ],  
  "latlong": {  
    "minLat": DOUBLE,  
    "maxLat": DOUBLE,  
    "minLong": DOUBLE,  
    "maxLong": DOUBLE  
  },  
  "elevation":  {  
    "minElev": DOUBLE,  
    "maxElev": DOUBLE  
  },  
  "countries": [STRING, STRING,…],  
  "states" : [STRING, STRING,…],   
  "types" : [ STRING, STRING, …],  
  "dates" : {
    "minDate" : STRING,  (format: "yyyy-MM-dd" )  
    "maxDate" : STRING   (format: "yyyy-MM-dd" )  
  },  
  "projects": [  
  {  
    "Project_ID":    STRING,  
    "Project_Name":  STRING  
  },  
  ...  
  ]  
}  
````
### Error response  
````
{   
  "status": {  
    "Message" : STRING  
  }  
}  
````
### Input options  

*Usage:*  
`...minLat=-10&maxLat=50.6&countries=US,CA`    

|Name           |Description                              |Value                                                     |  
|---------------|-----------------------------------------|----------------------------------------------------------|  
|minLat         |a min value of Latitude                  |DOUBLE  -90 to 90, South is negative                      |
|maxLat         |a max value of Latitude                  |DOUBLE  -90 to 90, South is negative                      |
|minLong        |a min value of Longitude                 |DOUBLE  -180 to 180, West is negative                     |
|maxLong        |a max value of Longitude                 |DOUBLE  -180 to 180, West is negative                     |
|minElev        |a min value of Elevation                 |DOUBLE  if elevation is not provided, value=99999         |
|maxElev        |a max value of Elevation                 |DOUBLE  if elevation is not provided, value=-99999        |
|minDate        |a min value of Collection Date           |STRING (format: yyyy-MM-dd e.g. minDate=2018-11-01)       |
|maxDate        |a max value of Collection Date           |STRING (format: yyyy-MM-dd e.g. maxDate=2018-12-31)       |
|countries      |one or multiple countries                |STRING  comma separated (e.g. countries=US,KG,CA)         |
|states         |one or multiple states                   |STRING  comma separated (e.g. states=ID,ON,WA)            |
|types          |one or multiple types of samples         |STRING  comma separated (e.g. types=Lake,Precipitation)   |
|projects       |one or multiple project ids              |STRING  comma separated (e.g. projectIds=00113,00161)     |

### Return options

*Usage:*   
`...return=sites,latlong,types`    

Users can choose what fields to return from the options below. Default return includes all of the return options.

sites   latlong   elevation   countries   states    types   dates   projects

------

## Request data

*Usage:*  
`[GET]...download.php?option1=sth&option2=sth&option3=sth&return=sth`  

### Default response (zip)  

This api will return a .zip file containing a data file, a project file and description file.

#### Data file (csv)

The default data file contains the information below.

Site_ID     
Site_Name    
Latitude    
Longitude    
Elevation    
Sample_ID    
Type    
Start_Date    
Start_Time_Zone    
Collection_Date    
Collection_Time_Zone    
Phase    
Depth_meters    
Sample_Comments    
d2H    
d18O    
d2H_Analytical_SD    
d18O_Analytical_SD    
WI_Analysis_Source    
Project_ID    

NOTE: for embargoed or Proprietary projects, the API will NOT return values for d2H, d18O, d2H_Analytical_SD, or d18O_Analytical_SD. Instead, if a value is available for any of these fields, return "9999", and if no value is available, return "-9999".

#### Project file (csv)

The project file (csv) contains the following information for all projects that appear in the data file.

Contact_Name  
Contact_Email  
Citation  
URL  
Project_Name  

#### Description file (xlsx)

The header file provides a description of the default fields included in the data file.

### Error response
````
{  
  "status": {  
    "Message" : STRING  
  }  
}  
````
### Input options

*Usage:*  
`...minLat=-10&maxLat=50.6&countries=US,CA`  

|name           |Description                              |value                                                     |  
|---------------|-----------------------------------------|----------------------------------------------------------|  
|minLat         |a min value of Latitude                  |DOUBLE  -90 to 90, South is negative                      |
|maxLat         |a max value of Latitude                  |DOUBLE  -90 to 90, South is negative                      |
|minLong        |a min value of Longitude                 |DOUBLE  -180 to 180, West is negative                     |
|maxLong        |a max value of Longitude                 |DOUBLE  -180 to 180, West is negative                     |
|minElev        |a min value of Elevation                 |DOUBLE  if elevation is not provided, value=99999         |
|maxElev        |a max value of Elevation                 |DOUBLE  if elevation is not provided, value=-99999        |
|minDate        |a min value of Collection Date           |STRING (format: yyyy-MM-dd e.g. minDate=2018-11-01)       |
|maxDate        |a max value of Collection Date           |STRING (format: yyyy-MM-dd e.g. maxDate=2018-12-31)       |
|countries      |one or multiple countries                |STRING  comma separated (e.g. countries=US,KG,CA)         |
|states         |one or multiple states                   |STRING  comma separated (e.g. states=ID,ON,WA)            |
|types          |one or multiple types of samples         |STRING  comma separated (e.g. types=Lake,Precipitation)   |
|projects       |one or multiple project ids              |STRING  comma separated (e.g. projectIds=00113,00161)     |

### Return options

*Usage:*  
`...return=sites,latlong,types`  

Users can choose a subset of fields to return in the data file by listing field names as arguments. All arguments are comma separated. Default return includes all of the return options. Project_ID is always returned.

------    

## Request field values    

*Usage:*    
`[GET]...values.php?fields=sth,sth`    

### Default response (JSON)  
````
{
  "countries": [STRING, STRING, ...],   
  "states" : [STRING, STRING, ...],    
  "types" : [ STRING, STRING, ...],   
  "projects": [   
  {    
    "Project_ID": STRING,    
    "Project_Name":  STRING    
  },     
  ...     
  ]     
}      
````

### Error response  
````  
{     
  "status": {     
    "Message" : STRING    
  }     
}     
````   

### fields options  

*Usage:*  
`...fields=types,countries`    

Catagorical database fields are supported, including:

  countries  states  types  projects Site_ID Sample_ID WI_Anlysis_ID Climate_ID

