library(shiny)
library(readxl)
runApp(
  list(
    ui = fluidPage(
      #options(stringsAsFactors = FALSE),
      shinyalert::useShinyalert(),
      titlePanel(""),
      tabsetPanel(
        tabPanel("File Input", fluid = TRUE,
          sidebarLayout(
            sidebarPanel(
              checkboxInput('header', "Contains header in the first row"),
              checkboxInput('one', "Data is stored on one sheet"),
              fileInput('file', 'Choose a xlsx file', accept = c(".xlsx"), ),
                   ),
              
            mainPanel(
             tableOutput('table')
             )
           )
        ),
        tabPanel("Sites", fluid = TRUE,
          sidebarLayout(
             sidebarPanel(
               p("Choose column that best fits, otherwise, choose NA"),
               uiOutput('id',),
               p("If Site_ID is NA, please enter an ID prefix. Otherwise, leave empty."),
               uiOutput('projectname',),
               uiOutput('sitename',),
               uiOutput('latitude',),
               uiOutput('longitude',),
               uiOutput('elevation',),
               uiOutput('address',),
               uiOutput('city',),
               uiOutput('state',),
               uiOutput('country',),
               uiOutput('scomment',),
               actionButton('done', 'Next')
          
        ),
          mainPanel(
            tableOutput('siteTable')
            )
          )
        ),
       tabPanel("Samples", fluid = TRUE,
            sidebarLayout(
              sidebarPanel(
                p("Choose column that best fits, otherwise, choose NA"),
                uiOutput('sid',),
                uiOutput('type',),
                uiOutput('startD',),
                uiOutput('startTZ',),
                uiOutput('collectionD',),
                uiOutput('collectionTZ',),
                uiOutput('sampleVol',),
                uiOutput('collectT',),
                uiOutput('phase',),
                uiOutput('depth',),
                uiOutput('sampleSource',),
                uiOutput('sampIgnore',),
                uiOutput('sampCom',),
                uiOutput('projectID',),
                actionButton('done1', 'Next')
               ),
              mainPanel(
                tableOutput('sampleTable')
               )
            )
         ),
       tabPanel("Export", fluid = TRUE,
                sidebarLayout(
                  sidebarPanel(
                    downloadButton('d1', 'Export')
                  ),
                  mainPanel(
                    #tabsetPanel(type = "tabs",
                                #tabPanel("Sites", tableOutput("ss")))
                  )
                )
                
       )
      ),
      
       
      
    ),
    server = function(input, output){
      options(stringsAsFactors = FALSE)
      
      #list of sheets for download
      sheets_list <- reactive({
        list(
          Sites = sitesSheet(),
          Samples = sampleSheet()
        )
      })
      
      
      conSites = function(sites, samples, siteField = "Site_ID", latField = "Latitude", lonField = "Longitude"){
        sites = sites[order(sites[,4]),]
        sites = sites[order(sites[,3]),]
        for(i in 1:nrow(sites)){
          if(i == 1){
            cid = sites[i, 1]
          } else if(sites[i, 3] != sites[i-1, 3] | sites[i, 4] != sites[i-1, 4]){
            cid = sites[i, 1]
          } else{
            oid = sites[i, 1]
            sites[i, 1] = cid
            ind = samples[,3] == oid
            samples[ind, 3] = cid
          }
        }
        sites = sites[!duplicated(sites[,1]),]
        return(list(sites, samples))
      }
      
      finishedSheets <- reactive({
        ss <- sitesSheet()
        sms <- sampleSheet()
        s <- conSites(ss, sms)
      })
      
      output$ss <- renderTable(finishedSheets())
      
      #downloads sheets
      output$d1 <- downloadHandler(
        filename = function() {"new1.xlsx"},
        content = function(file) { 
          writexl::write_xlsx(sheets_list(), file)}
      )
      
      #takes in user input of table
      data <- reactive({
        file1 <- input$file
        if(is.null(file1)){return()} 
        file.rename(file1$datapath,
                    paste(file1$datapath, ".xlsx", sep=""))
        if(input$one == TRUE && input$header == TRUE){
        read_excel(paste(file1$datapath, ".xlsx", sep=""), 1, col_names = input$header, )
        }
        else{
          shinyalert::shinyalert("Error", "Please reformat file so that all data is stored on one sheet and header is in the first row.", type= "error")
        }
        
      })
      
      #siteid column chosen by user. if "N/A" then we will assign siteids. 
      siteid <- reactive({
        sid <- input$siteID
        df <- data()
        pn <- input$pname
        if(sid == "N/A"){
          sn = seq_along(1:nrow(df))
          sn = formatC(sn, width = 4, format = "d", flag = "0")
          SITEID <- data.frame(paste0(pn, "_site_", sn)) 
        }
        else{
          SITEID <- df[, which(colnames(df) == sid)]
        }
      })
      
      #longitude input
      longitude <- reactive({
        df <- data()
        LONG <- df[, which(colnames(df) == input$long)]
      })
      
      #latitude input
      latitude <- reactive({
        df <- data()
        LAT <- df[, which(colnames(df) == input$lat)]
      })
      
      #renders table 
       output$table <- renderTable({
        if(is.null(data()))
          {return ()}
        data()
      })
       
      #function that checks that latitude is in correct range
       checkLat = function(dataframe, id){
          for(i in 1:length(dataframe)){
            if(dataframe[i,which(colnames(dataframe) == id)] >= 90 || 
               dataframe[i,which(colnames(dataframe) == id)] <= -90){
              return (FALSE);
            }
          }
         return (TRUE);
       }
       
      #function that checks that longitude is in correct range
       checkLong = function(dataframe, id){
         for(i in 1:length(dataframe)){
           if((dataframe[i,which(colnames(dataframe) == id)] >= 180) || 
              (dataframe[i,which(colnames(dataframe) == id)] <= 0)){
             return (FALSE);
           }
         }
         return (TRUE);
       }
       
       #function that checks if lat/long field are numeric
        checkNumeric = function(dataframe, id){
          L <- unlist(dataframe[,which(colnames(dataframe) == id)])
          L <- na.omit(L)
          for(i in 1:length(L)){
            L[i] <- as.numeric(L[i])
          }
          return(L);
        }
      
      #site sheet
       observeEvent(input$done, {
         df <- data() 
         
         #checks if lat/long are numeric
         LAT <- checkNumeric(df, input$lat)
         LONG <- checkNumeric(df, input$long)
         
         if(is.na(LAT) == FALSE || is.na(LONG) == FALSE){
            if(checkLat(df, input$lat) == FALSE ){
              shinyalert::shinyalert(title = "Error", 
                  "Some values in your latitude column are outside range [-90,90].", type = "error",)
            }
            else if(checkLong(df, input$long) == FALSE){
                shinyalert::shinyalert(title = "Error", 
                  "Some values in your longitude column are outside range [0,180].", type = "error",)
               }
            else{
                showModal(modalDialog(
                   title = "Warning", 
                  "Any rows without longitude or latitude values will be deleted.", 
                      footer = actionButton("dismiss", label = "OK")
              ))
            }
         } 
         else{
           shinyalert::shinyalert(title = "Error", 
                                     "The column you selected for latitude/longitude is not numeric.", 
                                                                                    type = "error", )
         }
         
       })

       
       sitesSheet <- eventReactive(input$dismiss, {
        removeModal()
        if(!is.null(input$file)){
        df <- data()
        SID <- siteid()
        

        
        #matches user input to column in inputted dataframe
        SITENAME <- df[, which(colnames(df) == input$sn)]
        LAT <- df[, which(colnames(df) == input$lat)]
        LONG <- df[,which(colnames(df) == input$long)]
        ELEV <- df[, which(colnames(df) == input$elv)]
        ADDY <- df[, which(colnames(df) == input$addy)]
        CITY <- df[, which(colnames(df) == input$cityy)]
        STATE <- df[, which(colnames(df) == input$stateProvince)]
        COUNTRY <- df[, which(colnames(df) == input$cc)]
        COMMENT <- df[, which(colnames(df) == input$sc)]
       
        
        if(input$sn == "N/A"){
          SITENAME <- rep(NA)
        }
        if(input$elv == "N/A"){
          ELEV <- rep(NA)
        }
        if(input$addy == "N/A"){
          ADDY <- rep(NA)
        }
        if(input$cityy == "N/A"){
          CITY <- rep(NA)
        }
        if(input$stateProvince == "N/A"){
          STATE <- rep(NA)
        }
        if(input$cc == "N/A"){
          COUNTRY <- rep(NA)
        }
        if(input$sc == "N/A"){
          COMMENT <- rep(NA)
        }
        
        #creates sites sheet
        siteHeader <- c("Site_ID", "Site_Name", "Latitude", "Longitude", "Elevation_mabsl", 
                        "Address", "City", "State_or_Province", "Country", "Site_Comments")
        dff <- data.frame(SID, SITENAME, LAT, longitude(), ELEV,
                          ADDY, CITY,  STATE, COUNTRY,  COMMENT)
        names(dff) <- siteHeader
        
        #removes rows without long/lat. needs to display a pop up dialogue.
        dfff <- subset(dff, !is.na(Longitude) | !is.na(Latitude), )
        }
        
        
      })
      
      #renders site table
      output$siteTable <- renderTable(sitesSheet(), rownames = TRUE)
       
       
      #samples sheet
      observeEvent(input$done1, {
        showModal(modalDialog(
          title = "Warning", "Any rows without a Type value will be deleted.", footer = actionButton("dismiss1", label = "OK")
        ))
      })
      
      sampleSheet <- eventReactive(input$dismiss1, {
        removeModal()
        if(!is.null(input$file)){
          dframe <- data()
          sid <- siteid()
          
          #matches user input with column in inputted dataframe
          SAMPLEID <- dframe[, which(colnames(dframe) == input$ssid)]
          TYPE <- dframe[, which(colnames(dframe) == input$tt)]
          STARTDATE <- dframe[, which(colnames(dframe) == input$sd)]
          STARTTZ <- dframe[, which(colnames(dframe) == input$stz)]
          COLD <- dframe[, which(colnames(dframe) == input$cd)]
          COLTZ <- dframe[, which(colnames(dframe) == input$ctz)]
          SAMPLEVOL <- dframe[, which(colnames(dframe) == input$sml)]
          COLTYPE <- dframe[, which(colnames(dframe) == input$ct)]
          PHASE <- dframe[, which(colnames(dframe) == input$ph)]
          DEPTH <- dframe[, which(colnames(dframe) == input$dm)]
          SAMPLESOURCE <- dframe[, which(colnames(dframe) == input$sampS)]
          SAMPLEIGNORE <- dframe[, which(colnames(dframe) == input$sampI)]
          SAMPLECOM <- dframe[, which(colnames(dframe) == input$sampC)]
          PROJECTID <- dframe[, which(colnames(dframe) == input$pID)]
         
          if(input$ssid == "N/A"){
            SAMPLEID <- rep(NA)
          }
          if(input$tt == "N/A"){
            TYPE <- rep(NA)
          }
          if(input$sd == "N/A"){
            STARTDATE <- rep(NA)
          }
          if(input$stz == "N/A"){
            STARTTZ <- rep(NA)
          }
          if(input$cd == "N/A"){
            COLD <- rep(NA)
          }
          if(input$ctz == "N/A"){
            COLTZ <- rep(NA)
          }
          if(input$sml == "N/A"){
            SAMPLEVOL <- rep(NA)
          }
          if(input$ct == "N/A"){
            COLTYPE <- rep(NA)
          }
          if(input$ph == "N/A"){
            PHASE <- rep(NA)
          }
          if(input$dm == "N/A"){
             DEPTH <- rep(NA)
          }
          if(input$sampS == "N/A"){
            SAMPLESOURCE <- rep(NA)
          }
          if(input$sampI == "N/A"){
            SAMPLEIGNORE <- rep(NA)
          }
          if(input$sampC == "N/A"){
            SAMPLECOM <- rep(NA)
          }
          if(input$pID == "N/A"){
            PROJECTID <- rep(NA)
          }
          
          #throwing error "object cannot be coerced to type double"
          #STARTDATE <- convertToDate(STARTDATE)
          #COLD <- convertToDate(COLD)
          
          #create samples sheet
          sampleHeader <- c("SampleID", "Sample_ID_2", "Site_ID", "Type", "Start_Date", "Start_Time_Zone", 
                          "Collection_Date", "Collection_Time_Zone", "Sample_Volume_ml", "Collector_type", 
                          "Phase", "Depth_meters", "Sample_Source", "Sample_Ignore", "Sample_Comment", "Project_ID")
          d <- data.frame(SAMPLEID, SAMPLEID, sid, TYPE, STARTDATE, STARTTZ,
                            COLD, COLTZ, SAMPLEVOL, COLTYPE, PHASE, DEPTH, SAMPLESOURCE,
                            SAMPLEIGNORE, SAMPLECOM, PROJECTID)
          names(d) <- sampleHeader
          
          #removes rows without a type
          dd <- subset(d, !is.na(Type), )
        }
       
      })
      
      #renders sample table
      output$sampleTable <- renderTable(sampleSheet(), rownames = TRUE)
       
      
      #function that rearranges names vector if column that might be field is found 
      fieldMatch = function(d, list){
        if(!is.null(d)){
          cn <- names(d)
          for(i in 1:length(cn)){
            if(tolower(cn[i]) %in% list){
              l = cn[i]
              cn <- cn[cn != cn[i]]
              cn <- append(cn, l, after = 0)
            }
          }
          return( cn <- as.list(cn))
        }
      }
      
      #function that rearranges names vector if column that might be field is found (FIX: doesn't work correctly if match is found)
      fieldMatchNA = function(d, list){
        if(!is.null(d)){
          cn <- names(d)
          for(i in 1:length(cn)){
            if(tolower(cn[i]) %in% list){
              l = cn[i]
              cn <- cn[cn != cn[i]]
              cn <- append(cn, l, after = 0)
              return( cn <- as.list(cn))
            }
          } 
          cn <- c("N/A", cn)
          return( cn <- as.list(cn))
        }
      }
      
      
#field matching
    #site fields
      siteidd <- reactive({
        sn = c("site_id", "site id", "id")
        fieldMatchNA(data(), sn)
      })
      #rearranges names vector if column that might be sitename is found 
      siteName <- reactive({
        sn = c("site_name", "name", "sitename")
        fieldMatchNA(data(), sn)
      })
      
      #rearranges names vector if column that might be latitude is found 
      lat <- reactive({
        latitude = c("latitude", "lat")
        fieldMatch(data(), latitude)
      })
      
      #rearranges names vector if column that might be longitude is found 
      long <- reactive({
        longitude = c("longitude", "long", "lon")
        fieldMatch(data(), longitude)
      })
      
      #rearranges names vector if column that might be country is found
      country <- reactive({
        Country = c("country")
        fieldMatchNA(data(), Country)
      })
      
      #rearranges names vector if column that might be elevation is found 
      elev <- reactive({
        elevation = c("elevation", "elev", "elevation_mabsl")
        fieldMatchNA(data(), elevation)
      })
      
      #rearranges names vector if column that might be state/province is found 
      state <- reactive({
        states = c("state","province", "state_or_province")
        fieldMatchNA(data(), states)
      })
      
      #rearranges names vector if column that might be city is found 
      city <- reactive({
        City = c("city", "municipality")
        fieldMatchNA(data(), City)
      })
      
      #rearranges names vector if column that might be address is found 
      address <- reactive({
        a = c("address", "location")
        fieldMatchNA(data(), a)
      })
      
      #rearranges names vector if column that might be site comment is found 
      sitecomment <- reactive({
        d = c("site_comment", "sitecomment", "site comment", "comment", "comments", "site_comments", "site comments")
        fieldMatchNA(data(), d)
      })
      
      
    #sample fields
      #rearranges names vector if column that might be type is found 
      typee <- reactive({
        type = c("type", "source", "water source", "water_source", "water_type", "water type", "sample type", "sample_type")
        fieldMatch(data(), type)
      })
      
      #rearranges names vector if column that might be phase is found 
      phase <- reactive({
        Phase = c("phase", "water phase")
        fieldMatchNA(data(), Phase)
      })
      
      #rearranges names vector if column that might be collection date is found 
      collectionDate <- reactive({
        cd = c("collection date", "date collected", "date", "collection_date", "date_collected")
        fieldMatchNA(data(), cd)
      })
      
      #rearranges names vector if column that might be start date is found 
      startDate <- reactive({
        sd = c("start date", "date", "start", "start_date")
        fieldMatchNA(data(), sd)
      })
      
      #rearranges names vector if column that might be depth is found 
      depth <- reactive({
        d = c("depth", "depth_meters", "depth meters")
        fieldMatchNA(data(), d)
      })
      
      #rearranges names vector if column that might be start time zone is found 
      startTimezone <- reactive({
        d = c("timezone", "start_time_zone", "start", "start_timezone", "start timezone")
        fieldMatchNA(data(), d)
      })
      
      #rearranges names vector if column that might be collection time zone is found 
      collectionTimezone <- reactive({
        d = c("timezone", "collection_time_zone", "collection", "collection_timezone", "collection timezone")
        fieldMatchNA(data(), d)
      })
      
      #rearranges names vector if column that might be sample_Vol is found 
      samplevolume <- reactive({
        d = c("samplevolume", "sample_volume", "sample volume", "sample_volume_ml", "volume", "ml")
        fieldMatchNA(data(), d)
      })
      
      #rearranges names vector if column that might be collector type is found 
      collectortype <- reactive({
        d = c("collector", "collector_type", "collector type")
        fieldMatchNA(data(), d)
      })
      
      #rearranges names vector if column that might be sample source is found 
      samplesource <- reactive({
        d = c("sample_source", "sample source", "source")
        fieldMatchNA(data(), d)
      })
      
      #rearranges names vector if column that might be sample ignore is found 
      sampleignore <- reactive({
        d = c("sample ignore", "ignore", "sample_ignore")
        fieldMatchNA(data(), d)
      })
      
      #rearranges names vector if column that might be sample comment is found 
      samplecomment <- reactive({
        d = c("comment", "sample comment", "sample_comment", "comments", "sample comments", "sample_comments")
        fieldMatchNA(data(), d)
      })
      
      #rearranges names vector if column that might be project ID is found 
      projectid <- reactive({
        d = c("project_id", "project", "id", "project id")
        fieldMatchNA(data(), d)
      })
      
      
#Sites drop down lists  
      
      output$projectname <- renderUI({
        textInput('pname', 'ID Prefix',)
      })
       output$id <- renderUI({
         selectInput('siteID', 'Site_ID', choices = c(siteidd()))
       })
       output$sitename <- renderUI({
         selectInput('sn', 'Site_Name', choices = c(siteName()))
       })
       output$latitude <- renderUI({
         selectInput('lat', 'Latitude', choices = c(lat()))
       })
       output$longitude <- renderUI({
         selectInput('long', 'Longitude', choices = c(long()))
       })
       output$elevation <- renderUI({
         selectInput('elv', 'Elevation_mabsl', choices = c(elev()))
       })
       output$address <- renderUI({
         selectInput('addy', 'Address', choices = c(address()))
       })
       output$city <- renderUI({
         selectInput('cityy', 'City', choices = c(city()))
       })
       output$state <- renderUI({
         selectInput('stateProvince', 'State_or_Province', choices = c(state()))
       })
       output$country <- renderUI({
         selectInput('cc', 'Country', choices = c(country()))
       })
       output$scomment <- renderUI({
         selectInput('sc', 'Site_Comment', choices = c(sitecomment()))
       })
       
       
#Samples drop down lists
       
       output$sid <- renderUI({
         selectInput('ssid', 'Sample_ID', choices = c(names(data())))
       })
       output$type <- renderUI({
         selectInput('tt', 'Type', choices = c(typee()))
       })
       output$startD <- renderUI({
         selectInput('sd', 'Start_Date', choices = c(startDate()))
       })
       output$startTZ <- renderUI({
         selectInput('stz', 'Start_Time_Zone', choices = c(startTimezone()))
       })
       output$collectionD <- renderUI({
         selectInput('cd', 'Collection_Date', choices = c(collectionDate()))
       })
       output$collectionTZ <- renderUI({
         selectInput('ctz', 'Collection_Time_Zone', choices = c(collectionTimezone()))
       })
       output$sampleVol <- renderUI({
         selectInput('sml', 'Sample_Volume_ml', choices = c(samplevolume()))
       })
       output$collectT <- renderUI({
         selectInput('ct', 'Collector_type', choices = c(collectortype()))
       })
       output$phase <- renderUI({
         selectInput('ph', 'Phase', choices = c(phase()))
       })
       output$depth <- renderUI({
         selectInput('dm', 'Depth_meters', choices = c(depth()))
       })
       output$sampleSource <- renderUI({
         selectInput('sampS', 'Sample_Source', choices = c(samplesource()))
       })
       output$sampIgnore <- renderUI({
         selectInput('sampI', 'Sample_Ignore', choices = c(sampleignore()))
       })
       output$sampCom <- renderUI({
         selectInput('sampC', 'Sample_Comments', choices = c(samplecomment()))
       })
       output$projectID <- renderUI({
         selectInput('pID', 'Project_ID', choices = c(projectid()))
       })
       

      
    }
  )
)



