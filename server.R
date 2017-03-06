library(shiny)

options(shiny.maxRequestSize = 30 * 1024 ^ 2,
        stringsAsFactors = F)

function(input, output, session) {

  rawData = reactive({
    
    # Do not proceed if no data file uploaded
    if (is.null(input$datafile)) {
      return(NULL)
    }
    
    # Import prescription data from .csv file
    infile = input$datafile$datapath
    rawData = read.csv(infile)
    rawData[is.na(rawData)] = 0
    return(rawData)
    
  })
  
  observe({
    
    # Update select input for selection of ID column
    updateSelectInput(session,
                      inputId = "IDcolumn",
                      choices = colnames(rawData()))
    
  })
  
  computation = reactive ({
    
    # Read in data with user specifications
    rawData = rawData()
    IDcolumn = rawData[, input$IDcolumn]
    
    # Extract only columns with necessary data
    days = rawData[, setdiff(grep("days", colnames(rawData)),
                             grep("tot", colnames(rawData)))]
    gennm = rawData[, grep("gennm", colnames(rawData))]
    mgs = rawData[, grep("mgs", colnames(rawData))]
    rxdose = rawData[, grep("rxdose", colnames(rawData))]
    
    # Standardize drug names and create matrix of scaling factors
    gennm = sapply(gennm, function(x) substr(tolower(x), 1, 2))
    gennm = apply(gennm, 2, function(x)
      sapply(x, function(y)
        switch(y,
               "at" = "Atorva",
               "fl" = "Fluva",
               "lo" = "Lova",
               "pi" = "Pitava",
               "pr" = "Prava",
               "ro" = "Rosuva",
               "si" = "Simva",
               "")))
    scaling = apply(gennm, 2, function(x)
      sapply(x, function(y)
        switch(y,
               "Atorva" = 1 / 20,
               "Fluva" = 1 / 160,
               "Lova" = 1 / 80,
               "Pitava" = 1 / 4,
               "Prava" = 1 / 80,
               "Rosuva" = 1 / 5,
               "Simva" = 1 / 40,
               0)))
    
    # Compute DDD and prescribed daily dose for each subject
    DDD = mgs * scaling
    dailyRx = DDD * rxdose
    
    # Compute PDD/DDD ratio (units: DDD/day) for each subject
    PDD = apply(days * dailyRx, 1, sum) / apply(days, 1, sum)
    PDDfactor = cut(PDD,
                    breaks = c(0, .25, .5, .75, 1, 1.25, 1.5, 1.75,
                               2, 2.5, 3, 3.5, 4, 5, 6, Inf),
                    right = F)
    
    # Compute frequencies of visits to pharmacist (units: number of times)
    visits = apply(days != 0, 1, sum)
    
    # Compute changes in prescription over time
    deltaRx = dailyRx[, -1] - dailyRx[, -ncol(dailyRx)]
    for (i in 1:nrow(deltaRx)) {
      deltaRx[i, visits[i]] = 0
    }
    deltaRx = deltaRx[, -ncol(dailyRx)]
    
    # Compute frequencies of prescription changes (units: number of times)
    RxChange = apply(deltaRx, 1, function(x) sum(x != 0))
    RxIncrease = apply(deltaRx, 1, function(x) sum(x > 0))
    RxDecrease = apply(deltaRx, 1, function(x) sum(x < 0))
    totalChange = sapply(1:nrow(dailyRx), function(x)
      dailyRx[x, visits[x]]) - dailyRx[, 1]
    RxChangeDenominator = RxChange
    RxChangeDenominator[RxChangeDenominator == 0] = 1
    ratioIncreaseChange = RxIncrease / RxChangeDenominator
    
    # Compute frequencies of drugs prescribed (units: number of days)
    Atorva = apply(gennm, 1, function(x) sum(x == "Atorva"))
    Fluva = apply(gennm, 1, function(x) sum(x == "Fluva"))
    Lova = apply(gennm, 1, function(x) sum(x == "Lova"))
    Pitava = apply(gennm, 1, function(x) sum(x == "Pitava"))
    Prava = apply(gennm, 1, function(x) sum(x == "Prava"))
    Rosuva = apply(gennm, 1, function(x) sum(x == "Rosuva"))
    Simva = apply(gennm, 1, function(x) sum(x == "Simva"))
    drugs = cbind(Atorva, Fluva, Lova, Pitava, Prava, Rosuva, Simva)
    mainDrug = c("Atorva", "Fluva", "Lova", "Pitava",
                 "Prava", "Rosuva", "Simva")[max.col(drugs)]
    initialDrug = gennm[, 1]
    finalDrug = sapply(1:nrow(gennm), function(x) gennm[x, visits[x] - 1])
    
    # Save processed data to csv file
    data = cbind(PDD, PDDfactor, visits, RxChange, RxIncrease,
                 RxDecrease, totalChange, ratioIncreaseChange, drugs,
                 mainDrug, initialDrug, finalDrug)
    colnames(data) = c("PDD/DDD", "PDD/DDD Factor", "Number of Visits",
                       "PDD/DDD Changes", "PDD/DDD Increases",
                       "PDD/DDD Decreases", "Total PDD/DDD Changes",
                       "Increase/Change Ratio", "Atorva", "Fluva", "Lova",
                       "Pitava", "Prava", "Rosuva", "Simva", "Primary Drug",
                       "Initial Drug", "Final Drug")
    rownames(data) = IDcolumn
    filename = strsplit(input$datafile$name, "\\.")[[1]]
    filename = paste0(filename[1], " (Processed).", filename[2])
    
    return(list(data = data,
                filename = filename))
    
  })
  
  # Indicate if a data file has been uploaded
  output$fileUploaded = reactive({
    return(!is.null(input$datafile))
  })
  outputOptions(output, "fileUploaded", suspendWhenHidden = F)
  
  output$table = renderTable({
    rbind(head(computation()$data, 20), "⋮")
  })
  
  output$downloadData = downloadHandler(
    filename = function(con) computation()$filename,
    content = function(con) write.csv(computation()$data, con)
  )
  
}
