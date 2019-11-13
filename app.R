source("libraries.R") # add new libraries if needed
source("pdPlots.R")  # get big pdp plot 

# needed to scroll up and down
textsfile = "shinyjs.pageCol = function(){window.scrollTo(0,document.body.scrollHeight);};
shinyjs.toTop = function(){window.scrollTo(0,0);};"

# ui --------------------------------------------------------------------------------------------------------------------------------
ui = dashboardPage(
  skin = "green",
  dashboardHeader(title = "IML-Shiny", titleWidth = 190
                  ),
  dashboardSidebar(
    width = 190,
    sidebarMenu(
      id = "tabs",
      menuItem("Introduction", tabName = "intro", icon = icon("home")),
      menuItem("Global Effects", tabName = "global", icon = icon("globe")),
      menuItem("Local Interpretation", tabName = "local", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML('
      .main-header .logo {
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 19px;
                              }
                              '))),
    useShinyjs(),
    # needed to scroll up and down
    extendShinyjs(text = textsfile),
    tabItems(
      
      # ~ ui intro --------------------------------------------------------------------------------------
      tabItem(
        tabName = "intro",
        fluidRow(
          column(width = 1),
          column(width = 10,
            box(
              width = NULL,
              solidHeader = TRUE,
              h2("Interpretable Machine Learning:", 
                 style = "text-align: center"),
              h2("Making Black Box Models Explainable", style = "text-align: center"),
              br(),
              img(src = "blackbox.jpg", width = "100%", height = "100%"),
              br(),
              p("Machine learning models usually operate as intransparent black boxes, which has huge potential to improve 
                research, products and processes. Interpretable Machine Learning aims at making 'boxes' transparent."),
              p("This dashboard aims at creating a iml-summary in a much easier way for users. The idea of this APP is to 
               supplement the fantastic APP that Julia Fried has written (an application study using 
                the Munich Rent Index). The dashboard provides an summary for your model with Interpretable Machine Learning.
                
                There is a right-click menu. Click Global Effects, save your predictor object as .RDS file and upload it. Then, 
                you get a table in few minutes, which includes variables' names,corresponding value ranges, partial dependence 
                plots(PDPs) and their feature importance.
                Click Local Interpretation, you will see the data set on the top. Then, if you select a certain row, the corresponding
                shapley values and its plot will be shown automatically as follows.")),
            br(),
            tags$a(href="https://christophm.github.io/interpretable-ml-book/", "iml book by Christoph Molnar"),
            br(),
            tags$a(href="https://juliafried.shinyapps.io/MunichRentIndex/", "an example by Julia Fried")),
          column(
            width = 1
          )
        )
      ),
      
      # ~ ui global --------------------------------------------------------------------------------------------------
      tabItem(
        tabName = "global",
        fluidRow(
          column(width = 12,
                 box(
                   width = NULL,
                   solidHeader = TRUE,
                   h2("Save your Predictor Object that created from iml package 
                           as PrediObj.RDS and upload this file", 
                      style = "text-align: center"),
                  
                  box(
                    solidHeader = TRUE,
                    fileInput('PrObj', 'Choose file to upload',accept = c('RDS.','rds.')))
                 ),
            ## describe PDP
            fluidRow(
              column(
                width = 9,
                box(
                  width = NULL,
                  solidHeader = TRUE,
                  h2("IML-Summary with PDP plots"),
                  p(""),
                  HTML("")
                ),
                
                # actionButton('goTable', 'Go table'),
                
                # table
                box(
                  width = NULL,
                  solidHeader = TRUE,
                  DT::dataTableOutput("pdpplot")
                )
              ),  # External links
              column(
                width = 3,
                box(
                  width = NULL,
                  solidHeader = TRUE,
                  h4("Links for more Explanations"),
                  HTML(paste("
                         <b>Introduction to iml</b>
                         <ul><li><a href='https://cran.r-project.org/web/packages/iml/vignettes/intro.html' target='_blank'>Interpretable Machine Learning in R</a></li>
                         </ul>
                         <b>Partial Dependence Plots (PDP)</b>
                         <ul><li><a href='https://christophm.github.io/interpretable-ml-book/pdp.html' target='_blank'>IML book</a></li>
                         <li><a href='https://mlr-org.github.io/mlr/articles/tutorial/partial_dependence.html' target='_blank'>Tutorial with package mlr</a></li>
                         </ul>
                         <b>Feature Importance</b>
                         <ul><li><a href='https://christophm.github.io/interpretable-ml-book/feature-importance.html' target='_blank'>IML book</a></li>
                         </ul>                
                         "))
                ),
                
                # Scroll down option
                box(
                  width = NULL,
                  solidHeader = TRUE,
                  h4("Info: Feature Explanations below!"),
                  actionButton("ctSend", "Scroll down")
                )
              ),
              
              column(
                width = 12,
                box(
                  width = NULL,
                  solidHeader = TRUE,
                  title = "Feature importance",
                  plotOutput("impplot")
                )
              ),
              
              # Tabs with variable explanations
              column(
                width = 12,
                box(
                  width = NULL,
                  solidHeader = TRUE,
                  title = "Explanation of Features",
                  uiOutput("varExplanation"),
                  br(),
                  actionButton("scrollUp", "Scroll up")
                )
              )
            )
          )
        )
      ), 
      # ~ ui local --------------------------------------------------------------------------------------
      tabItem(
        tabName = "local",
        fluidRow(
          sidebarPanel(
            # Input: Numeric entry for number of obs to view ----
            numericInput(inputId = "obs",
                         label = "Number of observations to view:",
                         value = 10)# default = 10
          ),
          mainPanel(
            # width = 12,
            box(
              width = NULL,
              title = "Your data",
              solidHeader = TRUE,
              DT::dataTableOutput("df"))
          ),
          column(
            width = 12,
            box(
              width = 4,
              solidHeader = TRUE,
              # background = "black",
              # collapsible=TRUE,
              title = "Select an instance that you want to calculate the shapley values and plot",#, br(), "More box content",
              textInput("instance", "instance:")
            ),
            box(
              width = 4,
              solidHeader = TRUE,
              # background = "black",
              # collapsible=TRUE,
              title = "Give the number of Monte Carlo samples for estimating the Shapley value.",#, br(), "More box content",
              textInput("nr_sv", "number:")
            ),
            box(
              width = 4,
              solidHeader = TRUE,
              # background = "black",
              # collapsible=TRUE,
              title = "set seed for Monte Carlo samples for estimating the Shapley value.",#, br(), "More box content",
              textInput("seed", "seed:")
              # actionButton('goPlot', 'Go plot'),
              # actionButton('goTabel', 'Go tabel')
              # 
            )),
          column(
            width = 9,
            tabsetPanel(
              id = "tabPanelId",
              tabPanel(
                # This tab allows to set up individual values for all features
                "Output of shapley value",
                fluidRow(
                  column(
                    width = 12,
                    box(
                      width = NULL,
                      title = "Explain the Outcome of your Results with Shapley Explanations",
                      solidHeader = TRUE,
                      DT::dataTableOutput("shapleyValue"))
                  )
                )
              ),
              tabPanel(
                "Plot",
                # The output tab shows the prediction and Shapley explanations
                fluidRow(
                  # Tabs with variable explanations
                  column(
                    width = 12,
                    box(
                      width = NULL,
                      title = "Plot with Shapley Values",
                      solidHeader = TRUE,
                      plotOutput("shapleyValuePlot", width = "100%", height = "400px", click = NULL,
                                 dblclick = NULL, hover = NULL, hoverDelay = NULL,
                                 hoverDelayType = NULL, brush = NULL, clickId = NULL,
                                 hoverId = NULL, inline = FALSE)
                    )
                  )
                  
                )
              )
              
            )
             
          ),
          column(
            width = 3,
            box(
              width = NULL,
              solidHeader = TRUE,
              h4("Links for more Explanations"),
              HTML(paste("
                         <b>Shapley Values</b>
                         <ul><li><a href='https://christophm.github.io/interpretable-ml-book/shapley.html' target='_blank'>IML book</a></li>
                         </ul>
                         "))
            )
          )
        )
      )
    )
  ),
  # Needed to plot the sparkline in DT
  htmlwidgets::getDependency("sparkline", "sparkline")
)

# server ---------------------------------------------------------------------------------------------------------------------
server = function(input, output, session){
  
  # ~ server global ---------------------------------------------------------------------------------------------
  # By default, the file size limit is 5MB. It can be changed by
  # setting this option. Here we'll raise limit to 100MB.
  options(shiny.maxRequestSize = 100*1024^2)
  
  dataInput = reactive({
    inFile = input$PrObj
    validate(
      need(input$PrObj != "", "Please upload your predictor object")
    )
    PrediObj = readRDS(inFile$datapath)
  })
  
  pdpBigPlot = reactive({
    PrediObj = dataInput()
    x = PrediObj$data$get.x()
    target = PrediObj$data$y
    dat = as.data.frame(PrediObj$data$get.xy())
    mod = PrediObj$model
    pd.big = pdPlot(p = PrediObj, mod, x, target)
  })
  
  
  
  
  
 spark.df = reactive({
  # input$goTable
  # draw callback needed for sparklines
  cb = htmlwidgets::JS('function(){debugger;HTMLWidgets.staticRender();}')
  PrediObj = dataInput()
  x = PrediObj$data$get.x()
  target = PrediObj$data$y
  dat = as.data.frame(PrediObj$data$get.xy())
  # var.names = colnames(x)
  dat.type = as.data.frame(sapply(x, class))
  num.var = rownames(dat.type)[dat.type[,1] =="numeric"]
  fac.var = rownames(dat.type)[dat.type[,1] =="factor"]
  size.data = dim(x)[1]
  
  # check variables with null pdp
  # null pdp - all entries are equal, will be excluded from step length determination
  valid.num = lapply(1:length(num.var), function(i){
    id = num.var[i]
    pd = FeatureEffect$new(PrediObj, id, method = "pdp")$results$.y.hat
    if(!isTRUE(all.equal(max(pd), min(pd)))){
      id
    }
  })
  valid.num.feat = unlist(valid.num)
  
  # calculate feature important
  fi = FeatureImp$new(PrediObj, loss = "mae", n.repetitions = 20)
  fi$results
  fi.res = fi$results[, c(1, 4)] 
  
  
  
  withProgress(message = 'Making table', value = 0, {
    # Number of times we'll go through the loop
    n <- size.data
    
    #  for factor variables -----------------------------------------------
    pd.list = c()
    pd.list = lapply(1:length(fac.var), function(i){
      dat.type = "factor"
      # feature importance per variable
      fi.var = fi.res[fi.res$feature == fac.var[i], ]$importance
      # calculate partial dependence dat
      pdr = FeatureEffect$new(PrediObj, fac.var[i], method = "pdp")$results
      # sort by name that colors can be set correctly
      pdr = pdr[order(pdr[, 1]), ]
      pdr.y = pdr$.y.hat
      
      # Aim: highlight pdp bar for factor variables, all other bars 
      # have the same color
      var.res = as.data.frame(diag(x = 1, nrow = nlevels(dat[,fac.var[i]]), 
                                   names = FALSE))
      pdr$color.map = c()
      for (j in 1:nrow(var.res)){
        pdr$color.map[j] = paste(as.vector(unlist(var.res[j,])), 
                                 collapse = ",")
      }
      print(paste(i, "from", length(fac.var)))
      df = pdr[,c(1,4)]
      df[,1] =  paste0(fac.var[i], df[,1])
      var.name = fac.var[i]
      colnames(df)[1] = "Feature"
      pd.list = as.data.frame(c(dat.type, fi.var, df, var.name, pdr.y))
      # add colnames for dat.type, at the moment X.factor. or X.numeric.
      names(pd.list)[1] = "dat.type"
      names(pd.list)[2] = "fi.var"
      names(pd.list)[5] = "var.name"
      pd.list
    })
    
    one.row = c()
    for (j in 1:length(pd.list)){
      for (i in 1:nrow(pd.list[[j]])){
        element.name = as.character(pd.list[[j]]$Feature[i])
        one.row[[element.name]] = as.data.frame(pd.list[[j]][i, ]) # rlist package
      }
    }
    
    feat = c()
    feat = sapply(1:length(one.row), function(u){
      feat = as.character(one.row[[u]]$var.name)
    })
    
    val = c()
    val = sapply(1:length(one.row), function(o){
      val = as.character(one.row[[o]]$Feature)
    })
    
    fi = c()
    fi = sapply(1:length(one.row), function(e){
      fi = one.row[[e]]$fi.var
    })
    
    
    pd.content = c()
    col.map = c()
    no.temp = c()
    
    for (i in 1:length(one.row)){
      temp = unlist(strsplit(as.character(one.row[[i]]$color.map), ","))
      temp[temp == 0] = "#337ab7"
      temp[temp == 1] = "#fc00a8"
      for (j in 1:length(temp)){
        col.map = list.append(col.map, temp[j])
      }
      no.temp = list.append(no.temp, length(temp))
    }
    
    k = 1
    iter = no.temp[1]
    for (i in 1:length(one.row)){
      print(paste(k, ":", iter, "--", one.row[[i]]$Feature, "--", no.temp[[i]]))
      
      pd.content[[i]] = spk_chr(
        unlist(dropNamed(
          one.row[[i]], 
          drop = c("dat.type", "fi.var", "Feature",  "color.map",  "var.name")),
          use.names = FALSE),
        type = "bar", colorMap = col.map[k:iter]
      )
      
      k = cumsum(no.temp)[i] + 1
      iter = cumsum(no.temp)[i + 1]
    }
    
    spark.data = tibble(Feature = feat[[1]], 
                        Value = val[[1]],
                        PDP = pd.content[[1]],
                        FeatureImportance = fi[[1]])
    
    
    if(length(one.row)>1){{
      incProgress(1/n, detail = paste("Doing part(factor variables)", i))}
      for (i in 2:length(one.row)){
        spark.data = add_row(spark.data, 
                             Feature = feat[[i]], 
                             Value = val[[i]],
                             PDP = pd.content[[i]],
                             FeatureImportance = fi[[i]])
      }
    }else{
      spark.data
    }
    
    #  for numeric variables -----------------------------------------------
    pd.list.n = c()
    pd.list.n = lapply(1:length(num.var), function(i){
      dat.type = "numeric"
      # feature importance per variable
      fi.var = fi.res[fi.res$feature == num.var[i], ]$importance
      # calculate partial dependence data
      # pd = Partial$new(p, num.var[i], ice = FALSE)$results
      pdr = FeatureEffect$new(PrediObj, num.var[i], method = "pdp")$results
      pdr = pdr[order(pdr[, 1]), ]
      pdr.y = pdr$.y.hat
      
      # No different colors for numeric features
      pdr$color.map = 1
      
      df = pdr[,c(1,4)]
      print(paste(i, "from", length(num.var), "--", num.var[i]))
      
      
      var.name  = as.character(num.var[i])
      df[,1] =  paste("Value ranges from", min(dat[[var.name]]), "to", max(dat[[var.name]]))
      
      colnames(df)[1] = "Feature"
      # dummy feat ----
      pd.list.n = as.data.frame(c(dat.type, round(fi.var, 4), df, var.name, pdr.y))
      names(pd.list.n)[1] = "dat.type"
      names(pd.list.n)[2] = "fi.var"
      names(pd.list.n)[5] = "var.name"
      # add new names per variable 
      pd.list.n
    })
    
    # one.row = c()
    # for (j in 1:length(pd.list.n)){
    #   for (i in 1:nrow(pd.list.n[[j]])){  ###   nrow(pd.list[[j]])   ---> N ULL???????????
    #     element.name = as.character(pd.list.n[[j]]$Feature[i])
    #     one.row[[element.name]] = as.data.frame(pd.list.n[[j]][i, ]) # rlist package
    #   }
    # }
    
    one.row = c()
    one.row = lapply(1:length(pd.list.n), function(j){
      last.row = nrow(pd.list.n[[j]])
      one.row = as.data.frame(pd.list.n[[j]][last.row,])
    })
    
    feat = c()
    feat = sapply(1:length(one.row), function(u){
      feat = as.character(one.row[[u]]$var.name)
    })
    
    val = c()
    val = sapply(1:length(one.row), function(o){
      val= as.character(one.row[[o]]$Feature)
    })
    
    fi = c()
    fi = sapply(1:length(one.row), function(e){
      fi = one.row[[e]]$fi.var
    })
    
    pd.content = c()
    for (i in 1:length(one.row)){
      pd.content[[i]] = spk_chr(
        unlist(dropNamed(
          one.row[[i]], 
          drop = c("dat.type", "fi.var", "Feature", "color.map", "var.name")),
          use.names = FALSE),
        lineColor = "#337ab7",
        type = "line",
        minSpotColor = "#fc00a8",
        maxSpotColor = "#fc00a8",
        spotColor = "#fc00a8"
      )
    }
    # pd.contentn = c()
    # pd.contentn = sapply(1:length(one.row), function(e){
    #   pd.contentn = spk_chr(
    #     unlist(dropNamed(
    #       one.row[[e]],
    #       drop = c("dat.type", "fi.var", "Feature", "color.map", "var.name")),
    #       use.names = FALSE),
    #     lineColor = "#337ab7",
    #     type = "line",
    #     minSpotColor = "#fc00a8",
    #     maxSpotColor = "#fc00a8",
    #     spotColor = "#fc00a8"
    #   )
    #   pd.contentn
    # })
    #   
    for (i in 1:length(one.row)){{
      incProgress(1/n, detail = paste("Doing part(numeric variables)", i))
      spark.data = add_row(spark.data, 
                           Feature = feat[[i]], 
                           Value = val[[i]],
                           PDP = pd.content[[i]],
                           FeatureImportance = fi[[i]])
    }
      spark.data = as.data.frame(spark.data)
      
      # draw callback needed for sparklines
      # https://stackoverflow.com/questions/20875081/properly-rendering-sparklines-in-a-datatable  
      cb <- htmlwidgets::JS('function(){debugger;HTMLWidgets.staticRender();}')
      
      
      columnLabels <- c("Variables", 
                        "variable characteristics",
                        "partial dependece plot (PDP) per feature ",
                        "feature importance per variable")
      
      
      
      # Pause for 0.1 seconds to simulate a long computation.
      Sys.sleep(0.1)
    }
  })
  
 
  
   # output : table
  datatable(
    # spark.data is created in globalEffects.R file
    spark.data,
    rownames = FALSE,
    escape = FALSE,
    # extensions = "Buttons",
    caption = "Mouse over column titles for more information!",
    
    options = list(
      columnDefs = list(
        list(orderable = FALSE, targets = c(1,3))
      ),
      drawCallback =  cb,
      pageLength = nrow(spark.data),
      dom = "t"
    ),
    # do not allow to select lines (needed to hide white sparkline bars)
    selection = "none",
    
    # show info for column names
    container = htmltools::withTags(
      table(
        class = 'display',
        thead(
          tr(apply(data.frame(colnames=names(spark.data), 
                              labels=columnLabels), 1,
                   function(x) th(title=x[2], x[1])))
        )
      )
    )
  )
})
  

  
  output$pdpplot = renderDT({
    spark.df()
  })
  
  
  output$impplot = renderPlot({
    PrediObj = dataInput()
    fi = FeatureImp$new(PrediObj, loss = "mae", n.repetitions = 20)
    plot(fi)
  })
  
  
  # Explain all variables
  output$varExplanation = renderUI({
    PrediObj = dataInput()
    x = PrediObj$data$get.x()
    pd.big = pdpBigPlot()
    
    # reorder variables -- name
    feat.name = as.data.frame(names(x))
    feat.name = feat.name[order(feat.name$name), ]
    feat.name = as.vector(feat.name)
    
    name = lapply(1:length(feat.name), function(i){
      HTML(feat.name[i])
    })
    # sort variables alphabetically
    names = sort(unlist(name))
    
    
    pd.big.plot = lapply(1:length(names), function(i){
      output$plotname = renderPlot({
        pd.big[[i]]
      }, width = 400, height = 300)
    })
    
    var.tabs = lapply(1:length(names), function(i){
      tabPanel(title = names[[i]], 
               # create two columns
               fluidRow(
                 
                 column(4, pd.big.plot[[i]])
               )
      )
    })
    
    do.call(tabsetPanel, var.tabs)
  })
  
  # Scroll down to variable explanations
  observeEvent(input$ctSend, {
    js$pageCol (input$ctSend)
  })
  
  # Scroll up to top of page
  observeEvent(input$scrollUp, {
    js$toTop (input$scrollUp)
  })
  
  
  # ~ server local --------------------------------------------------------------------------------
  # show the data in a data table
  output$df = renderDT({
    PrediObj = dataInput()
    validate(
      need(input$obs != "", "Number of observations is missing."),
      need(input$obs > 0, "Number of observations should be a positive integer.")
    )
    head( PrediObj$data$get.xy(), n = input$obs)
   
    
  })
  
  shapleyValue = reactive({
    inFile = input$PrObj
    PrediObj = dataInput()
    x = PrediObj$data$get.x()
    size.data = dim(x)[1]
    validate(
      need(inFile != "", "Please upload your predictor object.")
    )
    instance = input$instance 
    validate(
      need(instance != "", "Please give an instance."),
      need(instance > 0, "Instance should be a positive integer.")
      #need(instance <  size.data, "Instance cannot exceed data size.")
    )
   
    sampleSize = input$nr_sv
    validate(
      need(sampleSize != "", "Please give a sample size."),
      need(sampleSize > 0, "Sample size should be a positive integer.")
      
    )
    Seed = input$seed
    validate(
      need(Seed != "", "Please give a seed."),
      need(Seed > 0, "Seed should be a positive integer.")
    )
    PrediObj = readRDS(inFile$datapath)
    X = as.data.frame(x)
    x.interest = X[instance,]
    model_data = Predictor$new(PrediObj$model, data = X)
    set.seed(seed = Seed)
    shapley = Shapley$new(predictor = model_data, x.interest = x.interest, sample.size = sampleSize)
    
  })
  
  
  # Show shapley values of the specified instance in a data table
  output$shapleyValue = renderDT({
    shapley = shapleyValue()
    shapley$results[,c(1, 4, 2, 3)]
  })
  
  # Show the plot of shapley values of the specified instance in a data table
  output$shapleyValuePlot = renderPlot({
    # input$goPlot 
    # withProgress(message = 'Making plot', value = 0,  shapley = shapleyValue())
    shapley = shapleyValue()
    shapley$plot()
  })
  
  # output$pred = renderText({
  #   inFile = input$PrObj
  #   if (is.null(inFile))
  #     return(NULL)
  #   # https://stackoverflow.com/questions/20875081/properly-rendering-sparklines-in-a-datatable 
  #   # input$Prdi  will be NULL initially. After the user selects
  #   # and uploads a file.
  #   PrediObj = dataInput()
  #   X = as.data.frame(PrediObj$data$get.x())
  #   x.interest = X[6,]
  #   model_data = Predictor$new(PrediObj$model, data = X)
  #   shapley = Shapley$new(predictor = model_data, x.interest = x.interest, sample.size = 100)
  #   pred_y = sh$y.hat.interest
  #   HTML(paste("The predicted of y is ","<font size='+2'>", round(pred_y, 2), 
  #              "</font>", "per unit. </br>"
  #   ))
  # })
  

  
}






shinyApp(ui, server)
