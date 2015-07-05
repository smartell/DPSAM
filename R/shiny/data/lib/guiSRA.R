

guiSRA <- function()
{
  data <- mData
   sidebarLayout(
   		sidebarPanel( h5("Stock Parameters"),
   			selectInput("stockname", h6("Stock ID:"),
            c("Namibian hake" = "nhake",
              "Demo" = "demo",
              "Example" = "example")),

   			sliderInput("sldr_fmsy",h6("Fmsy Range:"),
   			            min=0,max=1.0,value=c(0.05,0.25),step = 0.01),

   			sliderInput("sldr_cmsy",h6("MSY Range:"),
   			            min=0,max=round_any(2*max(data$catch),100),
                    value=range(round_any(data$catch,10))),

   			sliderInput("sldr_natm",h6("Natural Mortality Range:"),
   			            min=0,max=1.0,value=c(0.1,0.3),step=0.05),
            hr(),
            actionButton("actionRunSRA", label = "Run SRA")
   		),
    
    	mainPanel("main panel",
         plotOutput("plotSRA",height = "550px")
      )
   )
   
} 