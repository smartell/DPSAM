

guiSRA <- function()
{
  data <- mData
   sidebarLayout(
   		sidebarPanel( h5("Stock Parameters"),
   			# selectInput("stockname", h6("Stock ID:"),
      #       c("Namibian hake" = "nhake",
      #         "Demo" = "demo",
      #         "Example" = "example")),
        "Use sliders to adjust parameter range",
   			sliderInput("sldr_fmsy",h6("Fmsy Range:"),
   			            min=0,max=1.0,value=c(0.05,0.25),step = 0.01),

   			sliderInput("sldr_cmsy",h6("MSY Range:"),
   			            min=0,max=round_any(2*max(data$catch),100),
                    value=range(round_any(data$catch,10))),

   			sliderInput("sldr_natm",h6("Natural Mortality Range:"),
   			            min=0,max=1.0,value=c(0.1,0.3),step=0.01),
        
        wellPanel(
          fluidRow(
            column(6,
              actionButton("actionRunSRA", 
                           label = "Run SIR",
                           icon = icon("play"))
            ),
            column(6,
              numericInput("nI_nSIR",
                         label=h6("N samples"),
                         value=100,min=100,max=1e6,step=100)
            )
          )
        )     
   		),
    
    	mainPanel(
        tabsetPanel(type="pills",id="tabPlots",
          tabPanel(title="Priors",
            plotOutput("plotSRA",height = "550px")
          ),
          tabPanel(title="Posteriors",
            plotOutput("plotPosterior",height = "550px")
          )
        )
      )
   )
   
} 