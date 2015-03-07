

guiSRA <- function()
{
   sidebarLayout(
   		sidebarPanel( h5("Stock Parameters"),
   			selectInput("stockname", "Stock ID:",
            c("Namibian hake" = "nhake",
              "Demo" = "demo",
              "Example" = "example")),

   			sliderInput("sldr_fmsy","Fmsy Range:",
   			            min=0,max=1.0,value=c(0.0,0.25),step = 0.05),

   			sliderInput("sldr_cmsy","MSY Range:",
   			            min=0,max=2*max(data$catch),value=range(data$catch)),

   			sliderInput("sldr_natm","Natural Mortality Range:",
   			            min=0,max=1.0,value=c(0.1,0.3),step=0.05),
            hr(),
            actionButton("actionRunSRA", label = "Run SRA")
   		),
    
    	mainPanel("main panel",
         plotOutput("plotSRA",height = "550px")
      )
   )
   
} 