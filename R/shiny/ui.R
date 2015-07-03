source("helpers.R")








shinyUI(fluidPage(
        navbarPage("DPSAM",
                   id="nav",
                   # footer=img(src="iphclogo.png",  height = 60, width = 60),
  
  # ---------------------------------------- #
  # ABOUT INTERFACE
  # ---------------------------------------- #
  tabPanel("INFO",
      guiINFO()
  ),
  
  # ---------------------------------------- #
  # STOCK INTERFACE
  # ---------------------------------------- #
  tabPanel("INPUT",
      guiSTOCK()
  ),

  # ---------------------------------------- #
  # SRA INTERFACE
  # ---------------------------------------- #
  tabPanel("SRA",
      guiSRA()
  )

   


)))