source("helpers.R")








shinyUI(fluidPage(
        navbarPage("Data Poor Stock Assessment Methods (DPSAM)",
                   id="nav",
                   # footer=img(src="iphclogo.png",  height = 60, width = 60),
  
  # ---------------------------------------- #
  # ABOUT INTERFACE
  # ---------------------------------------- #
  tabPanel("INFO",
      guiINFO()
  ),
  


  # ---------------------------------------- #
  # SRA INTERFACE
  # ---------------------------------------- #
  tabPanel("SRA",
      guiSRA()
  )

   


)))