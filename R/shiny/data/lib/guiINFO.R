

guiINFO <- function()
{
	fluidRow(
      navlistPanel(widths=c(3,9),
          "Navigation",
          tabPanel("About",
            includeMarkdown("www/About.md")
          ),
          tabPanel("Input",
            includeMarkdown("www/Input.md")
          ),
          tabPanel("SRA",
            includeMarkdown("www/StockReductionAnalysis.md")
          )
  #         tabPanel("Equilibrium",
  #           # includeMarkdown("www/Equilibrium.md")
  #         ),
  #         tabPanel("MSE"),
  #         tabPanel("OMI",
  #           # includeMarkdown("www/OMI.md")
  #         ),
  #         "----",
  #         tabPanel("MAP")
      )
    )
}


