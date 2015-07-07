guiSTOCK <- function()
{
	fluidRow(
		column(6,
			tabsetPanel(type="pills",id="stockpar",
				tabPanel("Data",
					renderDataInput()
				),
				tabPanel("Parameters",
					renderParameterInput()
				)
			)
		),
		column(6)
	)  

}


renderDataInput <- function()
{
	
	wellPanel(
		fluidRow(
		column(4,
			h5("Data input"),
			apply(dataDF[1:7,],1,function(p) 
			      {numericInput(p[1],h6(p[2]),p[3],p[4],p[5],p[6])})
		),
		column(8,
		    h5("Fisheries data"),
			tableOutput("mDataTable")
		)
	))
	
}

renderParameterInput <- function()
{
	wellPanel(
		fluidRow(
		column(4,
			h5("Parameter input"),
			sliderInput("tmp","Test",0,5,3,1),
			apply(parmDF,1,function(p) {sliderInput(p[1],p[2],as.numeric(p[["parmMin"]]),as.numeric(p[["parmMax"]]),as.numeric(p[["parmValue"]]),as.numeric(p[["parmStep"]]))})
		),
		column(8,
		    h5("Fisheries data"),
			tableOutput("vData")
		)
	))
}