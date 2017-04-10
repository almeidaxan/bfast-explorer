library(shiny)

shinyUI(
	navbarPage(
		title = div(
			em(strong("BFAST Explorer")),
			HTML("<small>by Alexandre E. Almeida</small>")
		),
		windowTitle = "BFAST Explorer",
		id = "navbar",
		header = {
			# load styles.css file with custom styles
			tags$head(includeCSS("www/styles.css"))
		},

		# ---------------------------------------------------- MAP TAB ----

		tabPanel(
			title = "Map",
			icon = icon(name = "map-marker", lib = "font-awesome", class = "fa-lg"),
			# set up shinyjs
			useShinyjs(),
			div(
				id = "map-leaflet",
				# render a leaflet map on the background
				leafletOutput(
					outputId = "leaf",
					height = "100%"
				),
				# create a toolbar on top of the map
				fixedPanel(
					id = "map-toolbar",
					class = "panel panel-default",
					top = 60,
					bottom = "auto",
					right = 250,
					left = 250,
					# map search query
					fluidRow(
						column(
							12,
							textInput(
								inputId = "select_search",
								label = NULL,
								width = "100%",
								placeholder = "Search for a location..."
							)
						)
					),
					fluidRow(
						# 'clear all markers' button
						column(
							4,
							bsButton(
								inputId = "action_clearMarkers",
								label = "Clear Markers",
								style = "danger",
								icon = icon(name = "trash", lib = "font-awesome"),
								width = "100%"
							)
						),
						# select which satellite products to download
						column(
							4,
							selectInput(
								inputId = "select_satGet",
								label = NULL,
								width = "100%",
								choices = c(""),
								multiple = T
							)
						),
						# 'get data' button
						column(
							4,
							bsButton(
								inputId = "action_getTs",
								label = "Get Data",
								style = "primary",
								icon = icon(name = "download", lib = "font-awesome"),
								width = "100%"
							),
							# tooltip with data preprocessing description
							bsTooltip(
								id = "action_getTs",
								title = '<div align="justify"><p>The Landsat "Surface Refle- ctance" (SR) product is, by construction, already atmosphe- rically corrected.</p> In addition, the downloaded data is also filtered for cloud effects by using the CFMask algorithm.</div>',
								placement = "bottom",
								trigger = "hover",
								options = list(container = "body")
							)
						)
					),
					# output text with download results
					htmlOutput(outputId = "text_getTs")
				)
			)
		),

		# ----------------------------------------------- ANALYSIS TAB ----

		tabPanel(
			title = "Analysis",
			icon = icon(name = "bar-chart", lib = "font-awesome", class = "fa-lg"),
			fluidPage(

				# ----------------------------------------- TIME SERIES ---

				fixedRow(
					column(
						3,
						h3(class = "text-heading", "Visualization"),
						# select which satellite products to plot
						selectInput(
							inputId = "select_satPlot",
							label = "Satellite product",
							width = "100%",
							choices = NULL,
							multiple = T
						),
						# select which band/index to plot
						selectInput(
							inputId = "select_index",
							label = "Data",
							width = "100%",
							choices = NULL
						),
						# add a tooltip to select_index warning about mixing bands from different satellites
						bsTooltip(
							id = "select_index",
							title = '<div align="justify">Be careful when comparing <strong>bands</strong> from different satellites.<p></p><hr><em>For more information about Landsat bands, please visit <a href="https://landsat.usgs.gov/what-are-band-designations-landsat-satellites" target="_blank">this link</a>.</em></div>',
							placement = "right",
							trigger = "focus",
							options = list(container = "body")
						),
						# select which change detection algorithm to run
						selectInput(
							inputId = "select_chgDet",
							label = "Change detection algorithm",
							width = "100%",
							choices = c("Select the change detection algorithm..." = "", "bfastmonitor", "bfast01", "bfast")
						),
						# tooltip with brief explanations about the change detection algorithms
						bsTooltip(
							id = "select_chgDet",
							title = '<div align="justify"><strong><div class="text-highlight">bfastmonitor</div></strong>Monitoring for the first break at the end of the time series.<p></p><strong><div class="text-highlight">bfast01</div></strong>Checking for one major break in the time series.<p></p><strong><div class="text-highlight">bfast</div></strong>Time series decomposition and multiple breakpoint detection in trend and seasonal components.<p></p><hr><em>For more information about these algorithms, please visit <a href="http://bfast.r-forge.r-project.org/" target="_blank">this link</a>.</em></div>',
							placement = "right",
							trigger = "focus",
							options = list(container = "body")
						),
						# buttons to download the data and the plot figures
						h5(style = "cursor:default;", strong("Download")),
						downloadButton(
							outputId = "action_downloadDataRaw",
							label = "Data",
							class = "btn-primary"
						),
						bsButton(
							inputId = "action_downloadPlotRaw",
							label = "Plot",
							style = "primary",
							icon = icon(name = "download", lib = "font-awesome")
						)
					),
					# time series plot and its legend
					column(
						9,
						splitLayout(
							cellWidths = c("75%", "25%"),
							plotOutput(outputId = "plot_raw", height = 300),
							plotOutput(outputId = "plot_raw_legend", height = 300)
						)
					)
				),

				# ---------------------------------------- BFASTMONITOR ---

				conditionalPanel(
					condition = "input.select_chgDet == 'bfastmonitor' && input.select_satPlot != null",
					hr(),
					fluidRow(
						column(
							3,
							h3(class = "text-heading", "bfastMonitor"),
							helpText("Based on", a("bfmApp", href = "https://github.com/loicdtx/bfmApp", target = "_blank"), "by Loïc Dutrieux"),
							fluidRow(
								# formula parameter
								column(
									6,
									selectInput(
										inputId = "select_bfm_formula",
										label = "Formula",
										width = "100%",
										choices = c("trend + harmon", "trend", "harmon"),
										selected = "trend + harmon"
									)
								),
								# history period type parameter
								column(
									6,
									selectInput(
										inputId = "select_bfm_history",
										label = "History period type",
										width = "100%",
										choices = c("ROC", "BP", "all"),
										selected = "ROC"
									),
									# tooltip with brief explanations about the history period type parameter
									bsTooltip(
										id = "select_bfm_history",
										title = '<div align="justify"><strong><div class="text-highlight">ROC</div></strong>Reverse-ordered CUSUM.<p></p><strong><div class="text-highlight">BP</div></strong>Bai & Perron<br> breakpoint estimation.<p></p><strong><div class="text-highlight">all</div></strong>All available observations.</div>',
										placement = "right",
										trigger = "focus",
										options = list(container = "body")
									)
								)
							),
							fluidRow(
								# harmonic order parameter (only show if applicable)
								column(
									6,
									conditionalPanel(
										condition = "input.select_bfm_formula == 'harmon' | input.select_bfm_formula == 'trend + harmon'",
										sliderInput(
											inputId = "select_bfm_order",
											label = "Harmonic order",
											width = "100%",
											value = 1,
											min = 1,
											max = 9,
											step = 1
										)
									)
								),
								# start of monitoring period parameter
								column(
									6,
									dateInput(
										inputId = "select_bfm_monitor",
										label = "Start of monitoring",
										width = "100%",
										format = "yyyy-mm",
										startview = "decade"
									),
									# tooltip with brief explanations about the monitoring period default
									bsTooltip(
										id = "select_bfm_monitor",
										title = '<div align="justify">Defaults to the last<br> 30% of the data.</div>',
										placement = "bottom",
										options = list(container = "body")
									)
								)
							),
							# buttons to download the data and the plot figures
							h5(style = "cursor:default;", strong("Download")),
							downloadButton(
								outputId = "action_downloadDataBfm",
								label = "Results",
								class = "btn-primary"
							),
							bsButton(
								inputId = "action_downloadPlotBfm",
								label = "Plot",
								style = "primary",
								icon = icon(name = "download", lib = "font-awesome")
							)
						),
						# bfastmonitor plot and its legend
						column(
							9,
							splitLayout(
								cellWidths = c("75%", "25%"),
								plotOutput(outputId = "plot_bfm", height = 300),
								plotOutput(outputId = "plot_bfm_legend", height = 300)
							)
						)
					)
				),

				# --------------------------------------------- BFAST01 ---

				conditionalPanel(
					condition = "input.select_chgDet == 'bfast01' && input.select_satPlot != null",
					hr(),
					fluidRow(
						column(
							3,
							h3(class = "text-heading", "bfast01"),
							fluidRow(
								# formula parameter
								column(
									6,
									selectInput(
										inputId = "select_bf01_formula",
										label = "Formula",
										width = "100%",
										choices = c("trend + harmon", "trend", "harmon"),
										selected = "trend + harmon"
									)
								),
								# harmonic order parameter (only show if applicable)
								column(
									6,
									conditionalPanel(
										condition = "input.select_bf01_formula == 'harmon' | input.select_bf01_formula == 'trend + harmon'",
										sliderInput(
											inputId = "select_bf01_order",
											label = "Harmonic order",
											width = "100%",
											value = 1,
											min = 1,
											max = 1,
											step = 1
										)
									)
								)
							),
							# buttons to download the data and the plot figures
							h5(style = "cursor:default;", strong("Download")),
							downloadButton(
								outputId = "action_downloadDataBf01",
								label = "Results",
								class = "btn-primary"
							),
							bsButton(
								inputId = "action_downloadPlotBf01",
								label = "Plot",
								style = "primary",
								icon = icon(name = "download", lib = "font-awesome")
							)
						),
						# bfast01 plot and its legend
						column(
							9,
							splitLayout(
								cellWidths = c("75%", "25%"),
								plotOutput(outputId = "plot_bf01", height = 300),
								plotOutput(outputId = "plot_bf01_legend", height = 300)
							)
						)
					)
				),

				# ----------------------------------------------- BFAST ---

				conditionalPanel(
					condition = "input.select_chgDet == 'bfast' && input.select_satPlot != null",
					hr(),
					fluidRow(
						column(
							3,
							h3(class = "text-heading", "bfast"),
							fluidRow(
								# h parameter (minimal segment size)
								column(
									6,
									sliderInput(
										inputId = "select_bfast_h",
										label = "h",
										width = "100%",
										value = 0.15,
										step = 0.01,
										min = 0.00,
										max = 0.50
									),
									# explanation about what the h parameter means
									bsTooltip(
										id = "select_bfast_h",
										title = '<div align="justify">Minimal segment size between potentially detected breaks.</div>',
										placement = "right",
										options = list(container = "body")
									)
								),
								# season type parameter
								column(
									6,
									selectInput(
										inputId = "select_bfast_season",
										label = "Season type",
										width = "100%",
										choices = c("dummy", "harmonic", "none"),
										selected = "dummy"
									)
								)
							),
							# buttons to download the data and the plot figures
							h5(style = "cursor:default;", strong("Download")),
							downloadButton(outputId = "action_downloadDataBfast",
										   label = "Results",
										   class = "btn-primary"),
							bsButton(
								inputId = "action_downloadPlotBfast",
								label = "Plot",
								style = "primary",
								icon = icon(name = "download", lib = "font-awesome")
							)
						),
						# bfast plot and its legend
						column(
							9,
							splitLayout(
								cellWidths = c("75%", "25%"),
								plotOutput(outputId = "plot_bfast", height = 300),
								plotOutput(outputId = "plot_bfast_legend", height = 300)
							)
						)
					)
				)
			)
		),

		# ----------------------------------------------- TUTORIAL TAB ----

		tabPanel(
			title = "Tutorial",
			icon = icon(name = "question-circle", lib = "font-awesome", class = "fa-lg"),
			fluidPage(
				div(style = "max-width: 70%; margin-left: auto; margin-right: auto;",
					align = "justify",
					includeMarkdown("./md/tutorial.md")
				)
			)
		),

		# -------------------------------------------------- ABOUT TAB ----

		tabPanel(
			title = "About",
			icon = icon(name = "info-circle", lib = "font-awesome", class = "fa-lg"),
			fluidPage(
				div(
					style = "margin-left: auto; margin-right: auto; width: 600px;",
					h3(class = "text-heading", "About"),
					p(),
					div(
						align = "center",
						div(class = "image-highlight", a(img(src = "logo-tribes.png", height = "80px"), href = "http://www.e-tribes.com.br", target = "_blank")),
						p(),
						div(class = "image-highlight", a(img(src = "logo-unicamp.svg", height = "80px"), href = "http://www.unicamp.br/unicamp/english/", target = "_blank")),
						div(class = "image-highlight", a(img(src = "logo-wur.png", height = "120px"), href = "http://www.wur.nl/en.htm", target = "_blank"))
					),
					p(),
					HTML('This work is a product of the Tribes project, developed within the University of Campinas (UNICAMP) <a href="http://www.ic.unicamp.br/en" target="_blank">Institute of Computing</a> and the Wageningen University &amp; Research (WUR) <a href="http://www.wur.nl/en/Expertise-Services/Chair-groups/Environmental-Sciences/Laboratory-of-Geo-information-Science-and-Remote-Sensing.htm" target="_blank">Laboratory of Geo-information Science and Remote Sensing</a>.'),
					hr(),
					h3(class = "text-heading", "Authors"),
					p(),
					HTML('Alexandre Esteves Almeida<br>Prof. Dr. Jan Verbesselt<br>Prof. Dr. Ricardo da Silva Torres'),
					p(),
					span("Mail:"),
					a("almeida.xan@gmail.com"),
					hr(),
					h3(class = "text-heading", "Supported by"),
					p(),
					div(align = "center",
						div(class = "image-highlight", a(img(src = "logo-fapesp.svg", height = "40px"), href = "http://www.fapesp.br/en/", target = "_blank")),
						div(style = "display:inline;", HTML("&nbsp&nbsp")),
						div(class = "image-highlight", a(img(src = "logo-ms-research.png", height = "40px"), href = "http://www.fapesp.br/en/5392", target = "_blank")),
						div(style = "display:inline;", HTML("&nbsp&nbsp")),
						div(class = "image-highlight", a(img(src = "logo-cnpq.svg", height = "60px"), href = "http://cnpq.br/", target = "_blank"))
					)
				),
				HTML('
					<span class="fa-stack hidden-n" style="position: absolute; left: 1px;">
						<i class="fa fa-heart fa-stack-2x text-danger"></i>
						<strong class="fa-stack-1x fa-inverse" style="margin-top: -.05em;">N</strong>
					</span>
			 	')
			)
		)
	)
)
