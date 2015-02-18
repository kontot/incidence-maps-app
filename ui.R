

shinyUI(pageWithSidebar(
#    theme = "bootstrap.css", 
  
  titlePanel(""),
  
  
  sidebarPanel(
     
    conditionalPanel(condition="input.conditionedPanels==1",
                     div(img(src = "Folkhalsomyndigheten_C_RGB.png", height = 90, width = 120),style="text-align: center;"), 
                     br(),
                     br(),
                     br(),
                     p("Utvecklat av:"), 
                     p("Ilias Galanis"),
                     p(tags$a(href='mailto:ilias.galanis@folkhalsomyndigheten.se', "ilias.galanis@folkhalsomyndigheten.se",style = "font-family: 'times'; font-si11pt")),
                     p("Achilleas Tsoumanis"),
                     
                     
                     p(tags$a(href='mailto:achilleas.tsoumanis@folkhalsomyndigheten.se', "achilleas.tsoumanis@folkhalsomyndigheten.se"),style = "font-family: 'times'; font-si11pt"),
                     p("Enhet för Statistik och Övervakning"), 
                     p("Version 1.0 (2015-01-31)")
    ),     
    conditionalPanel(condition="input.conditionedPanels==2",
                     fileInput('file1', 'Choose CSV File',
                               accept=c('text/csv', 
                                        'text/comma-separated-values,text/plain', 
                                        '.csv'))),
    
    fluidRow(
    conditionalPanel(condition="input.conditionedPanels==4",
                     numericInput("n_breaks", "Number of groups in map:", 4),
                     wellPanel(textInput("plot_title", "Graph Title:", ""),
                               textInput("legend_title", "Legend Title:", ""),
#                      actionButton("submit",label="Submit")),
                     htmlOutput("breaks")
#                      uiOutput("labels")
),
#                    checkboxGroupInput("checkGroup", 
#                    label = h6("Use the above values as graph legends?"), 
#                    choices = list("Yes" = 1, 
#                                   "No" = 2),
#                    selected = 1),
#                      jscolorInput("colorid"),
                     br(),
                     br(),
                     radioButtons(inputId="var1",label="Select the file type",choices=list("png", "jpeg")),
                     downloadButton('downloadPlot', 'Download Plot'))
    )

  , width =4), 
  mainPanel(
     tabsetPanel(
       
      tabPanel("Introduktion", textOutput("intro"), 
               h1("Om mjukaran"),
               p("Den här mjukvaran är ett verktyg för att skapa lätt och snabbt incidens kartor för Sverige, per län. Mer specifikt, mjukvaran kan:"),
               p("Läsa in en data fil"),
               p("Presentera data som en tabell"), 
               p("Skatta basala statistik om incidensen (min, max, medelvärde, median, kvartiler)"), 
               p("Skapa interaktiva incidens kartor. Användaren kan ändra färger samt antal grupper"), 
               p("Spara kartorna som en bild (*.png eller *.jpeg)"),
               br(),
               
               h1("Instruktioner"),
               p("1. Data måste sparas först som en csv-fil. "),
               p("2. Filen måste innehålla åtminstone två kolumner: 'Län' och 'Incidens'. OBS: Kolumnnamnen måste stavas exakt likadant som föreslagen här."), 
               p("3. Incidensen måste skattas som en siffra. Kommatecken (,) får användas som decimaltecken. "),
               p("4. Lännamnen måste stavas så här: Stockholm, Uppsala, Södermanland, Östergötland, Jönköping, 
                  Kronoberg, Kalmar, Gotland, Blekinge, Skåne, Halland, Västra Götaland, Värmland, Örebro, Västmanland, Dalarna, 
                  Gävleborg, Västernorrland, Jämtland, Västerbotten, Norrbotten")
               
               , value=1), 
      tabPanel("Data",p(strong(textOutput("title", container = span)),align="center"),
               verbatimTextOutput("summary"),
               tableOutput('contents'),value=2,align="center"), 
      tabPanel("Map", plotOutput("map",width="100%",height="100%"), value=4), 

      
      id = "conditionedPanels"))
     
  
)
)
