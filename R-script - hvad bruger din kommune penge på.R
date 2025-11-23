##-----------------------------------------------------------------------------------------##
####HVAD BRUGER DIN KOMMUNE PENGENE PÅ?###
##-----------------------------------------------------------------------------------------##

##-----------------------------------------------------------------------------------------##
###Trin 1: indlæs nødvendige pakker
##-----------------------------------------------------------------------------------------##

# Indlæs nødvendige pakker
library(shiny) # Pakke til interaktive web-apps
library(ggplot2) # Pakke til grafer
library(dplyr) # Pakke til datamanipulation
library(plotly) # Pakke til interaktive grafer
library(readr) # Pakke til at læse CSV-filer


##-----------------------------------------------------------------------------------------##
###Trin 2: Hente data###
##-----------------------------------------------------------------------------------------##

# Læs hver fil og tilføj år-kolonne
data_2016 <- read_csv("data/2016.csv", locale = locale(encoding = "latin1"), col_names = FALSE)
names(data_2016) <- c("Prisenhed", "Udgiftsart", "Politikområde", "Kommune", "Beløb")
data_2016$År <- 2016

data_2017 <- read_csv("data/2017.csv", locale = locale(encoding = "latin1"), col_names = FALSE)
names(data_2017) <- c("Prisenhed", "Udgiftsart", "Politikområde", "Kommune", "Beløb")
data_2017$År <- 2017

data_2018 <- read_csv("data/2018.csv", locale = locale(encoding = "latin1"), col_names = FALSE)
names(data_2018) <- c("Prisenhed", "Udgiftsart", "Politikområde", "Kommune", "Beløb")
data_2018$År <- 2018

data_2019 <- read_csv("data/2019.csv", locale = locale(encoding = "latin1"), col_names = FALSE)
names(data_2019) <- c("Prisenhed", "Udgiftsart", "Politikområde", "Kommune", "Beløb")
data_2019$År <- 2019

data_2020 <- read_csv("data/2020.csv", locale = locale(encoding = "latin1"), col_names = FALSE)
names(data_2020) <- c("Prisenhed", "Udgiftsart", "Politikområde", "Kommune", "Beløb")
data_2020$År <- 2020

data_2021 <- read_csv("data/2021.csv", locale = locale(encoding = "latin1"), col_names = FALSE)
names(data_2021) <- c("Prisenhed", "Udgiftsart", "Politikområde", "Kommune", "Beløb")
data_2021$År <- 2021

data_2022 <- read_csv("data/2022.csv", locale = locale(encoding = "latin1"), col_names = FALSE)
names(data_2022) <- c("Prisenhed", "Udgiftsart", "Politikområde", "Kommune", "Beløb")
data_2022$År <- 2022

data_2023 <- read_csv("data/2023.csv", locale = locale(encoding = "latin1"), col_names = FALSE)
names(data_2023) <- c("Prisenhed", "Udgiftsart", "Politikområde", "Kommune", "Beløb")
data_2023$År <- 2023

data_2024 <- read_csv("data/2024.csv", locale = locale(encoding = "latin1"), col_names = FALSE)
names(data_2024) <- c("Prisenhed", "Udgiftsart", "Politikområde", "Kommune", "Beløb")
data_2024$År <- 2024

# Sæt alle år sammen til ét datasæt
kommunaleudgifter_data <- bind_rows(data_2016, data_2017, data_2018, data_2019, data_2020, 
                                    data_2021, data_2022, data_2023, data_2024)

# Konverter Beløb til numerisk
kommunaleudgifter_data$Beløb <- as.numeric(kommunaleudgifter_data$Beløb)

# Fjern numre fra politikområder (fx "0.28.20 Grønne områder" → "Grønne områder")
kommunaleudgifter_data$Politikområde <- sub("^[0-9.]+\\s+", "", kommunaleudgifter_data$Politikområde)

##-----------------------------------------------------------------------------------------##
###Del 3: SHINY APP - UI###
##-----------------------------------------------------------------------------------------##

# Opretter hovedstruktur med navigationsfaner samt navngivning af siden
ui <- navbarPage(
  "Hvad bruger din kommune pengene på?",
  
  ##SIDE 1: UDVIKLINGEN I KOMMUNALE UDGIFTER##

  # Opretter første fane samt navngivning af denne fane
  tabPanel(
    "Udviklingen i kommunale udgifter",

    
    # Ændring af skrifttype
    tags$head(
      tags$style(HTML("
      * {
        font-family: 'Helvetica', 'Arial', sans-serif !important;
      }
    "))
    ),
    
        
    # Del siden i to: sidebar (venstre) og mainPanel (højre)
    sidebarLayout(
      
      # Definerer venstre side
      sidebarPanel(
        width = 3,
        
        # Tilføj drop-down til valg af kommune, der kan vælges flere kommuner
        selectInput(
          inputId = "valgte_kommuner",      
          label = "Vælg kommuner:",         
          choices = NULL,                   
          multiple = TRUE                   
        ),
        
        # Tilføj drop-down til valg af politikområde, der kan kun vælges et politikområde ad gangen
        selectInput(
          inputId = "valgt_politikområde",
          label = "Vælg politikområde:",
          choices = NULL,                   
          multiple = FALSE                 
        )
      ), 
      
      
      # Definer højre side samt oprettelse af graf
      mainPanel(
        width = 9,
        plotlyOutput("udgifts_graf", height = "500px")
      )
    )
  ),

  
  ##SIDE 2: OM DATA##
  # Opretter anden fane samt navngivning af denne
  tabPanel(
    "Om data og metode",
    
    # Opretter række og kolonner
    fluidRow(
      column(
        width = 10,
        offset = 1,
        
        # Opretter overskrift og linjeskift
        h2("Om data og metode"),
        br(),
        
        # Opretter underoverskrift og tekstafsnit samt linjeskift
        h3("Datakilde"),
        p("Data fra Danmarks Statistik, tabel REGK31: Kommunernes regnskaber for hovedkonto 0-6."),
        p("Alle beløb er i kroner per indbygger, løbende priser (ikke inflationsjusteret)."),
        br(),
        
        # Opretter ny underoverskrift og tekstafsnit samt linjeskift
        h3("Metode"),
        p("Dataene viser kommunale udgifter per indbygger fra 2016 til 2024."),
        p("Udgifterne er opgjort som nettobeløb (udgifter minus indtægter)."),
        p("Beløbene er ikke justeret for inflation og vises i løbende priser."),
        br(),
        
        # Opretter ny underoverskrift og punktliste 
        h3("Politikområder"),
        tags$ul(
          tags$li("0 - Byudvikling, bolig- og miljøforanstaltninger"),
          tags$li("2 - Transport og infrastruktur"),
          tags$li("3 - Undervisning og kultur"),
          tags$li("4 - Sundhedsområdet"),
          tags$li("5 - Sociale opgaver og beskæftigelse"),
          tags$li("6 - Fællesudgifter og administration")
        )
      )
    )
  )
)

##-----------------------------------------------------------------------------------------##
###Del 4: SERVER###
##-----------------------------------------------------------------------------------------##

# Definerer server-funktionen og tilføjer observatør for at opdatere dropdownmenuer
server <- function(input, output, session) {
  
  # Tjek om data er indlæst korrekt
  if(nrow(kommunaleudgifter_data) == 0) {
    stop("Ingen data indlæst!")
  }
  
  # Henter alle kommuner og sorterer dem alfabetisk
  kommuner_liste <- sort(unique(kommunaleudgifter_data$Kommune))
  
  # Fjerner "Hele landet" fra listen
  kommuner_liste <- kommuner_liste[kommuner_liste != "Hele landet"]
  
  # Henter alle politikområder
  politikområder_liste <- unique(kommunaleudgifter_data$Politikområde)
  
  # Debug: Print til konsollen for at se om data er der
  print(paste("Antal kommuner:", length(kommuner_liste)))
  print(paste("Antal politikområder:", length(politikområder_liste)))
  print("Første 5 kommuner:")
  print(head(kommuner_liste, 5))
  print("Første 5 politikområder:")
  print(head(politikområder_liste, 5))
  
  # Opdater dropdown-menuer når sessionen starter
  observe({
    updateSelectInput(session, "valgte_kommuner", 
                      choices = kommuner_liste, 
                      selected = head(kommuner_liste, 3))
    
    updateSelectInput(session, "valgt_politikområde", 
                      choices = politikområder_liste, 
                      selected = politikområder_liste[1])
  })
  
  # Opretter grafen og opdaterer automatisk når der vælges noget nyt
  output$udgifts_graf <- renderPlotly({
    
    # Kræver at bruger har valgt både kommuner og politikområde
    req(input$valgte_kommuner, input$valgt_politikområde)
    
    # Filtrerer dataen til kun at vise de valgte kommuner og det valgte politikområde
    filtreret_data <- kommunaleudgifter_data %>%
      filter(
        Kommune %in% input$valgte_kommuner,
        Politikområde == input$valgt_politikområde
      )
    req(nrow(filtreret_data) > 0)
    
    # Opretter en ggplot graf med år på x-aksen og beløb på y-aksen
    p <- ggplot(filtreret_data, aes(x = År, y = Beløb, color = Kommune, group = Kommune)) +
      
      # Tilføjer linjer mellem datapunkterne
      geom_line(linewidth = 1.2) +
      
      # Tegner punkter på hver observation
      geom_point(size = 3) +
      
      # Vælger tema 
      theme_minimal(base_size = 14, base_family = "Helvetica") +
      
      # Sætter titler på graf og akser
      labs(title = input$valgt_politikområde, x = "År", y = "Kroner per indbygger", color = "Kommune") +
      
      # Formaterer y-aksen
      scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
      
      # Placerer legend til højre og gør titlen fed
      theme(legend.position = "right", plot.title = element_text(size = 16, face = "bold"))
    
    # Konverterer ggplot til interaktiv plotly graf med hover-effekt
    ggplotly(p, tooltip = c("x", "y", "colour")) %>% 
      layout(hovermode = "x unified") %>%
      config(displayModeBar = FALSE)
  })
}

##-----------------------------------------------------------------------------------------##
###DEL 5: KØR APPEN###
##-----------------------------------------------------------------------------------------##

# Starter Shiny appen
shinyApp(ui = ui, server = server)




