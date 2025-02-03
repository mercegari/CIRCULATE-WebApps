
rm(list=ls())

# Load packages
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(ggplot2)
library(ggpubr)
library(shiny)
library(rsconnect)
library(BBmisc)
library(ggmap)
library(hms)
#library(patchwork)
library(cowplot)
library(grid)
library(colorspace)

# Read Data from RData
load("CIRCULATE-maps.RData")
load("CIRCULATE-Atmos-Sensors-1r-mostreig.RData")

d <- d %>%
  rename(Codi = Code) %>%
  rename(Localització = Localitzacio) %>%
  rename(Mediana = median) %>%
  mutate(Aparell = ifelse(Instrument %in% "Atmotube", Codi, Localització)) %>%
  filter((Data <= "2024-05-28 11:00:00 UTC" & Institut %in% "Molins de Rei") |
           (Data >= "2024-06-06 08:10:58 UTC" & Institut %in% "Badalona")) %>%
  filter(Data > "2024-05-21 13:30:00 UTC" & Data < "2024-06-13 12:00:00 UTC")

M.molins <- ggmap(molins.map) + 
  theme_minimal() +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(1, 0, 0, 0), "cm"))
M.badalona <- ggmap(badalona.map) + 
  theme_minimal() +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))


############################################################### Shiny starts

# Define User Interface
ui <- fluidPage(
  # Give the page a title
  titlePanel("Projecte CIRCULATE - Dades provinents dels sensors"),
  # Generate a row with a sidebar
  sidebarLayout(
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("Instrument", "Instrument:", 
                  choices=unique(d$Instrument)),
      hr(),
      selectInput("Mesura", "Mesura:", 
                  choices=unique(d$Mesura)),
      hr(),
      selectInput("Institut", "Institut:",
                  choices=unique(d$Institut)),
      hr(),
      #  checkboxGroupInput("Aparell", "Aparell:",
      #                     unique(d$Aparell),
      #                     choices=unique(d$Aparell)),
      # #      checkboxGroupInput("Visualitzacio", "Visualitzacio:",
      # #                        c("Dades crues", "Mitjanes"),
      # #                       choices=c("Dades crues", "Mitjanes")),
      # hr(),
      helpText(paste0("Primer Mostreig: Maig-Juny 2024")),
      hr()
      ),
    # Specify the main panel
    mainPanel(
      plotOutput("plot", height="600px"),
      plotOutput("mapa", height="600px")
      )
    )
  )

# Define Server
server <- function(input, output) {
  file.text <- reactive({
    switch(input$type, "csv" = "csv", "RData" = "RData")
  })
  # Plot showing the results
  output$plot <- renderPlot({
    if("Atmotube" %in% input$Instrument){
      d.fig <- d %>%
        filter(Instrument %in% input$Instrument) %>%
        filter(Mesura %in% input$Mesura) %>%
        filter(Institut %in% input$Institut)
    
    d.fig %>%
      ggplot(aes(x=Data, y=Valor, color=Codi)) +
      geom_line(alpha=0.5) +
      facet_grid(~Mesura, scales="free_x") +
      theme_bw() + 
      scale_y_log10() +
      scale_x_datetime(date_labels = "%d/%m") +
      geom_text(data = . %>%
                  filter(Data == min(Data)) %>%
                  select(Data) %>%
                  distinct() %>%
                  mutate(Valor = 25), hjust = -0.1, vjust = -1,
                aes(label = "Nivells màxims UE"), color = "darkred") +
      geom_text(data = . %>%
                  filter(Data == min(Data)) %>%
                  select(Data) %>%
                  distinct() %>%
                  mutate(Valor = 5), hjust = -0.1, vjust = -1,
                aes(label = "Nivells recomanats OMS"), color = "darkblue") +
      geom_hline(yintercept=25, lty=4, color="darkred") +
      geom_hline(yintercept=5, lty=4, color="darkblue") +
      xlab("") + ylab("Concentració (microgram/m3)") +
      ggtitle("Mesura al llarg d'una setmana") +
      scale_color_discrete_qualitative(palette = "Dark") +
      facet_wrap(~Codi, ncol=4) +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14),
            plot.title = element_text(size = 14, face = "bold"),
            legend.title = element_text(size=12),
            legend.text = element_text(size=12),
            legend.position="none")
    
    } else {
      d.fig <- d %>%
        filter(Instrument %in% input$Instrument) %>%
        filter(Mesura %in% input$Mesura) %>%
        filter(Institut %in% input$Institut)
      
      d.fig %>%
        ggplot(aes(x=Data, y=Valor, color=Localització)) +
        geom_line(alpha=0.5) +
        #facet_grid(~Mesura, scales="free_x") +
        theme_bw() + 
        scale_y_log10() +
        scale_x_datetime(date_labels = "%d/%m") +
        geom_text(data = . %>%
                    filter(Data == min(Data)) %>%
                    select(Data) %>%
                    distinct() %>%
                    mutate(Valor = 25), hjust = -0.1, vjust = -1,
                  aes(label = "Nivells màxims UE"), color = "darkred") +
        geom_text(data = . %>%
                    filter(Data == min(Data)) %>%
                    select(Data) %>%
                    distinct() %>%
                    mutate(Valor = 5), hjust = -0.1, vjust = -1,
                  aes(label = "Nivells recomanats OMS"), color = "darkblue") +
        geom_hline(yintercept=25, lty=4, color="darkred") +
        geom_hline(yintercept=5, lty=4, color="darkblue") +
        xlab("") + ylab("Concentració (microgram/m3)") +
        ggtitle("Mesura al llarg d'una setmana") +
        scale_color_discrete_qualitative(palette = "Dark") +
        facet_wrap(~Localització, ncol=2) +
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=14),
              plot.title = element_text(size = 14, face = "bold"),
              legend.title = element_text(size=12),
              legend.text = element_text(size=12),
              legend.position = "none")
      }
  })
  output$mapa <- renderPlot({
    if("Atmotube" %in% input$Instrument){
      if("Molins de Rei" %in% input$Institut){
        d.atmo.molins <- d %>%
          filter(Instrument %in% "Atmotube") %>%
          filter(Institut %in% "Molins de Rei") %>%
          filter(Mesura %in% input$Mesura) %>%
          filter(!Codi %in% "13") %>%
          filter(!is.na(Latitude)) %>% 
          filter(Setmana %in% "Laborable") %>%
          filter(Latitude >=41.4) %>%
          filter(Latitude <= 41.425) %>%
          filter(Longitude >= 2.01) %>%
          filter(Longitude <= 2.03) %>%
          droplevels()
        M.molins +
          geom_point(data = d.atmo.molins, 
                     aes(x = Longitude, y = Latitude, 
                         color = Valor), alpha=0.7, size = 2) +
          scale_color_viridis_c(trans="log10", direction=-1) +  
          labs(x = "Longitud", y = "Latitud") +  
          theme_minimal() + 
          facet_grid(~Franja) +
          ggtitle("Mapa de Molins de Rei") +
          theme(plot.title = element_text(size = 14, face = "bold"))
      } else {
        d.atmo.badalona <- d %>%
          filter(Instrument %in% "Atmotube") %>%
          filter(Institut %in% "Badalona") %>%
          filter(Mesura %in% input$Mesura) %>%
          filter(!is.na(Latitude)) %>% 
          filter(Setmana %in% "Laborable") %>%
          droplevels()
        M.badalona +
          geom_point(data = d.atmo.badalona, 
                     aes(x = Longitude, y = Latitude, 
                         color = Valor), alpha=0.7, size = 2) +
          scale_color_viridis_c(trans="log10", direction=-1) +  
          labs(x = "Longitud", y = "Latitud") +  
          theme_minimal() + 
          facet_wrap(~Franja, ncol=2) +
          ggtitle("Mapa de Badalona") +
          theme(plot.title = element_text(size = 14, face = "bold"))
      }
  } else {
    if("Molins de Rei" %in% input$Institut){
      d.sensor.molins <- d %>%
        filter(Instrument %in% "Smart Citizen Kit") %>%
        filter(Institut %in% "Molins de Rei") %>%
        filter(Mesura %in% input$Mesura) %>%
        filter(Setmana %in% "Laborable") %>%
        droplevels()
      # Plànol
      background.image <- png::readPNG("www/planol-institut-molins.png")
      background.grob <- rasterGrob(background.image, 
                                    width = unit(0.8, "npc"),  height = unit(1, "npc"))
      f1 <- ggplot() +
        annotation_custom(background.grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
        theme_void() +
        ggtitle("Plànol Institut Bernat el Ferrer") +
        theme(plot.title = element_text(size = 14, face = "bold")) 
      f2 <- d.sensor.molins %>%
        dplyr::select(Localització, Franja, Mediana) %>%
        unique() %>%
        ggplot(aes(y=Franja, x=Mediana, color=Mediana, fill=Mediana)) +
        geom_bar(stat = "identity") +
        facet_wrap(~Localització, ncol=1) +
        scale_color_viridis_c(direction=-1) +
        scale_fill_viridis_c(direction=-1) +
        theme_minimal() +
        xlab("Concentració (microgram/m3)") + ylab("") +
        ggtitle("Gràfic de barres (Sensors a l'Institut Bernat el Ferrer)") +
        theme(plot.title = element_text(size = 14, face = "bold")) +
        facet_wrap(~Localització, ncol=2)
      plot_grid(f2, f1, nrow = 2)
    } else {
      d.sensor.badalona <- d %>%
        filter(Instrument %in% "Smart Citizen Kit") %>%
        filter(Institut %in% "Badalona") %>%
        filter(Mesura %in% input$Mesura) %>%
        filter(Setmana %in% "Laborable") %>%
        droplevels()
      f1 <- d.sensor.badalona %>%
         dplyr::select(Localització, Franja, Mediana) %>%
         unique() %>%
         ggplot(aes(y=Franja, x=Mediana, color=Mediana, fill=Mediana)) +
         geom_bar(stat = "identity") +
         facet_wrap(~Localització, ncol=3) +
         scale_color_viridis_c(direction=-1) +
         scale_fill_viridis_c(direction=-1) +
         theme_minimal() +
         xlab("Concentració (microgram/m3)") + ylab("") +
         ggtitle("Gràfic de barres (Sensors a Badalona)") +
         theme(plot.title = element_text(size = 14, face = "bold"))
       f2 <- M.badalona +
         geom_point(data = d.sensor.badalona, 
                    aes(x = Longitude, y = Latitude, 
                        color = Mediana), alpha=0.7, size = 3) +
         geom_point(data = d.sensor.badalona,
                    aes(x = Longitude, y = Latitude), 
                    shape=1, color="black", stroke=1.5, size = 4) +
         scale_color_viridis_c(trans="log10", direction=-1) +  
         labs(x = "Longitud", y = "Latitud") +  
         theme_minimal() + 
         facet_wrap(~Franja, ncol=4) +
         ggtitle("Mapa de Badalona") +
         theme(plot.title = element_text(size = 14, face = "bold"))
       plot_grid(f1, f2, nrow = 2, rel_heights = c(1, 1))
    }
  }
  })
}

# Run ShinyApp
shinyApp(ui, server)
