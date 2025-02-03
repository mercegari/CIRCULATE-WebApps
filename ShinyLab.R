
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
#library(patchwork)
library(cowplot)

# Read Data from RData
load("CIRCULATE-1r-mostreig.RData")
load("CIRCULATE-maps.RData")

d <- d %>%
  mutate(Concentració = ifelse(Concentració <= 0, 0.0005, Concentració)) 
#global_colors = setNames(c("purple","orange"),unique(d$location))
d.cor <- d %>%
  filter(!Material %in% "Pegat") %>%
  droplevels() %>%
  dplyr::select(Compost, Mostra, Concentració, Institut, Material, Abreviatura, Grup) %>%
  spread(Material, Concentració)
d.pegat.molins <- d %>%
  filter(Material %in% "Pegat") %>%
  filter(Institut %in% "Molins de Rei")
d.pegat.badalona <- d %>%
  filter(Material %in% "Pegat") %>%
  filter(Institut %in% "Badalona")
M.molins <- ggmap(molins.map) + 
  geom_point(data = d.pegat.molins,
             aes(x = Longitude, y = Latitude), shape=1, color="black", stroke=1.5, size = 3) +
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
  geom_point(data = d.pegat.badalona,
             aes(x = Longitude, y = Latitude), shape=1, color="black", stroke=1.5, size = 3) +
  theme_minimal() +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))
margins <- range(d$Concentració)

############################################################### Shiny starts

# Define User Interface
ui <- fluidPage(
  # Give the page a title
  titlePanel("Projecte CIRCULATE - Dades analitzades al laboratori"),
  # Generate a row with a sidebar
  sidebarLayout(
  # Define the sidebar with one input
    sidebarPanel(
      selectInput("Material", "Tipus de mostra:", choices=unique(d$Material)),
      hr(),
      # selectInput("Material", "Comparar amb:", choices=c("No comparar", "Braçalet", "Orina")),
      # hr(),
      checkboxGroupInput("Institut", "Institut:",
                         unique(d$Institut),
                         choices = c("Molins de Rei", "Badalona")),
                         #selected("Molins Rei")),
      hr(),
      checkboxGroupInput("Grup", "Grups de compostos:", 
                         unique(d$Grup), choices=unique(d$Grup)),
      hr(),
      checkboxGroupInput("Visualitzacio", "Visualitzacio:",
                         c("Dades crues", "Medianes"),
                         choices=c("Dades crues", "Medianes")),
      hr(),
      helpText(paste0("Primer Mostreig: Maig-Juny 2024")),
      hr(),
      width=3
      # radioButtons('type', 'Format type:', choices='csv'),
      # downloadButton('downloadData', 'Download'),
      # hr()
      ),
    # Specify the main panel
    mainPanel(
     # textOutput("model"),
      plotOutput("plot", height="350px"),
      plotOutput("correlacio", height="600px")
#      plotOutput("mapa", height="450px")
      #tableOutput("calibration"),
      #tableOutput("blanks"),
      #tableOutput("samples")
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
      d.fig <- d %>%
        filter(Material %in% input$Material) %>%
        filter(Institut %in% input$Institut) %>%
        filter(Grup %in% input$Grup)
      d.fig.mitjanes <- d.fig %>%
        group_by(Abreviatura, Institut) %>%
        summarize(Mediana = exp(median(log(Concentració), na.rm=TRUE))) %>%
        ungroup()
      fig <- ggplot(d.fig) 
      if("Dades crues" %in% input$Visualitzacio) {
        fig <- fig +
          geom_point(aes(y=Abreviatura, 
                         x=Concentració, color=Institut),
                     alpha=0.8,
                     position=position_dodge(width=0.4)) 
      } 
      if("Medianes" %in% input$Visualitzacio) {
        fig <- fig +
          geom_point(data=d.fig.mitjanes, 
                     aes(y=Abreviatura, 
                         x=Mediana, group=Institut),
                     position=position_dodge(width=0.4),
                     color="black",
                     shape=0, size=4) +
          geom_point(data=d.fig.mitjanes, 
                     aes(y=Abreviatura, 
                         x=Mediana, color=Institut),
                     position=position_dodge(width=0.4),
                     shape=15, size=2.5)
      }
      fig <- fig + 
        theme_bw() +
        #scale_x_log10() +
        ylab("") + xlab("Concentració (ng/ml)") +
        scale_color_manual(values = c("Badalona"="purple", 
                                      "Molins de Rei"="#CC9900")) +
        expand_limits(x=margins) +
        scale_x_continuous(breaks=c(0.01, 0.1, 1, 10, 100), 
                           labels=c("0.01", "0.1", "1", "10", "100"),
                           trans="log10") +
        geom_vline(xintercept = 0.076, color="blue", lty=3) +
        geom_text(data = tibble(Text = "6PPD-Q \nen nens", pos = 0.076),
                  aes(x = pos, y = -Inf, label = Text), color="blue", #inherit.aes = F,
                  size = 3, vjust = -0.5, hjust = -0.1, nudge_x = 0) +
        geom_vline(xintercept = 0.4, color="blue", lty=3) +
        geom_text(data = tibble(Text = "6PPD-Q \nen adults", pos = 0.4),
                  aes(x = pos, y = -Inf, label = Text), color="blue", inherit.aes = F,
                  size = 3, vjust = -0.5, hjust = -0.1, nudge_x = 0) +
        ggtitle("Distribució de les concentracions") +
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=14),
              plot.title = element_text(size = 14, face = "bold"),
              legend.title = element_text(size=12),
              legend.text = element_text(size=12))
      fig
  })
  # Plot showing the correlation (Braçalet vs. Orina)
  output$correlacio <- renderPlot({
    if("Orina" %in% input$Material | "Braçalet" %in% input$Material){
      d.cor %>%
        filter(Grup %in% input$Grup) %>%
        ggplot(aes(x=Braçalet, y=Orina, color=Abreviatura)) +
        geom_point() + 
        theme_bw() +
        xlab("Concentració Braçalet (ng/ml)") +
        ylab("Concentració Orina (ng/ml)") +
        scale_x_continuous(breaks=c(0.01, 0.1, 1, 10, 100), 
                           labels=c("0.01", "0.1", "1", "10", "100"),
                           trans="log10") +
        scale_y_continuous(breaks=c(0.01, 0.1, 1, 10, 100), 
                           labels=c("0.01", "0.1", "1", "10", "100"),
                           trans="log10") +
        ggtitle("Correlació entre Braçalet i Orina") +
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=14),
              plot.title = element_text(size = 14, face = "bold"),
              legend.title = element_text(size=12),
              legend.text = element_text(size=12))
        
    } else {
  # Plot showing the map (Pegats)
        data1 <- d.pegat.molins %>%
          filter(Grup %in% "4OH-BTR (Producte Transformació)") %>%
          droplevels()
        p1 <- M.molins + 
          geom_point(data = data1,
                     aes(x = Longitude, y = Latitude, 
                         color = Concentració), size = 3) +
          scale_color_viridis_c(name = "", direction=-1) +  # Dynamic limits
          ggtitle("4OH-BTR")
        data2 <- d.pegat.molins %>%
          filter(Grup %in% "5Me-BTR (Benzotriazol)") %>%
          droplevels()
        p2 <- M.molins +
          geom_point(data = data2,
                     aes(x = Longitude, y = Latitude, 
                         color = Concentració), size = 3) +
          scale_color_viridis_c(name = "", direction=-1) +  # Dynamic limits
          ggtitle("5Me-BTR")
        data3 <- d.pegat.molins %>%
          filter(Grup %in% "6PPD-Quinona (Producte Transformació)") %>%
          droplevels()
        p3 <- M.molins +
          geom_point(data = data3,
                     aes(x = Longitude, y = Latitude, 
                         color = Concentració), size = 3) +
          scale_color_viridis_c(name = "", direction=-1) +  # Dynamic limits
          ggtitle("6PPD-Quinona") 
        data4 <- d.pegat.molins %>%
          filter(Grup %in% "NCBA (Benzotiazol)") %>%
          droplevels()
        p4 <- M.molins +
          geom_point(data = data4,
                     aes(x = Longitude, y = Latitude, 
                         color = Concentració), size = 3) +
          scale_color_viridis_c(name = "", direction=-1) +  # Dynamic limits
          ggtitle("NCBA") 
        data5 <- d.pegat.molins %>%
          filter(Grup %in% "FG (Amina cíclica)") %>%
          droplevels()
        p5 <- M.molins +
          geom_point(data = data5,
                     aes(x = Longitude, y = Latitude, 
                         color = Concentració), size = 3) +
          scale_color_viridis_c(name = "", direction=-1) +  # Dynamic limits
          ggtitle("FG")  
        data11 <- d.pegat.badalona %>%
          filter(Grup %in% "4OH-BTR (Producte Transformació)") %>%
          droplevels()
        p11 <- M.badalona + 
          geom_point(data = data11,
                     aes(x = Longitude, y = Latitude, 
                         color = Concentració), size = 3) +
          scale_color_viridis_c(name = "", direction=-1) +  # Dynamic limits
          ggtitle("4OH-BTR")
        data21 <- d.pegat.badalona %>%
          filter(Grup %in% "5Me-BTR (Benzotriazol)") %>%
          droplevels()
        p21 <- M.badalona + 
          geom_point(data = data21,
                     aes(x = Longitude, y = Latitude, 
                         color = Concentració), size = 3) +
          scale_color_viridis_c(name = "", direction=-1) +  # Dynamic limits
          ggtitle("5Me-BTR") 
        data31 <- d.pegat.badalona %>%
          filter(Grup %in% "6PPD-Quinona (Producte Transformació)") %>%
          droplevels()
        p31 <- M.badalona + 
          geom_point(data = data31,
                     aes(x = Longitude, y = Latitude, 
                         color = Concentració), size = 3) +
          scale_color_viridis_c(name = "", direction=-1) +  # Dynamic limits
          ggtitle("6PPD-Quinona")
        data41 <- d.pegat.badalona %>%
          filter(Grup %in% "NCBA (Benzotiazol)") %>%
          droplevels()
        p41 <- M.badalona + 
          geom_point(data = data41,
                     aes(x = Longitude, y = Latitude, 
                         color = Concentració), size = 3) +
          scale_color_viridis_c(name = "", direction=-1) +  # Dynamic limits
          ggtitle("NCBA")
        data51 <- d.pegat.badalona %>%
          filter(Grup %in% "FG (Amina cíclica)") %>%
          droplevels()
        p51 <- M.badalona + 
          geom_point(data = data51,
                     aes(x = Longitude, y = Latitude, 
                         color = Concentració), size = 3) +
          scale_color_viridis_c(name = "", direction=-1) +  # Dynamic limits
          ggtitle("FG")
          
        combined_plot <- plot_grid(
          plot_grid(p1, p2, p3, p4, p5, ncol = 5, rel_widths = c(rep(1, 5)), labels = c("Mapa de Molins de Rei", rep("", 4))),
          plot_grid(p11, p21, p31, p41, p51, ncol = 5, labels = c("\n Mapa de Badalona", rep("", 4))),
          nrow = 2,
          rel_heights = c(1, 1.2))
        combined_plot
    }
  })
}


# Run ShinyApp
shinyApp(ui, server)
