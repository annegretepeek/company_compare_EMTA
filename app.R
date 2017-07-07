library(shiny)
library(DT)
library(dplyr)
library(readxl)
library(magrittr)

dt <- read_excel("tasutud_maksud_05.07.2017.xlsx")

server <- function(input, output, session)  {
  updateSelectizeInput(session, "Firma1",
                       choices = as.vector(dt$Nimi), server = TRUE)
  updateSelectizeInput(session, "Firma2",
                       choices = as.vector(dt$Nimi), server = TRUE)
  updateSelectizeInput(session, "Firma3",
                       choices = as.vector(dt$Nimi), server = TRUE)
  
  empty_row <- data.frame(matrix(nrow = 1, ncol = ncol(dt)))
  names(empty_row) <- names(dt)
  dt <- rbind(dt, empty_row)
  names(dt)[c(4,5,9,10)] <- c("Käibemaksukohuslane", "Tegevusvaldkond", "Käive*", "Töötajate arv**")
  dt$`Riiklikud maksud käibest (%)` <- dt$`Riiklikud Maksud`/dt$`Käive*`*100
  dt$`Tööjõumaksud käibest (%)` <- dt$`Tööjõumaksud Ja Maksed`/dt$`Käive*`*100
  dt$`Käive töötaja kohta` <- dt$`Käive*`/dt$`Töötajate arv**`
  dt$`Tööjõumaksud töötaja kohta` <- dt$`Tööjõumaksud Ja Maksed`/dt$`Töötajate arv**`
  dt[,c(7:14)] <- round(dt[,c(7:14)])
  
  table_dt <- reactive({
    n1 <- dim(dt)[1]
    n2 <- n1
    n3 <- n1
    if (input$Firma1 != "") {
      n1 <- which(dt$Nimi == input$Firma1)
    }
    if (input$Firma2 != "") {
      n2 <- which(dt$Nimi == input$Firma2)
    }
    if (input$Firma3 != "") {
      n3 <- which(dt$Nimi == input$Firma3)
    }
    abi <- t(dt[c(n1,n2,n3),c(3:14)])
    abi <- data.frame(row.names(abi), abi)
    colnames(abi) <- c(" ", input$Firma1, input$Firma2, input$Firma3)
    return(abi)
  })
  
  output$compare_table <- DT::renderDataTable({
    DT::datatable(table_dt(), options = list(dom = 't',
                                             ordering = FALSE,
                                             pageLength = 12,
                                             autoWidth = FALSE,
                                             columnDefs = list(list(width = "400px", targets = "_all"))),
                  rownames = FALSE) %>% formatStyle(" ", fontWeight = "bold")
  })
}

ui <- fluidPage(
  mainPanel(width = 12,
    fluidRow(
      column(3, offset = 3, selectizeInput("Firma1", "", choices = NULL, options = list(placeholder = "Ettevõtte nimi"))),
      column(3, selectizeInput("Firma2", "", choices = NULL, options = list(placeholder = "Ettevõtte nimi"))),
      column(3, selectizeInput("Firma3", "", choices = NULL, options = list(placeholder = "Ettevõtte nimi")))
    ),
    fluidRow(column(12, DT::dataTableOutput("compare_table")
    )),
    tags$div(class = "header", checked = NA,
             tags$p("Tabel on tehtud EMTA väljastatud kvartaalsete andmete pealt. Kasutatakse märts, aprill, mai 2017. a andmeid, mis asuvad",
             tags$a(href = "http://www.emta.ee/et/kontaktid-ja-ametist/maksulaekumine-statistika/tasutud-maksud-kaive-ja-tootajate-arv", "siin.")),
             tags$p("*Deklareeritud käibena avaldatakse käibedeklaratsioonide ridade 1, 2 ja 3 summa."),
             tags$p("**Töötajate arv on möödunud kvartali viimase kuupäeva seisuga töötamise registrisse kantud kehtiva kandega tööd tegevate isikute arv, tööjõumaksud on kvartali jooksul kassapõhiselt tasutud summa. Seega ei ole töötajate arv ja tööjõumaksud kvartalis üks ühele võrreldavad.")
    ))
)

shinyApp(ui = ui, server = server)

