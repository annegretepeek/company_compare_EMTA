library(shiny)
library(DT)
library(dplyr)
library(readxl)
library(magrittr)

dt <- read_excel("tasutud_maksud_10.01.2018.xlsx")

parandus <- function(string){
  string <- gsub("<ff>FFFFC3<ff>FFFF9C", "Ü", string)
  string <- gsub("<ff>FFFFC3<ff>FFFF95", "Õ", string)
  string <- gsub("<ff>FFFFC3<ff>FFFF84", "Ä", string)
  string <- gsub("<ff>FFFFC3<ff>FFFF96", "Ö", string)
  string <- gsub("<ff>FFFFC5<ff>FFFFA0", "Š", string)
  string <- gsub("<ff>FFFFC5<ff>FFFFBD", "Ž", string)
  return(string)
}

server <- function(input, output, session)  {
  firma1 <- NULL
  firma2 <- NULL
  firma3 <- NULL
  
  onRestore(function(url) {
    firma1 <<- parandus(url$input$Firma1)
    firma2 <<- parandus(url$input$Firma2)
    firma3 <<- parandus(url$input$Firma3)
  })
  
  observe({
    updateSelectizeInput(session, "Firma1", choices = dt$Nimi, selected = firma1, server = TRUE)
    updateSelectizeInput(session, "Firma2", choices = dt$Nimi, selected = firma2, server = TRUE)
    updateSelectizeInput(session, "Firma3", choices = dt$Nimi, selected = firma3, server = TRUE)
  })

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
    abi <- dt[c(n1,n2,n3),c(3:14)] 
    abi[,c(5:12)] <- lapply(abi[,c(5:12)],FUN = function(x){c(prettyNum(x, format = "d", big.mark = " "))})
    abi[] <- lapply(abi, as.character)
    abi[abi == "NA"] <- ""
    abi <- t(abi)
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
  
  setBookmarkExclude(c("compare_table_cell_clicked", "compare_table_rows_all", "compare_table_rows_current", "compare_table_rows_selected", "compare_table_search", "compare_table_state"))
}

ui <- function(request) {
  fluidPage(
    mainPanel(width = 12,
              fluidRow(
                column(3, offset = 3, selectizeInput("Firma1", "", choices = NULL, options = list(placeholder = "Trüki või vali ettevõtte nimi"))),
                column(3, selectizeInput("Firma2", "", choices = NULL, options = list(placeholder = "Trüki või vali ettevõtte nimi"))),
                column(3, selectizeInput("Firma3", "", choices = NULL, options = list(placeholder = "Trüki või vali ettevõtte nimi")))
              ),
              fluidRow(column(12, DT::dataTableOutput("compare_table")
              )),
              br(),
              bookmarkButton(label = "Link jagamiseks", title = "Salvesta see vaade ja saa jagamiseks vaate URL."),
              br(),
              br(),
              tags$div(class = "header", checked = NA,
                       tags$p("Tabel on tehtud EMTA väljastatud kvartaalsete andmete pealt. Kasutatakse septembri, oktoobri, novembri 2017. a andmeid, mis asuvad",
                              tags$a(href = "http://www.emta.ee/et/kontaktid-ja-ametist/maksulaekumine-statistika/tasutud-maksud-kaive-ja-tootajate-arv", "siin."), "Ettevõtted on sorteeritud tunnuse \"Riiklikud  maksud\" järgi."),
                       tags$p("*Deklareeritud käibena avaldatakse käibedeklaratsioonide ridade 1, 2 ja 3 summa."),
                       tags$p("**Töötajate arv on möödunud kvartali viimase kuupäeva seisuga töötamise registrisse kantud kehtiva kandega tööd tegevate isikute arv, tööjõumaksud on kvartali jooksul kassapõhiselt tasutud summa. Seega ei ole töötajate arv ja tööjõumaksud kvartalis üks ühele võrreldavad.")
              ))
  )
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")

