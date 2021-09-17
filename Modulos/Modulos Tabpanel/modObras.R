
ObrasUI <- function(id) {
  
  ns <- NS(id)
  
  tabPanel("Obras",
           useShinyjs(),
           sidebarLayout(
             sidebarPanel(id = ns("Sidebar"),
                          h4(fix_string_output("ObrasSidebar")),
                          
                          hr(style = "border-top: 1px solid #000000;"),
                      
                          disabled(textInput(ns("EstructuraObra"), "Estructura", value = "")),
                          
                          disabled(textAreaInput(ns("DescripcionObra"), "Descripcion", 
                                                          value = "", resize = "vertical", rows = 4)),

                          hr(style = "border-top: 1px solid #000000;"),
                          
                          fluidRow(
                            column(4, align='center', 
                                   shinyjs::disabled(actionButton(ns("AgregarObra"), 
                                                                  "Agregar"))),
                            column(4, align='center', 
                                   shinyjs::disabled(actionButton(ns("EditarObra"), 
                                                                  "Editar"))),
                            column(4, align='center', 
                                   shinyjs::disabled(actionButton(ns("BorrarObra"), 
                                                                  "Borrar")))
                          ),
                          
                          bsModalObraUI(ns("ModalModificar")),
                          bsModalDeleteUI(ns("ModalBorrar"))

             ),
             mainPanel(id = ns("Main"),
                       
                       fluidRow(
                         column(1, HideSidebarButtonUI(ns("showpanel"))),
                         column(11, ReacTableUI(ns("DTObras")))
                       )
             ) 
           )
  )
  
}

ObrasServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    #Name Space for input model dialog
    ns <- session$ns

    #"Bandera" para transportar datos de OBRAS AGREGAR (1), EDITAR (2) y BORRAR (3)
    EditandoObraINVICO <- reactiveValues(IDModificacion = 0, Imputacion = "", 
                                         Partida = "", Fuente = "", 
                                         DescObra = "", Localidad = "", 
                                         CUIT = "", NormaLegal = "", 
                                         Cuenta = "", NroFila = 0,
                                         InformacionAdicional = "")
    
    # #"Bandera" para transportar datos del Formulario al validador
    # FormularioCargaObraINVICO <- reactiveValues(IDModificacion = 0, Imputacion = "", 
    #                                                 Partida = "", Fuente = "", 
    #                                                 Descripcion = "", Localidad = "",
    #                                                 CUIT = "", NormaLegal = "", 
    #                                                 Cuenta = "")
    
    #Expandable Sidebar
    toggleSidebar <- reactive(HideSidebarButtonServer("showpanel"))
    
    observeEvent(toggleSidebar(), {
      
      if(toggleSidebar() == TRUE) {
        removeCssClass("Main", "col-sm-12")
        addCssClass("Main", "col-sm-8")
        shinyjs::show(id = session$ns("Sidebar"), asis = TRUE)
        # shinyjs::enable(id = session$ns("Sidebar"), asis = TRUE)
        # shinyjs::disable(id = session$ns("AgregarObra"), asis = TRUE)
        # shinyjs::enable("EditarObra")
        # shinyjs::enable("EstructuraObra")
      } else {
        removeCssClass("Main", "col-sm-8")
        addCssClass("Main", "col-sm-12")
        shinyjs::hide(id = session$ns("Sidebar"), asis = TRUE)
      }
      
    })
    
    
    #DataTable
    Obras <- ReacTableServer("DTObras", reactive(ObrasIcaro.df() %>% 
                               select(Imputacion, Partida, Fuente,
                                      Descripcion, Localidad, CUIT,
                                      NormaLegal, Cuenta, InformacionAdicional)),
                               ListColDef = list(Imputacion = colDef(width = 100),
                                                 Partida = colDef(width = 75),
                                                 Fuente = colDef(width = 75),
                                                 InformacionAdicional = colDef(width = 75),
                                                 Descripcion = colDef(minWidth = 400))
                             ) 
                             
    
    observeEvent(Obras$NroFila(), {
      req(Obras$NroFila() > 0)
      
      EditandoObraINVICO$NroFila <- Obras$NroFila()
      data <- ObrasIcaro.df()[EditandoObraINVICO$NroFila, ]
      EditandoObraINVICO$Imputacion <- unname(data["Imputacion"])
      EditandoObraINVICO$Partida <- unname(data["Partida"])
      EditandoObraINVICO$Fuente <- unname(data["Fuente"])
      EditandoObraINVICO$DescObra <- unname(data["Descripcion"])
      EditandoObraINVICO$Localidad <- unname(data["Localidad"])
      EditandoObraINVICO$CUIT <- unname(data["CUIT"])
      EditandoObraINVICO$NormaLegal <- unname(data["NormaLegal"])
      EditandoObraINVICO$Cuenta <- unname(data["Cuenta"])
      EditandoObraINVICO$InformacionAdicional <- unname(data["InformacionAdicional"])
      HabilitarBotonesObra()
      UpdateDescripcionObra(paste(EditandoObraINVICO$Imputacion,
                                  EditandoObraINVICO$Partida, sep = "-"),
                            EditandoObraINVICO$DescObra,
                            session)
      
    })
    
    #Botones
    
    AgregarOEditar <- reactiveVal(0)
    
    ##Agregar
    
    observeEvent(input$AgregarObra, {
      
      HabilitarBotonesObra(FALSE)
      
      EditandoObraINVICO$IDModificacion <- 1
      
      n <- AgregarOEditar() + 1
      
      AgregarOEditar(n)
      
    })
    
    ##Editar
    observeEvent(input$EditarObra, {
      
      HabilitarBotonesObra(FALSE)
      
      EditandoObraINVICO$IDModificacion <- 2
      
      n <- AgregarOEditar() + 1
      
      AgregarOEditar(n)
      
    })
    
    bsModalObraServer("ModalModificar",
                      AgregarOEditar,
                      reactive(EditandoObraINVICO))
    
    ##Borrar
    observeEvent(input$BorrarObra, {

      HabilitarBotonesObra(FALSE)

      EditandoObraINVICO$IDModificacion <- 3

    })

    PermitirBorrar <- reactiveVal(FALSE)

    GlosaBorrar <-  reactive({
      req(EditandoObraINVICO$DescObra)

      ValidarBorrar <- ValidarBorrarObra(EditandoObraINVICO$DescObra)
      
      if (ValidarBorrar$EsValido) {
        PermitirBorrar(TRUE)
      } else {
        PermitirBorrar(FALSE)
      }
  
      Glosa <- ValidarBorrar$Descripcion

    })

    BorrarObra <- bsModalDeleteServer("ModalBorrar",
                                      reactive(input$BorrarObra),
                                      (GlosaBorrar),
                                      (PermitirBorrar))

    observeEvent(BorrarObra$Borrar(), {
      
      req(BorrarObra$Borrar() > 0)

      EliminarObraICARO(EditandoObraINVICO$NroFila)

    })
    
  })
}

# ui <- fluidPage(
#   ObrasUI(id = "Prueba")
# )
# 
# server <- function(input, output) {
#   ObrasServer(id = "Prueba")
# }
# 
# shinyApp(ui, server)