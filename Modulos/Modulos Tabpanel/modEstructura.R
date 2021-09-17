EstructuraUI <- function(id) {
  
  ns <- NS(id)
  
  tabPanel("Estructura",
           useShinyjs(),
           sidebarLayout(
             sidebarPanel(id = ns("Sidebar"),
                          h4(fix_string_output("EstructuraSidebar")),
                          
                          hr(style = "border-top: 1px solid #000000;"),
                      
                          disabled(textInput(ns("NroEstructura"), "Estructura", value = "")),
                          
                          disabled(textAreaInput(ns("DescripcionEstructura"), "Descripcion", 
                                                          value = "", resize = "vertical", rows = 4)),

                          hr(style = "border-top: 1px solid #000000;"),
                          
                          fluidRow(
                            column(4, align='center', 
                                   shinyjs::disabled(actionButton(ns("AgregarEstructura"), 
                                                                     "Agregar"))),
                            column(4, align='center', 
                                   shinyjs::disabled(actionButton(ns("EditarEstructura"), 
                                                                     "Editar"))),
                            column(4, align='center', 
                                   shinyjs::disabled(actionButton(ns("BorrarEstructura"), 
                                                    "Borrar")))
                          ),
                          
                          bsModalEstructuraUI(ns("ModalModificar")),
                          bsModalDeleteUI(ns("ModalBorrar"))
                          
             ),
             mainPanel(id = ns("Main"),
                       
                       fluidRow(
                         column(1, HideSidebarButtonUI(ns("showpanel"))),
                         column(11, tabsetPanel(id = ns("controller"),
                           tabPanel("Programas", ReacTableUI(ns("DTProgramasEstructura"))),
                           tabPanel("Subprogramas", ReacTableUI(ns("DTSubprogramasEstructura"))),
                           tabPanel("Proyectos", ReacTableUI(ns("DTProyectosEstructura"))),
                           tabPanel("Actividades", ReacTableUI(ns("DTActividadesEstructura")))
                         ))
                       )
             ) 
           )
  )
  
}

EstructuraServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    #"Bandera" para transportar datos de OBRAS AGREGAR (1), EDITAR (2) y BORRAR (3)
    EditandoEstructuraINVICO <- reactiveValues(IDModificacion = 0, Estructura = "", 
                                             Descripcion = "", NroFila = 0)
    
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
    
    Programas <- ReacTableServer("DTProgramasEstructura", ProgramasIcaro.df,
                                 ListColDef = list(Programa = colDef(width = 150)))
    
    observeEvent(Programas$NroFila(), {
      req(Programas$NroFila() > 0)
      
      EditandoEstructuraINVICO$NroFila <- Programas$NroFila()
      data <- ProgramasIcaro.df()[EditandoEstructuraINVICO$NroFila, ]
      EditandoEstructuraINVICO$Estructura <- (data[["Programa"]])
      EditandoEstructuraINVICO$Descripcion <- (data[["DescProg"]])
      HabilitarBotonesEstructura()
      UpdateDescripcionEstructura(EditandoEstructuraINVICO$Estructura,
                                  EditandoEstructuraINVICO$Descripcion,
                                  session)
      
    })
    
    Subprogramas <- ReacTableServer("DTSubprogramasEstructura", SubprogramasIcaro.df,
                                    ListColDef = list(Programa = colDef(width = 150),
                                                      Subprograma = colDef(width = 150)))
    
    observeEvent(Subprogramas$NroFila(), {
      req(Subprogramas$NroFila() > 0)
      
      EditandoEstructuraINVICO$NroFila <- Subprogramas$NroFila()
      data <- SubprogramasIcaro.df()[EditandoEstructuraINVICO$NroFila, ]
      EditandoEstructuraINVICO$Estructura <- (data[["Subprograma"]])
      EditandoEstructuraINVICO$Descripcion <- (data[["DescSubprog"]])
      HabilitarBotonesEstructura()
      UpdateDescripcionEstructura(EditandoEstructuraINVICO$Estructura,
                                  EditandoEstructuraINVICO$Descripcion,
                                  session)
      
    })
    
    Proyectos <- ReacTableServer("DTProyectosEstructura", ProyectosIcaro.df,
                                 ListColDef = list(Subprograma = colDef(width = 150),
                                                   Proyecto = colDef(width = 150)))

    observeEvent(Proyectos$NroFila(), {
      req(Proyectos$NroFila() > 0)
      
      EditandoEstructuraINVICO$NroFila <- Proyectos$NroFila()
      data <- ProyectosIcaro.df()[EditandoEstructuraINVICO$NroFila, ]
      EditandoEstructuraINVICO$Estructura <- (data[["Proyecto"]])
      EditandoEstructuraINVICO$Descripcion <- (data[["DescProy"]])
      HabilitarBotonesEstructura()
      UpdateDescripcionEstructura(EditandoEstructuraINVICO$Estructura,
                                  EditandoEstructuraINVICO$Descripcion,
                                  session)
      
    })
    
    Actividades <- ReacTableServer("DTActividadesEstructura", ActividadesIcaro.df,
                                   ListColDef = list(Proyecto = colDef(width = 150),
                                                     Actividad = colDef(width = 150)))
    
    observeEvent(Actividades$NroFila(), {
      req(Actividades$NroFila() > 0)
      
      EditandoEstructuraINVICO$NroFila <- Actividades$NroFila()
      data <- ActividadesIcaro.df()[EditandoEstructuraINVICO$NroFila, ]
      EditandoEstructuraINVICO$Estructura <- (data[["Actividad"]])
      EditandoEstructuraINVICO$Descripcion <- (data[["DescAct"]])
      HabilitarBotonesEstructura()
      UpdateDescripcionEstructura(EditandoEstructuraINVICO$Estructura,
                                  EditandoEstructuraINVICO$Descripcion,
                                  session)

    })
    
    #Botones
    
    observeEvent(input$controller, {
      HabilitarBotonesEstructura(FALSE)
    })
    
    AgregarOEditar <- reactiveVal(0)
    
    ##Agregar
    observeEvent(input$AgregarEstructura, {
      
      HabilitarBotonesEstructura(FALSE)
      
      EditandoEstructuraINVICO$IDModificacion <- 1
      
      n <- AgregarOEditar() + 1
      
      AgregarOEditar(n)
      
      UpdateDescripcionEstructura(sessionID = session)
      
    })

    ##Editar
    observeEvent(input$EditarEstructura, {
      
      HabilitarBotonesEstructura(FALSE)
      
      EditandoEstructuraINVICO$IDModificacion <- 2
      
      n <- AgregarOEditar() + 1
      
      AgregarOEditar(n)
      
      UpdateDescripcionEstructura(sessionID = session)

    })
    
    bsModalEstructuraServer("ModalModificar", 
                            AgregarOEditar,
                            reactive(EditandoEstructuraINVICO$IDModificacion),
                            reactive(EditandoEstructuraINVICO$Estructura),
                            reactive(EditandoEstructuraINVICO$NroFila))
        
    
    ##Borrar
    observeEvent(input$BorrarEstructura, {
      
      HabilitarBotonesEstructura(FALSE)
      
      EditandoEstructuraINVICO$IDModificacion <- 3
      
    })
    
    PermitirBorrar <- reactiveVal(FALSE)
    
    GlosaBorrar <-  reactive({
      req(EditandoEstructuraINVICO$Estructura)
      
      ValidarBorrar <- ValidarBorrarEstructura(EditandoEstructuraINVICO$Estructura)
      
      if (ValidarBorrar$EsValido) {
        PermitirBorrar(TRUE)
      } else {
        PermitirBorrar(FALSE)
      }
  
      Glosa <- ValidarBorrar$Descripcion
      
    }) 
    
    BorrarEstructura <- bsModalDeleteServer("ModalBorrar", 
                                            reactive(input$BorrarEstructura),
                                            (GlosaBorrar),
                                            (PermitirBorrar))
    
    observeEvent(BorrarEstructura$Borrar(), {
      
      req(BorrarEstructura$Borrar() > 0)
      
      EliminarEstructuraICARO(EditandoEstructuraINVICO$Estructura,
                              EditandoEstructuraINVICO$NroFila)
      
      UpdateDescripcionEstructura(sessionID = session)
      
    })
    
  })
}

# ui <- fluidPage(
#   EstructuraUI(id = "Prueba")
# )
# 
# server <- function(input, output) {
#   EstructuraServer(id = "Prueba")
# }
# 
# shinyApp(ui, server)