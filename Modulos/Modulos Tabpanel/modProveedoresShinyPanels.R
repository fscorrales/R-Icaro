
ProveedoresSPUI <- function(id) {
  
  ns <- NS(id)
  
  tabPanel("Proveedores",
           panelsPage(
             panel(title = "Menu", color = "chardonnay", collapsed = FALSE, width =  400,
                   # body = div(
                   #   h4(fix_string_output("ProveedoresSidebar")),
                   # hr(style = "border-top: 1px solid #000000;"),
                   # FileInputUI(ns("modProveedores")),
                   # hr(style = "border-top: 1px solid #000000;")
                   # )
                   ),

             panel(title = "Listado Proveedores", color = "magenta", collapsed = FALSE
                   # body = ReacTableUI(ns("DT"))
             )
        )
      ) 
}

ProveedoresSPServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # ReacTableServer("DT", reactive(ProveedoresSGF.df() %>%
    #                                  select(CUIT, Descripcion, CondicionIVA)),
    #                 ListColDef = list(CUIT = colDef(width = 120),
    #                                   Descripcion = colDef(minWidth = 300)),
    #                 selection = NULL)
    # 
    # FileInputServer("modProveedores", "ListadoProveedores")
    
  })
}

# ui <- fluidPage(
#   ProveedoresShinyPanelsUI(id = "Prueba")
# )
# 
# server <- function(input, output) {
#   ProveedoresShinyPanelsServer(id = "Prueba")
# }
# 
# shinyApp(ui, server)