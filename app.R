# Librerias necesarias 
library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)


sessionInfo()

# Lectura de datos
data=read.csv("walmart-sales/Train.csv")

# Estructura del cuerpo del panel 
body = dashboardBody(
    
    tabItems(
        tabItem(tabName = 'tab1',
                fluidPage(
                    title = "Market Explorer",
                    column(width = 3,
                           span(tags$img(src="logo_central-mayorista.png", width = '190')),
                           box(
                               title = "Filtros Grafica 1",
                               status = "primary",
                               width = 12,
                               solidHeader = TRUE,
                               background = "navy",
                               box(
                                   width = 12,
                                   status = "primary",
                                   solidHeader = FALSE,
                                   background = "navy",
                                   selectInput("filtro1","Filtro 1:", choices = unique(data$Outlet_Type),
                                               selected = 'Supermarket Type1', multiple=TRUE, selectize=TRUE),
                                   selectInput("filtro2","Filtro2:", choices = unique(data$Outlet_Location_Type),
                                               selected = 'Tier 1', multiple=TRUE, selectize=TRUE),
                                   selectInput("filtro3","Filtro3:", choices = unique(data$Item_Fat_Content),
                                               selected = 'Low Fat', multiple=TRUE, selectize=TRUE)
                               )),
                           box(
                               title = "Filtros Grafica 2",
                               status = "primary",
                               width = 12,
                               solidHeader = TRUE,
                               background = "navy",
                               box(
                                   width = 12,
                                   status = "primary",
                                   solidHeader = FALSE,
                                   background = "navy",
                                   selectInput("filtro1b","Filtro 1:", choices = unique(data$Outlet_Type),
                                               selected = 'Supermarket Type1', multiple=TRUE, selectize=TRUE),
                                   selectInput("filtro2b","Filtro2:", choices = unique(data$Outlet_Location_Type),
                                               selected = 'Tier 1', multiple=TRUE, selectize=TRUE),
                                   selectInput("filtro3b","Filtro3:", choices = unique(data$Item_Fat_Content),
                                               selected = 'Low Fat', multiple=TRUE, selectize=TRUE)
                               ))),
                    column(width = 9,
                           box(
                               title = "Grafico de dispersion 1",
                               status = "primary",
                               width = 12,
                               solidHeader = TRUE,
                               collapsible = TRUE,
                               fluidRow(
                                   box(
                                       title = "Valor var1 vs valor var2",
                                       status = "primary",
                                       width = 12,
                                       solidHeader = FALSE,
                                       collapsible = TRUE,
                                       plotlyOutput("plot",height = 250)
                                   )# end of box
                               )),
                           box(
                               title = "Grafico de dispersion 2",
                               status = "primary",
                               width = 12,
                               solidHeader = TRUE,
                               collapsible = TRUE,
                               fluidRow(
                                   box(
                                       title = "Valor var1 vs valor var2",
                                       status = "primary",
                                       width = 12,
                                       solidHeader = FALSE,
                                       collapsible = TRUE,
                                       plotlyOutput("plot2",height = 250)
                                   )# end of box
                               ))))),
        
        tabItem(tabName = 'tab2',
                tabBox(title = 'Titulo del tab en caja',
                       tabPanel('tabName1', 'Primer tab en caja'),
                       tabPanel('tabName2', 'Segundo tab en caja'))))
    )

sidebar = dashboardSidebar(
    
    sidebarMenu(menuItem(text = 'Mapa de categorias', tabName = 'tab1'),
                                       menuItem(text = 'Cluster', tabName = 'tab2')), 
                           collapsed = TRUE)


# Maqueta del panel 
ui = dashboardPage(header = dashboardHeader(),
                   sidebar = sidebar,
                   body = body)




# Logica del panel 
server = function(input, output) {
   
    # Data reactiva 1     
    data_total_re = reactive({
        data %>% 
            filter(Outlet_Type %in% input$filtro1 ,
                   Outlet_Location_Type %in% input$filtro2,
                   Item_Fat_Content %in% input$filtro3)
            #%>%  group_by(NOM_COMUNA) %>% summarise(tasa=sum(tasa))
    }) 
    
    # Data reactiva 2     
    data_total_re2 = reactive({
        data %>% 
            filter(Outlet_Type %in% input$filtro1b ,
                   Outlet_Location_Type %in% input$filtro2b,
                   Item_Fat_Content %in% input$filtro3b)
        #%>%  group_by(NOM_COMUNA) %>% summarise(tasa=sum(tasa))
    })
    
    
    # Grafico de dispersion reactivo   
    output$plot <- renderPlotly({
        
        # Nombres de ejes
        x <- list(title = "Nombre eje X")
        y <- list(title = "Nombre eje Y")

    plot_ly(data_total_re(),x = data_total_re()$Item_Weight,
            y = data_total_re()$Item_Visibility, text = data_total_re()$Item_Type,
            color = data_total_re()$Item_Outlet_Sales, size = data_total_re()$Item_Weight, 
            mode = "markers") %>% layout(xaxis = x, yaxis = y)
    
    })
    
    output$plot2 <- renderPlotly({
        
        # Nombres de ejes
        x <- list(title = "Nombre eje X")
        y <- list(title = "Nombre eje Y")
        
        plot_ly(data_total_re2(),x = data_total_re2()$Item_Weight,
                y = data_total_re2()$Item_Visibility, text = data_total_re2()$Item_Type,
                color = data_total_re2()$Item_Outlet_Sales, size = data_total_re2()$Item_Weight, 
                mode = "markers") %>% layout(xaxis = x, yaxis = y)
        
    })
    
}

# Run APP
shinyApp(ui = ui, server = server)
