
library(shiny)
library(shinythemes)
library(shinydashboard)
library(tidyverse)
#library(viridis)
library(colourpicker)

ui <- fluidPage(
    theme = shinytheme("spacelab"),

    titlePanel("Maybe It's Generative Art?"),

    mainPanel(
       fluidRow(
       box(
           h4("Pick 3 colors then hit \"Run\""),
           colourpicker::colourInput("colorPicker1", "", "#EE3524"),
           colourpicker::colourInput("colorPicker2", "", "#000"),
           colourpicker::colourInput("colorPicker3", "", "#FFF"),
           actionButton("runButton","Run")
       ),
       box(plotOutput("maybe_art",width = "100%",height = "600px"))
    )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {

    
    
    observeEvent(input$runButton,{
    

    
    output$maybe_art <- renderPlot({
        #data
        end <- sample(35:45,1)
        bin_num <- sample(10:end,1)
        ratio <- 1:end
        
        #data frame
        df1 <- data.frame(v1 = sample(ratio,size = bin_num, replace = T))
        df1 <- do.call("rbind", replicate(11, data.frame(v1 = sample(ratio,size = bin_num, replace = T)), simplify = FALSE))
        
        df2 <- data.frame(v2 = sample(ratio,size = bin_num, replace = T)) %>% 
            bind_rows(list(v2 = sample(ratio,size = bin_num*10, replace = T)))
        
        data <- df1 %>% 
            add_column(v2 = unlist(df2))
        
        #define points
        min_y <- min(data$v2)
        max_y <- max(data$v2)
        min_x <- min(data$v1)
        max_x <- max(data$v1)
        
        #colors
        color1 <- isolate({input$colorPicker1})
        color2 <- isolate({input$colorPicker2})
        color3 <- isolate({input$colorPicker3})
        
        #randomness
        coord_flip_value <- sample(0:1,1)
        coord_flip <- ifelse(coord_flip_value == 0,"coord_flip()","")
        
        if(coord_flip_value == 0){
            grid_x <- 0
            grid_y <- .5
            
        } else {
            grid_x <- .5
            grid_y <- 0
        }
        
        #more randomness
        # scale_fill <- ifelse(sample(0:1,1) == 0,"scale_fill_viridis_c()"," scale_fill_viridis(option='magma')")
        
        
        # scale_fill <- reactive({
        #     if(!is.null(input$colorInput1)){
        #         "scale_fill_viridis_c()"
        #     }
        #     if(!is.null(input$colorInput2)){
        #         scale_fill_viridis(option='magma')
        #     }else{
        #         paste0("scale_fill_gradientn(colors = c(\"",color1,"\",\"",color2,"\",\"",color3,"\"))")
        #         
        #     }
        # })
        # 
        # scale_fill <- scale_fill()
        scale_fill <- paste0("scale_fill_gradientn(colors = c(\"",color1,"\",\"",color2,"\",\"",color3,"\"))")
        
        x_reverse_value <- sample(0:1,1)
        x_reverse <- ifelse(x_reverse_value == 0,"scale_x_reverse()","")
        
        y_reverse_value <- sample(0:1,1)
        y_reverse <- ifelse(y_reverse_value == 0,"scale_y_reverse()","")
        
        
        #plot
        plot <- data %>% 
            ungroup() %>% 
            ggplot() +
            geom_bar(aes(x = v1, y = v2,fill = v2),
                     stat = "identity",
                     # position = "dodge",
                     #color = "black",
                     #width = 1,
                     position = position_nudge(x = 0.5)
            ) +
            geom_errorbar(aes(x=v1,ymin=v2,ymax=v2),size=0.5,width=1,color = "black",position=position_nudge(x = .5),stat = "identity") +
            
            geom_segment(aes(x=1,xend=end+1,y=0,yend=0)) + #x axis bottom
            geom_segment(aes(x=1,xend= end+1,y=max_y,yend=max_y)) + #x axis top
            # geom_segment(aes(x=.5,xend=.5,y=0,yend=max_y)) + #y axis right
            # geom_segment(aes(x=end + .5,xend=end + .5,y=0,yend=max_y)) + #y axis left
            
            eval(parse(text = scale_fill)) +
            eval(parse(text = coord_flip)) +
            eval(parse(text = y_reverse)) +
            eval(parse(text = x_reverse)) +
            
            theme_void() +
            theme(legend.position = "none",
                  panel.border=element_rect(fill=NA)
            ) +
            scale_x_continuous(breaks = seq(1, end, 1))
        
        #add in separating lines
        for(i in c(seq(min_x,max_x),max_x+1)){
            if(y_reverse_value == 0){max_y_loop <- max_y * -1}else{max_y_loop <- max_y}
            plot <- plot + geom_linerange(x = i,ymin = 0, ymax = max_y_loop, color = 'black')
        }
        
        plot
    },width = sample(4:6,1)*100,height = sample(5:6.5,1)*100)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
