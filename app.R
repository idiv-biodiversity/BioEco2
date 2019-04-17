################################################################
### DESIRE - Nature Ecology and Evolution 
### February 2019
### 
### Figure 2
### 
### Ines S. Martins (ines.martins@idiv.de) 
#####################################################################################


library(ggplot2)
library(reshape2)
library(network)
library(wordcloud)
library(plotrix)
library(plyr)
library(gridExtra)
library(grid)
library(stringr)
library(graphics)
library(shiny)
library(shinydashboard)


df2<-read.csv("Figure_2_line.csv")
df43<-read.csv("Figure_2.csv")


df43<-df43[,2:7]
df2<-df2[,2:7]

df43<-droplevels(df43)
df2<-droplevels(df2)

df43$type<-factor(df43$type,levels=c("pop","gdp_cap","bio_pro_gdp","bio_con_gdp","cseq_pro_gdp","cseq_con_gdp"))
df2$type <-factor(df2$type,levels=c("bio_pro","bio_con","cseq_con","cseq_pro"))


######  RUN SHINY ######


ui <- fluidPage(

   titlePanel(h3("Trends in production and consumption impacts on bidiversity and carbon sequestration")
   ),

   sidebarLayout(

      sidebarPanel(position = "right",
                   fluidRow(selectInput("bins2", h5("Total impacts and their decomposition into the contribution of population, GDP per capita and impact per GDP."),
                                        choices = list("Impacts" = unique(df43$impacts)), selected = 1)),
                   fluidRow(selectInput("bins", h5(""),
                                        choices = list("Region" = unique(df43$WR)), selected = 1)),
                   # fluidRow(helpText("Annual changes in production impacts relative to year 2000")),
                   fluidRow(helpText("Production and consumption impacts from agriculture and forestry activities in selected country or region. Annual cummulative changes are given relative to 2000. Biodiversity impacts are measured in terms of impending global bird extinctions, and ecosystem
                                     services impacts in terms of carbon sequestration lost.")),
                   fluidRow(helpText( em("Based on Figure 2 in Marques et al. (2019) Trends on higher land-use efficiency insufficient to mitigate impacts on nature from population and consumption growth. Nat.Ecol.Evo."))),
                   fluidRow(helpText("Graph by I.S. Martins."))

                   ),

      mainPanel(

         plotOutput(outputId = "distPlot")

      )
   )
)


#Run and store the plot
server <- function(input, output) {

   dataInput <- reactive({
      df43[df43$WR==input$bins & df43$impacts==input$bins2,]

   })

   dataInput2 <- reactive({

      df2[df2$WR==input$bins & df2$impacts==input$bins2,]
   })

   output$distPlot <- renderPlot({

      b<-c('#E69F00','#009E73','#56B4E9','#0072B2')
      l<-c('black','black')

      #df43[df43$WR==input$bins & df43$impacts==input$impacts,]

      ggplot() +
         geom_bar(data=dataInput(), aes(x = year, y = value, fill = type), stat="identity", alpha=.5)+
         #geom_bar(data=df43, aes(x = year, y = value, fill = type), stat="identity", alpha=.5)+
         scale_fill_manual(labels = c("Population","GDP per capita","Total biodiversity impacts per GDP","Total ecosystem services impacts per GDP"), values = b)+
         geom_line(data=dataInput2(), aes(x= year,y=value,group = type,color=type,linetype=type),size=1)+
         #geom_line(data=df2, aes(x= year,y=value,group = type,color=type,linetype=type))+
         guides(colour = guide_legend(override.aes = list(size=0.9)),
                fill=guide_legend(ncol=2,byrow=TRUE))+
         scale_linetype_manual(values = c('solid', 'dotted'),
                               labels = c("Total biodiversity impacts","Total ecosystem services impacts"))+
         scale_color_manual(values = l,
                            labels = c("Total biodiversity impacts","Total ecosystem services impacts"))+
         labs(x="", y=expression(paste("Relative change (yr"["x"]*"-yr"["1"]*" / yr"["1"]*",%)")), size=10)+
         facet_grid(input$bins ~ base,scales = "free")+
         #facet_grid(WR ~ base,scales = "free")+
         scale_x_discrete(limits = seq(2001, 2011, 1))+
         geom_hline(yintercept=0, color = "black",size=0.2)+
         theme(#strip.text.y = element_text(size = 7, colour = "black", angle = -90),
            strip.text.x = element_blank(),
            axis.text.x=element_text(size=8),
            axis.text.y=element_text(size=8),
            legend.text=element_text(size=10),
            axis.title=element_text(size=15),
            legend.spacing.x = unit(0.3, 'cm'),
            #legend.spacing.y = unit(0.5, 'cm'),
            legend.title=element_blank(),
            legend.key=element_blank(),
            legend.direction="vertical",
            legend.position="bottom",
            panel.spacing = unit(1, "lines"),
            strip.text.y = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black",size = 0.2),
            panel.border = element_rect(colour = "black", fill=NA, size=1)
         )

   })

}

shinyApp(ui = ui, server = server)
