# if (!require("devtools"))
#     install.packages("devtools")
# devtools::install_github("shiny", "rstudio")


if (!require("tm")) install.packages("tm", quiet=TRUE) ; require("tm")
if (!require("wordcloud")) install.packages("wordcloud", quiet=TRUE) ; require("wordcloud")
if (!require("memoise")) install.packages("memoise", quiet=TRUE) ; require("memoise")
library (ggplot2)
library(shiny)


shinyApp(
    ui = fluidPage(
        selectInput("bets", "Choose:",
                    list(`On Total Bets Basis` = list("Bets by Language", "Bets by Country", "Bets by Application Description"),
                         `On Customers Basis ` = list("Customers by Language", "Customers by Country", "Customers by Product","Customers by Age"),
                         `On Gender Basis ` = list("Gender Repartition Countrywise","Gender Winnings"),
                         `On PokerChip Basis ` = list("PokerChips Bought by Country","PokerChips Sold by Country"))
        ),
        plotOutput("result")
    ),
    server = function(input, output) {
        output$result <- renderPlot({
            
            if (input$bets == "Bets by Language"){
                
                ggplot(data = read.csv('Final_Datamart.csv'), aes(Language_Description, Total_Bets)) +
                    geom_bar(stat="identity") +
                    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
                    # Use the input value as the plot's title
                    ggtitle(input$title)
            }
            
            else if (input$bets == "Bets by Country"){
                ggplot(data = read.csv('Final_Datamart.csv'), aes(CountryName, Total_Bets)) +
                    geom_bar(stat="identity") +
                    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
                    # Use the input value as the plot's title
                    ggtitle(input$title)
            }
            
            else if (input$bets == "Bets by Application Description"){
                ggplot(data = read.csv('Final_Datamart.csv'), aes(Application_Description, Total_Bets)) +
                    geom_bar(stat="identity") +
                    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
                    # Use the input value as the plot's title
                    ggtitle(input$title)
            }
            
            else if (input$bets == "Customers by Country"){
                ggplot(data = read.csv('Final_Datamart.csv'), aes(CountryName, UserID)) +
                    geom_bar(stat="identity") +
                    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
                    # Use the input value as the plot's title
                    ggtitle(input$title)
            }
            
            else if (input$bets == "Customers by Language"){
                ggplot(data = read.csv('Final_Datamart.csv'), aes(Language_Description, UserID)) +
                    geom_bar(stat="identity") +
                    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
                    # Use the input value as the plot's title
                    ggtitle(input$title)
            }
            
            else if (input$bets == "Customers by Product"){
                ggplot(data = read.csv('Daily_Aggregation.csv'), aes(ProductID, UserID)) +
                    geom_bar(stat="identity") +
                    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
                    # Use the input value as the plot's title
                    ggtitle(input$title)
            }
            
            else if (input$bets == "Customers by Age"){
                ggplot(data = read.csv('Final_Datamart.csv'), aes(AGE, UserID)) +
                    geom_bar(stat="identity") +
                    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
                    # Use the input value as the plot's title
                    ggtitle(input$title)
            }
            
            else if (input$bets == "Gender Winnings"){
                ggplot(data = read.csv('Final_Datamart.csv'), aes(Gender, Total_Winnings)) +
                    geom_bar(stat="identity") +
                    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
                    # Use the input value as the plot's title
                    ggtitle(input$title)
            }
            
            else if (input$bets == "Gender Repartition Countrywise"){
                ggplot(data = read.csv('Final_Datamart.csv')) + 
                    geom_density(aes(x=CountryName,fill=factor(Gender)),bins=10, position = "identity",alpha = 0.5, adjust=2)+
                    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
                    ggtitle(input$title)
            }
            
            else if (input$bets == "PokerChips Bought by Country"){
                ggplot(data = read.csv('Final_Datamart.csv'), aes(CountryName, Total_Buy)) +
                    geom_bar(stat="identity") +
                    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
                    # Use the input value as the plot's title
                    ggtitle(input$title)
            }
            
            else if (input$bets == "PokerChips Sold by Country"){
                ggplot(data = read.csv('Final_Datamart.csv'), aes(CountryName, Total_Sell)) +
                    geom_bar(stat="identity") +
                    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
                    # Use the input value as the plot's title
                    ggtitle(input$title)
            }
            
        })
    }
)
