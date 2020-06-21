library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(geosphere)
library(readr)
library(dplyr)

options(warn=-1)


listings <- read.csv('mod_listings.csv')
listing<-read.csv('listings.csv')
listing1<-read.csv('listings_1.csv')
temp<-cbind(listing,listing1)
listing <- temp[, !duplicated(colnames(temp))]

condenseMe <- function(vector, threshold = 0.02, newName = "Other") {
  toCondense <- names(which(prop.table(table(vector)) < 0.02))
  vector[vector %in% toCondense] <- newName
  vector
}
##########################
parsed_amenities <-
  listing %>% 
  .$amenities %>% 
  sub("^\\{(.*)\\}$", "\\1\n", x = .) %>% 
  lapply(function(x) names(read_csv(x)))

df <-
  unique(unlist(parsed_amenities)) %>% 
  .[!grepl("translation missing", .)] %>% 
  setNames(., .) %>% 
  lapply(function(x) vapply(parsed_amenities, "%in%", logical(1), x = x)) %>% 
  as_data_frame()

r<-data.frame(df)

w<-apply(r, 1, function(x) sum(x))

listing$NumberofAmenities<-w
##########################


ui <- dashboardPage(
  dashboardHeader(title = "Airbnb Chicago !"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Price & Size", tabName = "tab_1", icon = icon("bar-chart")),
      menuItem("Price & Location", tabName = "tab_2", icon = icon("bar-chart")),
      menuItem("Price, Size & Location", tabName = "tab_3", icon = icon("bar-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "tab_1",
              h2("Exploring Price with respect to size"),
              fluidRow(
                box("The following graphs show the variation of price with respect to the size of the house (measured in terms of number of tenants, 
                    guests, number of bedrooms and bathrooms)",width = 12)
              ),
              fluidRow(
                  box(selectInput(inputId = "dataset",
                              label = "Choose a parameter",
                              choices = c("Number of people accommodated","Number of bedrooms","Number of Guests","Number of Bathrooms")
    
                  ))
              ),
              fluidRow( plotOutput("distPlot",width = "90%", height = "500px")),
              fluidRow(box(textOutput("Tab1"),width = 12))
              
              
      ),
      
      # Second tab content
      tabItem(tabName = "tab_2",
              h2("Exploring Price with repect to location of the house"),
              fluidRow(
                box("In this section, we try to understand the average price of the house with respect to the factors that are influenced by location. We also try
                    to understand the influence of the proximity to downtown on the price tag. Also considered are the review scores with respect to location and the average price 
                    variation with respect to the area (a collection of neighbourhoods is an area)",width = 12)
              ),
              fluidRow(
                box(selectInput(inputId = "var_1",
                                label = "Choose Variable",
                                choices = c("Average Price by Neighbourhood", "Average Distance From Downtown by Neighbourhood", "Average Number of Amenities by Neighbourhood"
                                            ,"Average Review Score by Neighbourhood","Average Price by Area")
                                
                ))
              ),
              fluidRow(box(plotOutput("Categ"),width = 12)),
              fluidRow(box("From the graphs, it can be seen that River North area is one of the most expensive places to rent a house or roo. This is mostly because of its proximity to 
                       Chicago downtown.The only other area closer to the downtown is The Loop. Prices here are however reasonable, as the majority of listings are private or shared rooms
                       and not entire houses (which are typically more expensive)",width=12))
              
      ),
      tabItem(tabName = "tab_3",
              h2("Price - Location - Size: South Shore and South Chicago"),
              fluidRow(box("In this section we choose two neighbourhoods - South Shore and South Chicago and examine the housing options with repect to room type and price. Click
                           on the marker to learn more about the house",width = 12)),
              fluidRow(tabBox(
                title = "Neighbourhood",
                id = "tabset1", height = "200px",
                tabPanel("South Shore", selectInput(inputId = "loc2",
                                                               label = "Choose Room Type",
                                                               choices = c("Entire","Private","Shared")
                                                               
                )),
                tabPanel("South Chicago",selectInput(inputId = "loc1",
                                                         label = "Choose Room Type",
                                                         choices = c("Entire","Private","Shared")
                                                         
                )),width = 8
              ),
              box(sliderInput("range", "Price Range:",
                              min = 14, max = 150,
                              value = c(14,80)),width = 4)),
              fluidRow(leafletOutput("mymap"))
              )
    )
  )
 

)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    key_1 <- c("Number of people accommodated","Number of bedrooms","Number of Guests","Number of Bathrooms")
    if(input$dataset==key_1[1])
    {
      ggplot(listing)+geom_point(aes(x=accommodates,y=log(price),colour=room_type))+theme_bw()+labs(x=input$dataset,y="log(price)")+theme_set(theme_bw(base_size = 20))
    }
    else if (input$dataset==key_1[2])
    {
      ggplot(listing)+geom_point(aes(x=bedrooms,y=log(price),colour=room_type))+theme_bw()+labs(x=input$dataset,y="log(price)")+theme_set(theme_bw(base_size = 20))
    }
    else if (input$dataset==key_1[3])
    {
      ggplot(listing)+geom_point(aes(x=guests_included,y=log(price),colour=room_type))+theme_bw()+labs(x=input$dataset,y="log(price)")+theme_set(theme_bw(base_size = 20))
    }
    else
    {
      ggplot(listing)+geom_point(aes(x=bathrooms,y=log(price),colour=room_type))+theme_bw()+labs(x=input$dataset,y="log(price)")+theme_set(theme_bw(base_size = 20))
    }
    
  })
  output$Tab1 <- renderText({
    key_1 <- c("Number of people accommodated","Number of bedrooms","Number of Guests","Number of Bathrooms")
    if(input$dataset==key_1[1])
    {
      "The following plot shows the variation of price (on a logarithmic scale) of the room as a function of the number of people it can accommodate. In general, it can
      be seen that there is a trend of price increase with repect to the room size, which is expected. Also, it can be seen that a lot more options are observed for smaller
      room sizes (in terms of the number of people it can accommodate."
    }
    else if (input$dataset==key_1[2])
    {
      "The following plot shows the variation of price (on a logarithmic scale) of the room as a function of the number of bedrooms in the house. Again, as the
      number of bedrooms increase, there is an upward trend in the price paid for it, which is intuitive. "
    }
    else if (input$dataset==key_1[3])
    {
      "The following plot shows the variation of price (on a logarithmic scale) of the room as a function of the number of guests allowed. It can be seen that almost only
      entire houses allow more than 4 guests and the number of houses that accepts guests, falls as the number of guests increase (i.e.) there are very few houses that allow
      more than 6 guests."
    }
    else
    {
      "The following plot shows the variation of price (on a logarithmic scale) of the room as a function of the number of bathrooms in the house. The price does not 
      show significant variation with the number of bathrooms for houses that have 3 or lesser bathrooms."
    }
    
  })
  output$Categ <- renderPlot({
    key_2 <- c("Average Price by Neighbourhood", "Average Distance From Downtown by Neighbourhood", "Average Number of Amenities by Neighbourhood"
               ,"Average Review Score by Neighbourhood","Average Price by Area")
    listing$NeighbourWithOthers<-condenseMe(as.character(listing$neighbourhood))
    listing$property_type<-condenseMe(as.character(listing$property_type))
    
    if (input$var_1 == key_2[1])
    {
        AveragePriceByNeighb<-aggregate(listing$price,FUN="mean",by=list(listing$NeighbourWithOthers)) 
      colnames(AveragePriceByNeighb)[1:2]<-c("Neighbourhood","Average_Price")
      
      #INCLUDE
      #AveragePriceByNeighb[with(AveragePriceByNeighb, order(-Average_Price)), ]
      a<-AveragePriceByNeighb[with(AveragePriceByNeighb, order(-Average_Price)), ]
      
      
      
      ggplot(a,aes(x=reorder(Neighbourhood, -Average_Price),y=Average_Price,fill=Neighbourhood))+
        geom_bar(stat = "identity")+
        geom_text(label = paste0("$", round(a$Average_Price))) + 
        # scale_x_discrete(label = abbreviate) + 
        labs(x = "Neighborhood", y = "Average Price", title = "Average Price By Neighborhood") + 
        theme_minimal() + theme_set(theme_bw(base_size = 20)) +
        theme(legend.position = "none") + theme(axis.text.x=element_text(angle=90, hjust=1))
    }
    else if (input$var_1 == key_2[2])
    {
      dist<-c()
      for(i in 1:nrow(listing)){
        dist[i]<-distCosine(c(41.881832,-87.623177),c(listing$latitude[i],listing$longitude[i]))
      }
      listing$dist<-dist
      
      #convert meter to mile
      listing$dist<-0.000621371*listing$dist
      
      
      DistanceFromDowntownByNeighb<-aggregate(listing$dist,FUN="mean",by=list(listing$NeighbourWithOthers)) 
      colnames(DistanceFromDowntownByNeighb)[1:2]<-c("Neighbourhood","Average_dist_From_Dowtown")
      
      #INCLUDE
      a1<-DistanceFromDowntownByNeighb[with(DistanceFromDowntownByNeighb, order(Average_dist_From_Dowtown)), ]
      
      
      ggplot(a1,aes(x=reorder(Neighbourhood,Average_dist_From_Dowtown),y=Average_dist_From_Dowtown,fill=Neighbourhood))+
        geom_bar(stat = "identity")+
        geom_text(label = paste0(round(a1$Average_dist_From_Dowtown,2)," miles")) + 
        # scale_x_discrete(label = abbreviate) + 
        labs(x = "Neighbourhood", y = "Average Distance in Miles From Downtown", title = "Average Distance From Downtown by Neighbourhood") + 
        theme_minimal() +theme_set(theme_bw(base_size = 20))+ 
        theme(legend.position = "none") + theme(axis.text.x=element_text(angle=90, hjust=1))
    }
    else if (input$var_1 == key_2[3])
    {
      AverageNumberofAmenitiesByNeighb<-aggregate(listing$NumberofAmenities,FUN="mean",by=list(listing$NeighbourWithOthers)) 
      colnames(AverageNumberofAmenitiesByNeighb)[1:2]<-c("Neighbourhood","AverageNumberofAmenities")
      
      
      #INCLUDE
      #AverageNumberofAmenitiesByNeighb[with(AverageNumberofAmenitiesByNeighb, order(-AverageNumberofAmenities)), ]
      
      
      ggplot(AverageNumberofAmenitiesByNeighb,aes(x=reorder(Neighbourhood,-AverageNumberofAmenities),y=AverageNumberofAmenities,fill=Neighbourhood))+
        geom_bar(stat = "identity")+
        geom_text(label = paste0(round(AverageNumberofAmenitiesByNeighb$AverageNumberofAmenities,2))) + 
        # scale_x_discrete(label = abbreviate) + 
        labs(x = "Neighbourhood", y = "Average Number of Amenities", title = "Average Number of Amenities by Neighbourhood") + 
        theme_minimal() +theme_set(theme_bw(base_size = 20))+ 
        theme(legend.position = "none") + theme(axis.text.x=element_text(angle=90, hjust=1))
    }
    else if (input$var_1 == key_2[4])
    {
      AverageReviewScoreByNeighb<-aggregate(listing$review_scores_rating,FUN="mean",by=list(listing$NeighbourWithOthers),na.rm=TRUE, na.action=NULL)
      colnames(AverageReviewScoreByNeighb)[1:2]<-c("Neighbourhood","AverageReviewRating")
      
      #INCLUDE
      #AverageReviewScoreByNeighb[with(AverageReviewScoreByNeighb, order(-AverageReviewRating)), ]
      
      ggplot(AverageReviewScoreByNeighb,aes(x=reorder(Neighbourhood,-AverageReviewRating),y=AverageReviewRating,fill=Neighbourhood))+
        geom_bar(stat = "identity")+
        geom_text(label = paste0(round(AverageReviewScoreByNeighb$AverageReviewRating,2))) + 
        # scale_x_discrete(label = abbreviate) + 
        labs(x = "Neighbourhood", y = "Average Review Score", title = "Average Review Score by Neighbourhood") + 
        theme_minimal() +theme_set(theme_bw(base_size = 20))+ 
        theme(legend.position = "none") + theme(axis.text.x=element_text(angle=90, hjust=1))
    }
    else
    {
      #based on https://www.investopedia.com/articles/managing-wealth/060916/10-most-expensive-zip-codes-chicago
      listing$rich<-ifelse(listing$zipcode=="60654","RIVER NORTH",ifelse(listing$zipcode=="60610","RIVER WEST",
                                                                         ifelse(listing$zipcode=="60611","STREETERVILLE",ifelse(listing$zipcode=="60614","OLD TOWN TRIANGLE",ifelse(listing$zipcode=="60657","LAKEVIEW1",
                                                                                                                                                                                    ifelse(listing$zipcode=="60613","LAKEVIEW2",
                                                                                                                                                                                           ifelse(listing$zipcode=="60605","SOUTH LOOP",
                                                                                                                                                                                                  ifelse(listing$zipcode=="60640","UPTOWN",
                                                                                                                                                                                                         
                                                                                                                                                                                                         ifelse(listing$zipcode=="60607","THE LOOP",
                                                                                                                                                                                                                ifelse(listing$zipcode=="60622","BUCKTOWN",
                                                                                                                                                                                                                       "OTHER"))))))))))
      
      AveragePriceByArea<-aggregate(listing$price,FUN="mean",by=list(listing$rich)) 
      colnames(AveragePriceByArea)[1:2]<-c("Area","Average_Price")
      
      #INCLUDE
      #AveragePriceByArea[with(AveragePriceByArea, order(-Average_Price)), ]
      
      ggplot(AveragePriceByArea,aes(x=reorder(Area,-Average_Price),y=Average_Price,fill=Area))+
        geom_bar(stat = "identity")+
        geom_text(label = paste0(round(AveragePriceByArea$Average_Price,2))) + 
        # scale_x_discrete(label = abbreviate) + 
        labs(x = "Neighbourhood", y = "Average Price", title = "Average Price by Area") + 
        theme_minimal() +theme_set(theme_bw(base_size = 20))+ 
        theme(legend.position = "none") + theme(axis.text.x=element_text(angle=90, hjust=1))
    }
  })
    
  
  output$mymap <- renderLeaflet({
      subs = listings[listings$neighbourhood==input$tabset1,]
      if(input$tabset1=="South Chicago")
        {subs_1 = subs[subs$room_type==input$loc1,]}
      if(input$tabset1=="South Shore")
        {subs_1 = subs[subs$room_type==input$loc2,]}
      
      coord_lat = subs_1$latitude[subs_1$price > input$range[1] & subs_1$price < input$range[2]]
      coord_lng = subs_1$longitude[subs_1$price > input$range[1] & subs_1$price < input$range[2]]
      coord <- cbind(coord_lat,coord_lng)
      if (nrow(coord) >= 1)
      {leaflet() %>% addTiles %>%setView(lng = coord_lng[1],lat=coord_lat[1],zoom=14) %>%  addMarkers(lng = coord_lng,lat=coord_lat,popup=subs_1$name)}
      else if (input$tabset1=="South Chicago")
      {
        leaflet() %>% addTiles %>%setView(lng = -87.5544, lat = 41.7397, zoom=14)
      }
      else
      {
        leaflet() %>% addTiles %>%setView(lng =  -87.5742 , lat = 41.7600, zoom=14)
      }
    })  
    
  

}

shinyApp(ui, server)