library(shiny)
library(shinydashboard)
library(tibble)
library(dplyr)
library(purrr)
library(ggforce)
library(ggiraph)

options(shiny.reactlog = TRUE)

windowsFonts(V = windowsFont("Verdana"),
             C = windowsFont("Cambria"))

#Get list of language families
uniqueFamilies <- c(setdiff(sort(unique(language$category)), "other"), "other")
familiesBySize <- c(setdiff(
  arrange(enframe(table(language$category)), desc(value))$name, "other"), "other")

#Client side ui
ui <- dashboardPage(
  skin = "green",
  
  dashboardHeader(title = "World Atlas of Language Structures: Language Family Comparison"),
  dashboardSidebar(
    #Select first language family
    selectInput(
      inputId = "family1",
      label = "Choose a language family",
      choices = familiesBySize,
      selected = "Indo-European"
    ),
    
    #Select second language family
    selectInput(
      inputId = "family2",
      label = "Choose another",
      choices = familiesBySize,
      selected = "Sino-Tibetan"
    ),
    
    #Slider input for number of bars in bar chart
    shinyWidgets::sliderTextInput(
      inputId = "charFeatureCount",
      label = "Choose how many features to display",
      choices = prettyNum(1:12),
      selected = "4",
      grid = FALSE, dragRange = FALSE, hide_min_max = TRUE
    ),
    
    #Allows selection of a feature to see how common it is amongst selected families
    selectInput(
      inputId = "releventFeature",
      label = "Select a feature to look at more closely",
      choices = add_row(topChars("Indo-European", 4), topChars("Sino-Tibetan", 4))$feature,
      selected = "122A Relativization on Subjects"
    ),
    
    #Sometimes two languages have the same characteristic feature, but different values
    radioButtons(
      inputId = "tiebreaker",
      label = "Choose which language family to prioritize for the pie chart",
      choices = c(1,2),
      selected = 1
    ),
    
    #Select third language family
    selectInput(
      inputId = "family3",
      label = "Choose a third family to compare to the first 2",
      choices = familiesBySize,
      selected = "Quechuan"
    )
  ),
  dashboardBody(
    fluidRow(
      box(
        #Display world map showing distribution of languages in selected families
        plotOutput(
          outputId = "worldMap",
          width = "100%",
          height = "600",
          click = clickOpts(
            id = "plot_click",
            clip = TRUE
          )
        )
      ),
      box(
        plotOutput(
          outputId = "likenessPlot",
          width = "100%",
          height = 500
        )
      )
    ),
    fluidRow(
      box(width = 8,
          plotOutput(
            outputId = "characteristicBars",
            width = "100%",
            height = 600
          )
      ),
      box(width = 4,
          plotOutput(
            outputId = "characteristicPie",
            width = "100%",
            height = 600
          )
      )
    )
  )
)

#Server side output
server <- function(input, output, session) {
  
  observe({updateRadioButtons(session, "tiebreaker",
                              choiceNames = c(input$family1, input$family2),
                              choiceValues = 1:2
  )})
  
  #Reactive timer is necessary to make click events work
  autoInvalidate <- reactiveTimer(2000)
  
  observe({
    autoInvalidate()
    
    possible <- nearPoints(
      df = filter(language, family == input$family1 | family == input$family2),
      coordinfo = input$plot_click,
      xvar = "longitude", yvar = "latitude",
      threshold = 10,
      maxpoints = 1
    )
    
    if (nrow(possible) != 0){
      assign(
        "maybeClicked",
        possible,
        envir = .GlobalEnv
      )
    }
  })
  
  #Create the plot used for the world map
  output$worldMap <- renderPlot({
    
    #Grab inputs
    fam1 <- input$family1
    fam2 <- input$family2
    
    #Remove languages not in selected families
    filteredLanguage <- filter(language, category == fam1 | category == fam2)
    
    
    if(maybeClicked$category != fam1 & maybeClicked$category != fam2){
      maybeClicked <- maybeClicked[0,]
    }
    
    #Determine limits manually to get a good aspect ratio
    xLimLow <- min(filteredLanguage$longitude)
    xLimHigh <- max(filteredLanguage$longitude)
    yLimLow <- min(filteredLanguage$latitude)
    yLimHigh <- max(filteredLanguage$latitude)
    
    #Get axis range from limits and divide width by height
    xRange <- xLimHigh - xLimLow
    yRange <- yLimHigh - yLimLow
    aspectRatio <- yRange / xRange
    
    #Get click data to label clicked point
    xClick <- as.numeric(input$plot_click[1])
    yClick <- as.numeric(input$plot_click[2])
    
    ggplot(
      filteredLanguage
    ) +
      geom_map(
        data = world,
        map = world,
        color = "white",
        fill = "lightgray",
        size = 0.1,
        aes(map_id = region)
      ) +
      #Shading clusters
      geom_mark_hull(
        aes(x = longitude, y = latitude, group = category, fill = category),
        concavity = 2,
        linetype = 0,
        alpha = 0.3,
        show.legend = FALSE
      ) +
      #Overlay points on world map
      geom_point(
        aes(x = longitude, y = latitude, fill = category),
        size = 5,
        shape = 21,
        color = "white",
        alpha = 0.5,
      ) +
      geom_point(
        data = maybeClicked,
        aes(x = longitude, y = latitude, fill = category),
        size = 5,
        shape = 23,
        stroke = 1.5,
        alpha = 1,
        show.legend = FALSE
      ) +
      #Language label on click
      ggrepel::geom_label_repel(
        data = maybeClicked,
        aes(x = longitude, y = latitude, label = Name),
        nudge_y = -5,
        show.legend = FALSE,
        alpha = 1,
        family = "V",
        size = 6,
        min.segment.length = 20,
        color = "bisque4"
      ) +
      #Fun language info
      geom_text(
        x = xLimHigh, y = yLimHigh,
        label = if(nrow(maybeClicked) != 0){
          sprintf("Language: %s, WALS Code: %s
                   Genus: %s, Family: %s
                   MacroArea: %s",
                  maybeClicked$Name,
                  maybeClicked$wals_code,
                  maybeClicked$genus,
                  maybeClicked$family,
                  maybeClicked$macroarea
          )
        } else "",
        hjust = "right",
        vjust = "top",
        family = "V",
        fontface = "italic"
      ) +
      #Set limits manually
      xlim(xLimLow, xLimHigh) +
      ylim(yLimLow, yLimHigh) +
      labs(
        title = sprintf("%s and %s Languages", fam1, fam2),
        caption = "Dryer, Matthew S. & Haspelmath, Martin (eds.) 2013.
                  The World Atlas of Language Structures Online.
                  Leipzig: Max Planck Institute for Evolutionary Anthropology.
                  Available online at http://wals.info, Accessed on 2022-03-10.",
        x = "", y = "", color = ""
      ) +
      guides(
        fill = guide_legend(
          title = NULL,
          nrow = 1,
          label.theme = element_text(family = "V", size = 15)
        )
      ) +
      theme(
        aspect.ratio = aspectRatio,
        
        #Textual elements
        title = element_text(family = "C", size = 18, face = "bold"),
        plot.caption = element_text(family = "V", size = 10),
        
        #Axes
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        
        #Legend
        legend.position = "bottom",
        legend.key = element_blank(),
        legend.justification = c(0,0),
        legend.box.spacing = unit(-16, "pt")
      )
  })
  
  #Create the bar charts used to show characteristic features
  output$characteristicBars <- renderPlot({
    
    #Grab inputs
    fam1 <- input$family1
    fam2 <- input$family2
    fCount <- input$charFeatureCount
    
    #Get the characteristic lists
    chars1 <- tibble(topChars(fam1, fCount), family = rep(fam1, fCount))
    chars2 <- tibble(topChars(fam2, fCount), family = rep(fam2, fCount))
    
    #Features have to be renamed to deal with factor level collisions
    preRename <- add_row(chars1, chars2) %>% arrange(score) %>% arrange(desc(family))
    #Factor level collisions arise from trying to sort the bars
    reFactor <- preRename %>% mutate(feature=factor(make.unique(feature), levels=make.unique(feature)))
    
    #Get duplicate features
    duplicates <- table(append(chars1$feature, chars2$feature)) %>% subset(., as.numeric(.) > 1) %>% names()
    
    ggplot(
      #Factor level collisions arise from trying to sort the bars
      data = reFactor,
      aes(x = feature, y = score)
    ) +
      geom_bar(
        data = reFactor,
        aes(fill = family),
        width = .4,
        stat = "identity",
        color = "black"
      ) +
      #Make the bars go horizontal for better labels
      coord_flip() +
      #Text info gives the features
      geom_text(
        y = 0,
        aes(label = paste0(sub("^\\d+[A-Z] ", "", sub(".\\d+$", "", feature)), ": ",
                           sub("^\\d ", "", value),
                           map(feature, function(f){
                             if(sub(".\\d+$", "", f) %in% duplicates)" (Shared)"else""}))),
        color = "black",
        family = "V",
        fontface = "bold",
        hjust = "left",
        size = 4,
        nudge_x = 0.5
      ) +
      labs(
        title = sprintf("%s and %s Languages:\nCharacteristic Features", fam1, fam2),
        x = "", y = ""
      ) +
      theme(
        #Textual elements
        title = element_text(family = "C", size = 16, face = "bold"),
        plot.margin= unit(c(0.5,0,0,0), 'cm'),
        #plot.background = element_rect(fill = "cornsilk2"),
        
        #Axes
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        
        #Legend
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(family = "V", size = 14),
        legend.title = element_blank(),
        
        #Panel
        panel.background = element_blank(),
        panel.grid = element_blank()
      )
  })
  
  #Create a pie chart!
  output$characteristicPie <- renderPlot({
    
    #Grab inputs
    fam1 <- input$family1
    fam2 <- input$family2
    fCount <- input$charFeatureCount
    
    #Get the characteristic lists
    chars1 <- tibble(topChars(fam1, fCount), family = rep(fam1, fCount))
    chars2 <- tibble(topChars(fam2, fCount), family = rep(fam2, fCount))
    
    #Update feature selector input if needed
    ch <- add_row(chars1, chars2)$feature
    updateSelectInput(session, "releventFeature",
                      choices = ch,
                      selected = if(input$releventFeature %in% ch) input$releventFeature else ch[1]
    )
    
    #If there are two relevent values, use tiebreaker radio buttons
    combin <- if(input$tiebreaker == "1"){
      add_row(chars1, chars2)} else {
        add_row(chars2, chars1)
      }
    releventValue <- combin %>% filter(feature == input$releventFeature) %>% .$value
    
    pie_data <- filter(language, language[[input$releventFeature]] == releventValue[1]) %>%
      .[["family"]] %>%
      map(function(f){
        if(f == fam1 | f == fam2) f else "Other"
      }) %>% as_vector %>% table %>% enframe %>% mutate(value = as.numeric(value))
    
    ggplot(data = pie_data,
           aes(x = "", y = value, fill = name)) +
      geom_bar(
        stat = "identity",
        width = 1,
        show.legend = FALSE,
        color = "white",
        size = 3
      ) + 
      coord_polar("y", start = 0) +
      theme_void() +
      geom_text(
        aes(label = name),
        color = "black",
        family = "C",
        fontface = "bold",
        size = 5,
        position = position_stack(vjust = 0.5)
      ) +
      scale_fill_brewer(palette = "Greens", direction = -1) +
      labs(
        title = paste0(input$releventFeature, ":\n",
                       releventValue)
      ) +
      theme(
        title = element_text(
          family = "C",
          size = 13,
          face = "bold"),
        plot.margin= unit(c(0.5,0,0,0), 'cm')
      )
  })
  
  output$likenessPlot <- renderPlot({
    
    #Grab inputs
    fam1 <- input$family1
    fam2 <- input$family2
    fam3 <- input$family3
    
    #Sort by most researched languages (those with most features defined)
    rank1 <- arrange(filter(language, family == fam1), desc(research))
    rank2 <- arrange(filter(language, family == fam2), desc(research))
    rank3 <- arrange(filter(language, family == fam3), desc(research))
    
    
    filtered <- add_row(rank1[1:min(36, nrow(rank1)),],
                        add_row(rank2[1:min(36, nrow(rank2)),],
                                rank3[1:min(36, nrow(rank3)),]
                        ))
    languageNames <- filtered$Name
    languageFamilies <- filtered$category
    likeness <- tibble(Name = languageNames,
                       like1 = map_dbl(languageNames, function(x){compareTo(x, fam1)}),
                       like2 = map_dbl(languageNames, function(x){compareTo(x, fam2)}),
                       family = languageFamilies)
    ggplot(
      data = likeness
    ) +
      geom_point(
        aes(x = like1, y = like2, fill = family),
        size = 6,
        shape = 21,
        color = "white",
        alpha = 0.7
      ) +
      labs(
        title = paste0(fam3," in terms of ",fam1, " and ",fam2),
        x = paste0(fam1, "-ness"),
        y = paste0(fam2, "-ness")
      ) +
      guides(
        fill = guide_legend(
          title = NULL,
          label.theme = element_text(family = "V", size = 15)
        )
      ) +
      theme(
        #Textual elements
        title = element_text(family = "C", size = 16, face = "bold"),
        
        #Axes
        axis.title = element_text(family = "V", size = 13, face = "plain"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        
        #Legend
        legend.key = element_blank(),
      )
  })
}

#Generate the shiny app
shinyApp(ui = ui, server = server)