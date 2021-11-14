library(tidyverse)
library(shiny)
library(glue)
library(rvest)

# ---- Load Data ---------------------------------------------------------------
pokemon <- read_csv("data/FirstGenPokemon.csv") |> 
    mutate(
      number = str_pad(Number, width = "3", pad = "0", side = "left"),
      name = Name
    ) |> 
    arrange(number)

classification <- read_csv("data/pokemon.csv") |> 
    select(name, classification)
  
pokemon <- left_join(pokemon, classification)



# ---- Extract where to catch --------------------------------------------------
where_to_catch <- \(name) {
    if(name == "Mr. Mime") name <- "mr-mime"
    if(name == "Nidoran (M)") name <- "nidoran-m"
    if(name == "Nidoran (F)") name <- "nidoran-f"
  
    simple <- read_html(glue("https://pokemondb.net/pokedex/{str_to_lower(name)}"))
    
    rows <- simple |> 
      # Where to find
      html_elements("h2:contains('Where to find') + div") |> 
      html_elements("table") |> 
      html_elements("tr")
      
    location <- lapply(rows, \(row){ 
      if(!is.na(html_element(row, ".igame.red"))) {
        row |> 
          html_element("td") |> 
          html_text()
      }
    })
    
    location <- unlist(location)
    
    return(location)
}


ui <- div(id = "pokemon_color", class="min-h-screen flex flex-col text-gray-800",
    # ---- TailwindCSS ---------------------------------------------------------
    shiny.tailwind::use_tailwind(
      css = c("custom.css"), version = 3,
      # Custom coral color
      tailwindConfig = "tailwind.config.js"
    ),
    # ---- Header --------------------------------------------------------------
    div(
        id="header", 
        class="max-w-xl w-[36rem] mx-auto pt-16 pb-4 flex flex-col",
        # img(src="https://upload.wikimedia.org/wikipedia/commons/9/98/International_Pok%C3%A9mon_logo.svg"),
        # ---- Pokemon Select --------------------------------------------------
        h1(class="w-full pt-4 pb-2 tracking-wide font-bold text-2xl", "Select a Pokemon"),
        selectizeInput("pokemon_name", label="", choices= pokemon$name, selected="Squirtle")
    ),
    # ---- Body ----------------------------------------------------------------
    div(
        class="flex-grow w-full flex  justify-center",
        # ---- Pokedex ---------------------------------------------------------
        div(class = "flex flex-col w-full ",  
            
            # ---- Pokemon Image -----------------------------------------------
            div(class="relative w-full px-2 sm:px-8 py-4 ", id="pokemon_highlight", 
                div(id="pokemon_card", class="max-w-xl mx-auto py-4 px-2 sm:px-8 bg-white backdrop-filter backdrop-blur-lg bg-opacity-20 shadow-lg rounded-lg border-2 border-white",
                    # ---- Card Header -----------------------------------------
                    div(class="flex justify-between items-end -mt-24",
                        div(class="flex flex-col",
                            uiOutput("pokemon_header")
                        ),
                        # imageOutput without defaults
                        # img() in there for initialization
                        div(id="pokemon_img", class="shiny-html-output", img())
                    ),
                    # ---- Info ------------------------------------------------
                    div(class="py-4",
                        uiOutput("pokemon_bio")
                    ),
                    # ---- Stats -----------------------------------------------
                    div(class="py-4",
                        uiOutput("pokemon_stats")
                    ),
                    # ---- Types multiplier ------------------------------------
                    div(class="pt-8 ",
                        uiOutput("pokemon_rel")
                    ),
                    # ---- Where to catch --------------------------------------
                    div(class="pt-8 ",
                        uiOutput("pokemon_catch")
                    )
                )
            )
        )
    ),
    
    # ---- JS Scripts ----------------------------------------------------------
    tags$script(src="jquery.adaptive-backgrounds.js"),
    tags$script(src="pokemon.js")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$pokemon_header <- renderUI({
        idx <- pokemon$name == input$pokemon_name
        name <- pokemon[idx, "name"]
        num <- pokemon[idx, "number"]
        type <- str_to_title(pokemon[idx, "Type1"])
      
        tagList(
          p(class="text-sm text-gray-500", glue("#{num}")),
          div(class="flex flex-row items-baseline",
              h2(class="text-gray-800 text-2xl sm:text-4xl font-bold", name),
          ),
          div(class="px-1 flex items-center",
              img(class="w-5 h-5 mr-2", src=glue("types/{type}.png")),
              span(class="text-lg", type)
          )
        )
    })
    
    output$pokemon_img <- renderUI({
      idx <- pokemon$name == input$pokemon_name
      num <- pokemon[idx, "number"]
      name <- pokemon[idx, "name"]
      
      path <- glue("https://raw.githubusercontent.com/kylebutts/RStudio_2021_Table_Comp/main/www/pokemon/{num}{name}.png")
      if(name == "Mr. Mime") path <- "https://raw.githubusercontent.com/kylebutts/RStudio_2021_Table_Comp/main/www/pokemon/122Mr Mime.png"
      
      tagList(
        tags$script(HTML("
            $.adaptiveBackground.run({parent:'#pokemon_color'});
            var col = $('#pokemon_color').css('background-color');
        ")),
        img(
          src=path, 'data-adaptive-background'='1', class="w-48 h-48 object-cover "
        )
      )
    })
    
    
    output$pokemon_bio <- renderUI({
        idx <- pokemon$name == input$pokemon_name
        name <- pokemon[idx, "name"]
        num <- pokemon[idx, "number"]
        type <- str_to_title(pokemon[idx, "Type1"])
        height <- pokemon[idx, "Height(m)"]
        weight <- pokemon[idx, "Weight(kg)"]
        capture <- pokemon[idx, "Capt_Rate"]
        cat <- pokemon[idx, "classification"]
        cat <- str_remove(cat, " Pokémon")
        
        div(class="grid grid-cols-4",
            div(class="", 
                h2(class="text-xs font-bold", "Height"),
                p(class="font-light text-lg", glue("{height}m"))
            ),
            div(class="", 
                h2(class="text-xs font-bold", "Weight"),
                p(class="font-light text-lg", glue("{weight}kg"))
            ),
            div(class="",
                h2(class="text-xs font-bold", "Capture Difficulty"),
                p(class="font-light text-lg", glue("{capture}"))
            ),
            div(class="", 
                h2(class="text-xs font-bold", "Category"),
                p(class="font-light text-lg", cat)
            ),
        )
    })
    
    output$pokemon_stats <- renderUI({
      idx <- pokemon$name == input$pokemon_name
      HP <- sprintf("%1.0f%%", 100 * pokemon[idx, "HP"] / max(pokemon[, "HP"]))
      Attack <- sprintf("%1.0f%%", 100 * pokemon[idx, "Attack"] / max( pokemon[, "Attack"]) )
      Defense <- sprintf("%1.0f%%", 100 * pokemon[idx, "Defense"] / max( pokemon[, "Defense"]))
      Special <- sprintf("%1.0f%%", 100 * pokemon[idx, "Special"] / max( pokemon[, "Special"]) )
      Speed <- sprintf("%1.0f%%", 100 * pokemon[idx, "Speed"] / max( pokemon[, "Speed"]))
      
      stats <- purrr::imap(
          list("HP" = HP, "Attack" = Attack, "Defense" = Defense, "Special" = Special, "Speed" = Speed), 
          \(x, y) {
            div(class="flex flex-col h-48 justify-end",
                span(class="text-center font-semibold", x),
                div(class = glue("stat-bar w-3/5 mx-auto h-[{x}] bg-white ")),
                h3(class="pt-2 text-center font-light tracking-wide", y)
            )
      })
      
      tagList(
        h2(class="text-xs font-bold", "Base Stats"),
        div(class="grid grid-cols-5",
            stats
        )
      )
    })
    
    output$pokemon_rel <- renderUI({
      idx <- pokemon$name == input$pokemon_name
      
      types <- c("Normal_Dmg", "Fire_Dmg", "Water_Dmg", "Electric_Dmg", "Grass_Dmg", "Ice_Dmg", "Fighting_Dmg", "Poison_Dmg", "Ground_Dmg", "Flying_Dmg", "Psychic_Dmg", "Bug_Dmg", "Rock_Dmg", "Ghost_Dmg", "Dragon_Dmg")
      
      weak <- purrr::map(types, \(x) {
          if(pokemon[idx, x] == 0.5) {
              type <- str_remove(x, "_Dmg")
              img(class = "w-5 h-5", 
                  src = glue("types/{type}.png"), 
                  title = type
              )
          }
      })
      
      average <- purrr::map(types, \(x) {
        if(pokemon[idx, x] == 1) {
          type <- str_remove(x, "_Dmg")
          img(class = "w-5 h-5", 
              src = glue("types/{type}.png"), 
              title = type
          )
        }
      })
      
      strong <- purrr::map(types, \(x) {
        if(pokemon[idx, x] == 2) {
          type <- str_remove(x, "_Dmg")
          img(class = "w-5 h-5", 
              src = glue("types/{type}.png"), 
              title = type
          )
        }
      })
      
      
      div(class="grid grid-flow-row grid-cols-3 space-x-2",
          div(
            h2(class="text-xs font-bold", "Weak Against"),
            div(class="flex flex-row flex-wrap gap-1 mt-2",
                weak
            ),
          ),
          div(
            h2(class="text-xs font-bold", "Average Against"),
            div(class="flex flex-row flex-wrap gap-1 mt-2",
                average
            ),
          ),
          div(
            h2(class="text-xs font-bold", "Strong Against"),
            div(class="flex flex-row flex-wrap gap-1 mt-2",
                strong
            ),
          )
      )
    })
    
    output$pokemon_catch <- renderUI({
      tagList(
        h2(class="text-xs font-bold", "Where to get in Pokemon Red/Blue/Yellow"),
        p(class="pt-2", where_to_catch(input$pokemon_name))
      )
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)
