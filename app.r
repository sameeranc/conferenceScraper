# app.R
library(shiny)
library(rvest)
library(httr)
library(taskscheduleR)
library(gh)
library(dotenv)

# Load environment variables
load_dot_env()

# Function to scrape conferences
search_conferences <- function() {
  tryCatch({
    # Set up headers for the request
    headers <- c(
      `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
    )
    
    # Make the request to Google
    url <- "https://www.google.com/search?q=upcoming+data+science+conferences"
    page <- GET(url, add_headers(.headers = headers))
    
    # Parse the HTML
    content <- read_html(page)
    
    # Extract search results
    results <- content %>%
      html_nodes("div.g") %>%
      map_df(function(result) {
        title <- result %>% 
          html_node("h3") %>% 
          html_text()
        
        link <- result %>%
          html_node("a") %>%
          html_attr("href")
        
        data.frame(
          title = title,
          link = link,
          stringsAsFactors = FALSE
        )
      })
    
    return(results)
  }, error = function(e) {
    print(paste("Error in search_conferences:", e$message))
    return(data.frame())
  })
}

# Function to update GitHub page
update_github_page <- function(conferences) {
  tryCatch({
    # Create HTML content
    html_content <- sprintf('
    <!DOCTYPE html>
    <html>
    <head>
        <title>Data Science Conferences</title>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <style>
            body { font-family: Arial, sans-serif; margin: 40px; }
            .conference { margin-bottom: 20px; padding: 10px; border-bottom: 1px solid #eee; }
        </style>
    </head>
    <body>
        <h1>Data Science Conferences</h1>
        <p>Last updated: %s</p>
        <div class="conferences">
            %s
        </div>
    </body>
    </html>
    ',
                            format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                            paste(sprintf('
        <div class="conference">
            <h2><a href="%s">%s</a></h2>
        </div>
    ', conferences$link, conferences$title), collapse = "\n")
    )
    
    # Update GitHub using gh package
    gh_token <- Sys.getenv("GITHUB_TOKEN")
    gh_repo <- Sys.getenv("GITHUB_REPO")
    
    # Try to get existing file
    tryCatch({
      existing_file <- gh("/repos/:owner/:repo/contents/index.html",
                          owner = gh_repo_owner,
                          repo = gh_repo,
                          ref = "gh-pages")
      
      # Update existing file
      gh("PUT /repos/:owner/:repo/contents/index.html",
         owner = gh_repo_owner,
         repo = gh_repo,
         message = sprintf("Update conferences %s", format(Sys.time(), "%Y-%m-%d")),
         content = base64enc::base64encode(charToRaw(html_content)),
         sha = existing_file$sha,
         branch = "gh-pages")
    }, error = function(e) {
      # Create new file if it doesn't exist
      gh("PUT /repos/:owner/:repo/contents/index.html",
         owner = gh_repo_owner,
         repo = gh_repo,
         message = sprintf("Initial conferences %s", format(Sys.time(), "%Y-%m-%d")),
         content = base64enc::base64encode(charToRaw(html_content)),
         branch = "gh-pages")
    })
    
    return(TRUE)
  }, error = function(e) {
    print(paste("Error in update_github_page:", e$message))
    return(FALSE)
  })
}

# Daily task function
daily_task <- function() {
  message(sprintf("Running daily update at %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  conferences <- search_conferences()
  if (nrow(conferences) > 0) {
    success <- update_github_page(conferences)
    message(sprintf("Update %s", if(success) "successful" else "failed"))
  } else {
    message("No conferences found")
  }
}

# Shiny UI
ui <- fluidPage(
  titlePanel("Conference Scraper Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("run_now", "Run Scraper Now"),
      hr(),
      helpText("Scraper runs automatically at 6 AM daily")
    ),
    
    mainPanel(
      verbatimTextOutput("log")
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  # Log storage
  logs <- reactiveVal(character(0))
  
  # Schedule daily task
  observe({
    invalidateLater(60000) # Check every minute
    current_time <- format(Sys.time(), "%H:%M")
    if (current_time == "06:00") {
      daily_task()
      logs(c(logs(), sprintf("Scheduled task run at %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))))
    }
  })
  
  # Manual run button
  observeEvent(input$run_now, {
    daily_task()
    logs(c(logs(), sprintf("Manual task run at %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))))
  })
  
  # Display logs
  output$log <- renderPrint({
    cat(paste(logs(), collapse = "\n"))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)