# app/logic/igv_helper.R

box::use(
  processx[process]
)

options(igv.port = 8080)

# Generate track objects for JavaScript
#' @export
build_igv_tracks <- function(samples) {
  port <- getOption("igv.port")
  tracks <- lapply(samples, function(sample) {
    sprintf("{
              name: '%s',
              url: 'http://127.0.0.1:%d/%s',
              indexURL: 'http://127.0.0.1:%d/%s.bai',
              format: 'bam'
            }", sample$name, port, sample$file, port, sample$file)
  })
  paste(tracks, collapse = ",\n")
}


# Starts CORS compatible server using npx http-server through processx
#' @export
start_static_server <- function(dir) {
  port <- getOption("igv.port")
  # Zjisti, jestli port už něco používá
  # server_check <- system(paste0("lsof -ti tcp:", port), intern = TRUE)
  # 
  # # Pokud ano – zabij starý proces
  # if (length(server_check) > 0) {
  #   message("⚠️ Port ", port, " je obsazený. Ukončuji předchozí proces...")
  #   for (pid in server_check) {
  #     system(paste("kill -9", pid))
  #   }
  #   Sys.sleep(1)
  # }
  assign("cors_server", process$new(
    "npx",
    c("http-server", dir, "-p", as.character(port), "--cors", "--no-cache"),
    stdout = NULL, stderr = NULL,
    supervise = TRUE
  ), envir = .GlobalEnv)
  message("Static server running on http://127.0.0.1:", port)
}

# Stops static server
#' @export
stop_static_server <- function() {
  if (exists("cors_server", envir = .GlobalEnv, inherits = FALSE)) {
    proc <- get("cors_server", envir = .GlobalEnv)
    if (!is.null(proc) && inherits(proc, "process") && proc$is_alive()) {
      proc$kill()
      message("Static server stoped")
    }
    rm("cors_server", envir = .GlobalEnv)
  } else {
    message("No static server running")
  }
}



