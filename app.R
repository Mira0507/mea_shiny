project_root <- normalizePath(".", mustWork = TRUE)
local_r_lib <- file.path(project_root, "env", "lib", "R", "library")
if (dir.exists(local_r_lib)) {
  .libPaths(c(local_r_lib, .libPaths()))
}
local_r_bin <- file.path(project_root, "env", "bin")
if (dir.exists(local_r_bin)) {
  path_entries <- strsplit(Sys.getenv("PATH"), .Platform$path.sep, fixed = TRUE)[[1]]
  if (!local_r_bin %in% path_entries) {
    Sys.setenv(PATH = paste(c(local_r_bin, path_entries), collapse = .Platform$path.sep))
  }
  if (file.exists(file.path(local_r_bin, "pandoc"))) {
    Sys.setenv(RSTUDIO_PANDOC = local_r_bin)
  }
}

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(dplyr)
  library(readxl)
  library(rmarkdown)
  library(stringr)
  library(tibble)
})

analysis_rmd <- file.path(
  project_root,
  "analysis",
  "MEA_organoids_spont_basesline_local_paths.Rmd"
)
input_dir <- file.path(project_root, "input")
plot_dir <- file.path(
  project_root,
  "output"
)

if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
}
addResourcePath("analysis-output", plot_dir)

relative_path <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  str_remove(path, paste0("^", fixed(root), "/?"))
}

load_metadata_xlsx <- function(filepath) {
  raw <- read_xlsx(filepath, col_names = FALSE, .name_repair = "minimal")
  row_labels <- as.character(raw[[1]])

  well_row_idx <- which(str_trim(str_to_upper(row_labels)) == "MEA WELL")[1]
  if (is.na(well_row_idx)) {
    stop("Could not find 'MEA WELL' row in: ", filepath)
  }

  wells <- as.character(raw[well_row_idx, -1])
  wells <- wells[!is.na(wells) & wells != "NA"]
  n_wells <- length(wells)

  get_row <- function(label) {
    idx <- which(str_trim(str_to_upper(row_labels)) == str_to_upper(label))[1]
    if (is.na(idx)) {
      return(rep(NA_character_, n_wells))
    }
    vals <- as.character(raw[idx, 2:(n_wells + 1)])
    vals[vals == "NA"] <- NA_character_
    vals
  }

  tibble(
    well = wells,
    recorded = get_row("RECORDED"),
    genotype = get_row("GENOTYPE"),
    protocol = get_row("PROTOCOL"),
    age_weeks = suppressWarnings(as.numeric(get_row("AGE (WEEKS)"))),
    batch = suppressWarnings(as.numeric(get_row("BATCH"))),
    n_organoid = suppressWarnings(as.numeric(get_row("#ORGANOID"))),
    n_slice = suppressWarnings(as.numeric(get_row("#SLICE")))
  ) |>
    filter(is.na(recorded) | str_to_upper(recorded) == "TRUE")
}

metadata_inventory <- function() {
  files <- list.files(input_dir, pattern = "\\.xlsx$", recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) {
    return(tibble(
      metadata_file = character(),
      experiment_folder = character(),
      plate = character(),
      source_path = character(),
      well = character(),
      recorded = character(),
      genotype = character(),
      protocol = character(),
      age_weeks = numeric(),
      batch = numeric(),
      n_organoid = numeric(),
      n_slice = numeric()
    ))
  }

  bind_rows(lapply(files, function(path) {
    tryCatch(
      {
        load_metadata_xlsx(path) |>
          mutate(
            metadata_file = basename(path),
            experiment_folder = basename(dirname(path)),
            plate = str_to_upper(str_extract(basename(path), regex("plate\\d+", ignore_case = TRUE))),
            source_path = relative_path(path),
            .before = 1
          )
      },
      error = function(err) {
        tibble(
          metadata_file = basename(path),
          experiment_folder = basename(dirname(path)),
          plate = NA_character_,
          source_path = relative_path(path),
          well = NA_character_,
          recorded = NA_character_,
          genotype = paste("ERROR:", err$message),
          protocol = NA_character_,
          age_weeks = NA_real_,
          batch = NA_real_,
          n_organoid = NA_real_,
          n_slice = NA_real_
        )
      }
    )
  }))
}

input_file_inventory <- function() {
  files <- list.files(input_dir, recursive = TRUE, full.names = TRUE)
  files <- files[file.info(files)$isdir %in% FALSE]
  if (length(files) == 0) {
    return(tibble(
      file = character(),
      type = character(),
      size_mb = numeric(),
      modified = character(),
      path = character()
    ))
  }

  info <- file.info(files)
  tibble(
    file = basename(files),
    type = case_when(
      str_detect(file, "\\.xlsx$") ~ "Metadata",
      str_detect(file, "spont.*spike_list.*\\.csv$") ~ "Spont spike list",
      str_detect(file, "spike_counts.*\\.csv$") ~ "Spike counts",
      str_detect(file, "burst_list.*\\.csv$") ~ "Burst list",
      str_detect(file, "environmental_data.*\\.csv$") ~ "Environmental data",
      str_detect(file, "\\.spk$") ~ "SPK",
      str_detect(file, "\\.png$") ~ "Image",
      TRUE ~ "Other"
    ),
    size_mb = round(info$size / 1024^2, 3),
    modified = format(info$mtime, "%Y-%m-%d %H:%M"),
    path = vapply(files, relative_path, character(1))
  ) |>
    arrange(type, path)
}

plot_category <- function(file) {
  case_when(
    str_detect(file, "_1_violin_amplitude_") ~ "amplitude_violin",
    str_detect(file, "_1b_sina_mfr_") ~ "mfr_sina",
    str_detect(file, "_2_heatmap_amplitude") ~ "amplitude_heatmap",
    str_detect(file, "_3_heatmap_mfr") ~ "mfr_heatmap",
    str_detect(file, "_4_raster_") ~ "raster",
    str_detect(file, "_5_isi_ridgeline") ~ "isi",
    TRUE ~ "other"
  )
}

parse_plot_file <- function(path) {
  file <- basename(path)
  category <- plot_category(file)
  stem <- str_remove(file, "\\.png$")
  parts <- str_split(stem, "_", simplify = FALSE)[[1]]

  protocol <- if (length(parts) >= 1) parts[[1]] else NA_character_
  experiment <- NA_character_
  plate <- NA_character_
  well <- NA_character_
  age_weeks <- suppressWarnings(as.numeric(str_match(file, "_(\\d+)wk")[, 2]))

  if (category %in% c("raster", "isi") && length(parts) >= 2) {
    experiment <- parts[[2]]
  }

  if (category %in% c("amplitude_heatmap", "mfr_heatmap")) {
    is_legend <- str_detect(file, "_LEGEND\\.png$")
    if (is_legend && length(parts) >= 2) {
      experiment <- parts[[2]]
    } else if (length(parts) >= 5) {
      experiment <- parts[[2]]
      well <- parts[[length(parts) - 3]]
      plate_parts <- parts[3:(length(parts) - 4)]
      plate <- paste(plate_parts, collapse = "_")
    }
  }

  tibble(
    path = path,
    file = file,
    src = paste0("analysis-output/", URLencode(file, reserved = TRUE)),
    category = category,
    protocol = protocol,
    experiment = experiment,
    plate = plate,
    well = well,
    age_weeks = age_weeks,
    is_legend = str_detect(file, "_LEGEND\\.png$"),
    modified = file.info(path)$mtime,
    size_mb = round(file.info(path)$size / 1024^2, 3)
  )
}

plot_inventory <- function() {
  files <- list.files(plot_dir, pattern = "\\.png$", full.names = TRUE)
  if (length(files) == 0) {
    return(tibble(
      path = character(),
      file = character(),
      src = character(),
      category = character(),
      protocol = character(),
      experiment = character(),
      plate = character(),
      well = character(),
      age_weeks = numeric(),
      is_legend = logical(),
      modified = as.POSIXct(character()),
      size_mb = numeric()
    ))
  }

  bind_rows(lapply(files, parse_plot_file)) |>
    arrange(category, protocol, experiment, plate, well, file)
}

report_zip_file <- function() {
  file.path(plot_dir, "report.zip")
}

create_report_zip <- function() {
  if (!dir.exists(plot_dir)) {
    return(NA_character_)
  }

  output_entries <- list.files(plot_dir, all.files = FALSE, full.names = FALSE)
  output_entries <- setdiff(output_entries, "report.zip")
  if (length(output_entries) == 0) {
    return(NA_character_)
  }

  temp_zip <- tempfile(fileext = ".zip")
  old_wd <- setwd(project_root)
  on.exit({
    setwd(old_wd)
    if (file.exists(temp_zip)) {
      unlink(temp_zip)
    }
  }, add = TRUE)

  zip_status <- utils::zip(
    zipfile = temp_zip,
    files = "output",
    flags = "-r9Xq",
    extras = "-x output/report.zip"
  )
  if (!identical(zip_status, 0L)) {
    stop("Failed to create report.zip")
  }

  target <- report_zip_file()
  if (file.exists(target)) {
    unlink(target)
  }
  if (!file.copy(temp_zip, target, overwrite = TRUE)) {
    stop("Failed to copy report.zip into output")
  }

  target
}

stat_box <- function(label, value) {
  div(
    class = "stat-box",
    div(class = "stat-value", value),
    div(class = "stat-label", label)
  )
}

empty_state <- function(message) {
  div(class = "empty-state", message)
}

plot_tile <- function(row, image_class = "plot-img") {
  div(
    class = "plot-tile",
    a(
      href = row$src,
      target = "_blank",
      img(src = row$src, class = image_class, alt = row$file)
    ),
    div(class = "plot-caption", row$file),
    div(
      class = "plot-meta",
      paste0("Updated ", format(row$modified, "%Y-%m-%d %H:%M"), " | ", row$size_mb, " MB")
    )
  )
}

gallery_ui <- function(rows, image_class = "plot-img") {
  if (nrow(rows) == 0) {
    return(empty_state("No plots found."))
  }

  div(
    class = "gallery-grid",
    lapply(seq_len(nrow(rows)), function(i) plot_tile(rows[i, ], image_class))
  )
}

heatmap_view_ui <- function(rows, legend_rows) {
  if (nrow(rows) == 0) {
    return(empty_state("No heatmaps found."))
  }

  div(
    class = "heatmap-view",
    div(
      class = "heatmap-main",
      gallery_ui(rows, "heatmap-img")
    ),
    div(
      class = "heatmap-legend",
      if (nrow(legend_rows) == 0) {
        empty_state("No legend found.")
      } else {
        gallery_ui(legend_rows, "legend-img")
      }
    )
  )
}

ui <- page_navbar(
  title = "MEA Shiny Analysis",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#216869",
    secondary = "#4d5360",
    success = "#558b2f",
    warning = "#b26a00"
  ),
  header = tags$head(
    tags$style(HTML("
      body {
        background: #f6f7f8;
        color: #20242a;
        font-family: Inter, Arial, sans-serif;
      }
      .navbar {
        border-bottom: 1px solid #d7dcdf;
      }
      .page-wrap {
        max-width: 1480px;
        margin: 0 auto;
        padding: 18px 18px 32px;
      }
      .toolbar {
        display: flex;
        flex-wrap: wrap;
        align-items: center;
        gap: 10px;
        margin-bottom: 14px;
      }
      .run-status {
        color: #4d5360;
        font-size: 0.92rem;
      }
      .stats-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
        gap: 10px;
        margin: 12px 0 16px;
      }
      .stat-box {
        background: #ffffff;
        border: 1px solid #d7dcdf;
        border-radius: 8px;
        padding: 12px 14px;
      }
      .stat-value {
        font-size: clamp(1.35rem, 2.4vw, 1.9rem);
        line-height: 1.1;
        font-weight: 720;
        color: #1f4f4f;
      }
      .stat-label {
        margin-top: 3px;
        font-size: 0.82rem;
        color: #5b626c;
      }
      .section-title {
        margin: 20px 0 10px;
        font-size: 1.05rem;
        font-weight: 700;
      }
      .gallery-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(320px, 1fr));
        gap: 14px;
        align-items: start;
      }
      .plot-tile {
        background: #ffffff;
        border: 1px solid #d7dcdf;
        border-radius: 8px;
        padding: 10px;
        min-width: 0;
      }
      .plot-img,
      .heatmap-img,
      .legend-img {
        display: block;
        width: 100%;
        height: auto;
      }
      .heatmap-img {
        max-height: 68vh;
        object-fit: contain;
      }
      .legend-img {
        max-height: 160px;
        object-fit: contain;
      }
      .plot-caption {
        margin-top: 9px;
        font-size: 0.84rem;
        font-weight: 650;
        color: #20242a;
        overflow-wrap: anywhere;
      }
      .plot-meta {
        margin-top: 2px;
        font-size: 0.75rem;
        color: #6a717b;
      }
      .empty-state {
        background: #ffffff;
        border: 1px dashed #b7bec5;
        border-radius: 8px;
        color: #5b626c;
        padding: 18px;
      }
      .heatmap-controls {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(190px, 1fr));
        gap: 10px;
        margin-bottom: 14px;
      }
      .heatmap-view {
        display: grid;
        grid-template-columns: minmax(280px, 1fr) minmax(260px, 420px);
        gap: 14px;
        align-items: start;
      }
      .heatmap-legend .gallery-grid {
        grid-template-columns: 1fr;
      }
      table.dataTable {
        width: 100% !important;
      }
      @media (max-width: 900px) {
        .page-wrap {
          padding: 12px;
        }
        .heatmap-view {
          grid-template-columns: 1fr;
        }
      }
    "))
  ),
  nav_panel(
    "Metadata",
    div(
      class = "page-wrap",
      div(
        class = "toolbar",
        actionButton("run_analysis", "Run analysis", icon = icon("play")),
        actionButton("refresh_outputs", "Refresh", icon = icon("rotate")),
        uiOutput("run_status", inline = TRUE)
      ),
      uiOutput("metadata_stats"),
      div(class = "section-title", "Input metadata"),
      DTOutput("metadata_table")
    )
  ),
  nav_panel(
    "Input Files",
    div(
      class = "page-wrap",
      div(class = "section-title", "Input files"),
      DTOutput("input_files_table")
    )
  ),
  nav_panel(
    "Amplitude",
    div(
      class = "page-wrap",
      uiOutput("amplitude_violin_gallery")
    )
  ),
  nav_panel(
    "Firing Rate",
    div(
      class = "page-wrap",
      uiOutput("mfr_gallery")
    )
  ),
  nav_panel(
    "Amplitude Heatmaps",
    div(
      class = "page-wrap",
      uiOutput("amplitude_heatmap_controls"),
      uiOutput("amplitude_heatmap_view")
    )
  ),
  nav_panel(
    "MFR Heatmaps",
    div(
      class = "page-wrap",
      uiOutput("mfr_heatmap_controls"),
      uiOutput("mfr_heatmap_view")
    )
  ),
  nav_panel(
    "Raster",
    div(
      class = "page-wrap",
      uiOutput("raster_gallery")
    )
  ),
  nav_panel(
    "ISI",
    div(
      class = "page-wrap",
      uiOutput("isi_gallery")
    )
  ),
  nav_panel(
    "Report",
    div(
      class = "page-wrap",
      uiOutput("report_link"),
      uiOutput("report_zip_link"),
      uiOutput("analysis_log")
    )
  )
)

server <- function(input, output, session) {
  refresh_key <- reactiveVal(Sys.time())
  analysis_log <- reactiveVal(character())
  run_state <- reactiveVal("Ready")

  metadata_data <- reactive({
    refresh_key()
    metadata_inventory()
  })

  input_files_data <- reactive({
    refresh_key()
    input_file_inventory()
  })

  plots_data <- reactive({
    refresh_key()
    plot_inventory()
  })

  observeEvent(input$refresh_outputs, {
    refresh_key(Sys.time())
    run_state("Refreshed")
  })

  observeEvent(input$run_analysis, {
    req(file.exists(analysis_rmd))
    run_state("Running")
    analysis_log(character())

    withProgress(message = "Running analysis", value = 0.2, {
      log <- tryCatch(
        {
          captured <- capture.output(
            rmarkdown::render(
              input = analysis_rmd,
              output_dir = plot_dir,
              envir = new.env(parent = globalenv()),
              quiet = TRUE
            ),
            type = "output"
          )
          incProgress(0.7)
          run_state("Completed")
          captured
        },
        error = function(err) {
          run_state("Failed")
          paste("ERROR:", conditionMessage(err))
        }
      )
      analysis_log(log)
      refresh_key(Sys.time())
      incProgress(0.1)
    })
  })

  output$run_status <- renderUI({
    state <- run_state()
    last_plot <- plots_data() |>
      pull(modified) |>
      max(na.rm = TRUE)

    if (!is.finite(last_plot)) {
      last_text <- "No plot outputs"
    } else {
      last_text <- paste("Latest output", format(last_plot, "%Y-%m-%d %H:%M"))
    }

    span(class = "run-status", paste(state, "|", last_text))
  })

  output$metadata_stats <- renderUI({
    meta <- metadata_data()
    files <- input_files_data()
    plots <- plots_data()

    div(
      class = "stats-grid",
      stat_box("Experiments", dplyr::n_distinct(meta$experiment_folder, na.rm = TRUE)),
      stat_box("Metadata wells", nrow(meta)),
      stat_box("Protocols", dplyr::n_distinct(meta$protocol, na.rm = TRUE)),
      stat_box("Genotypes", dplyr::n_distinct(meta$genotype, na.rm = TRUE)),
      stat_box("Input files", nrow(files)),
      stat_box("Plot outputs", nrow(plots))
    )
  })

  output$metadata_table <- renderDT({
    metadata_data() |>
      arrange(experiment_folder, plate, well) |>
      datatable(
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 18, scrollX = TRUE)
      )
  })

  output$input_files_table <- renderDT({
    input_files_data() |>
      datatable(
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 12, scrollX = TRUE)
      )
  })

  output$amplitude_violin_gallery <- renderUI({
    rows <- plots_data() |>
      filter(category == "amplitude_violin")
    gallery_ui(rows)
  })

  output$mfr_gallery <- renderUI({
    rows <- plots_data() |>
      filter(category == "mfr_sina")
    gallery_ui(rows)
  })

  heatmap_rows <- function(category_name) {
    plots_data() |>
      filter(category == category_name, !is_legend)
  }

  heatmap_legend_rows <- function(category_name, selected_rows) {
    legends <- plots_data() |>
      filter(category == category_name, is_legend)

    if (nrow(selected_rows) == 0 || nrow(legends) == 0) {
      return(legends)
    }

    matched <- legends |>
      filter(
        protocol %in% selected_rows$protocol,
        experiment %in% selected_rows$experiment
      )

    if (nrow(matched) > 0) {
      matched
    } else {
      legends
    }
  }

  heatmap_controls <- function(category_name, prefix) {
    rows <- heatmap_rows(category_name)
    if (nrow(rows) == 0) {
      return(empty_state("No heatmaps found."))
    }

    protocols <- sort(unique(na.omit(rows$protocol)))
    experiments <- sort(unique(na.omit(rows$experiment)))
    plates <- sort(unique(na.omit(rows$plate)))

    div(
      class = "heatmap-controls",
      selectInput(paste0(prefix, "_protocol"), "Protocol", choices = c("All", protocols)),
      selectInput(paste0(prefix, "_experiment"), "Experiment", choices = c("All", experiments)),
      selectInput(paste0(prefix, "_plate"), "Plate", choices = c("All", plates)),
      actionButton(paste0(prefix, "_update"), "Update heatmaps", icon = icon("rotate"))
    )
  }

  heatmap_selection <- function(prefix) {
    list(
      protocol = input[[paste0(prefix, "_protocol")]],
      experiment = input[[paste0(prefix, "_experiment")]],
      plate = input[[paste0(prefix, "_plate")]]
    )
  }

  heatmap_selection_has_filter <- function(selection) {
    any(vapply(selection, function(value) {
      !is.null(value) && !is.na(value) && value != "All"
    }, logical(1)))
  }

  selected_heatmap_rows <- function(category_name, selection) {
    rows <- heatmap_rows(category_name)

    protocol <- selection$protocol
    experiment <- selection$experiment
    plate <- selection$plate

    if (!is.null(protocol) && protocol != "All") {
      rows <- rows |> filter(protocol == !!protocol)
    }
    if (!is.null(experiment) && experiment != "All") {
      rows <- rows |> filter(experiment == !!experiment)
    }
    if (!is.null(plate) && plate != "All") {
      rows <- rows |> filter(plate == !!plate)
    }

    rows
  }

  output$amplitude_heatmap_controls <- renderUI({
    heatmap_controls("amplitude_heatmap", "amp_heatmap")
  })

  amplitude_heatmap_selection <- reactiveVal(NULL)

  observeEvent(input$amp_heatmap_update, {
    selection <- heatmap_selection("amp_heatmap")
    if (heatmap_selection_has_filter(selection)) {
      amplitude_heatmap_selection(selection)
    } else {
      amplitude_heatmap_selection(NULL)
    }
  }, ignoreInit = TRUE)

  output$amplitude_heatmap_view <- renderUI({
    selection <- amplitude_heatmap_selection()
    if (is.null(selection)) {
      return(empty_state("Select a protocol, experiment, or plate, then click Update heatmaps."))
    }
    if (!heatmap_selection_has_filter(selection)) {
      return(empty_state("Select a protocol, experiment, or plate, then click Update heatmaps."))
    }

    rows <- selected_heatmap_rows("amplitude_heatmap", selection)
    legends <- heatmap_legend_rows("amplitude_heatmap", rows)
    heatmap_view_ui(rows, legends)
  })

  output$mfr_heatmap_controls <- renderUI({
    heatmap_controls("mfr_heatmap", "mfr_heatmap")
  })

  mfr_heatmap_selection <- reactiveVal(NULL)

  observeEvent(input$mfr_heatmap_update, {
    selection <- heatmap_selection("mfr_heatmap")
    if (heatmap_selection_has_filter(selection)) {
      mfr_heatmap_selection(selection)
    } else {
      mfr_heatmap_selection(NULL)
    }
  }, ignoreInit = TRUE)

  output$mfr_heatmap_view <- renderUI({
    selection <- mfr_heatmap_selection()
    if (is.null(selection)) {
      return(empty_state("Select a protocol, experiment, or plate, then click Update heatmaps."))
    }
    if (!heatmap_selection_has_filter(selection)) {
      return(empty_state("Select a protocol, experiment, or plate, then click Update heatmaps."))
    }

    rows <- selected_heatmap_rows("mfr_heatmap", selection)
    legends <- heatmap_legend_rows("mfr_heatmap", rows)
    heatmap_view_ui(rows, legends)
  })

  output$raster_gallery <- renderUI({
    rows <- plots_data() |>
      filter(category == "raster")
    gallery_ui(rows)
  })

  output$isi_gallery <- renderUI({
    rows <- plots_data() |>
      filter(category == "isi")
    gallery_ui(rows)
  })

  output$report_link <- renderUI({
    report <- list.files(plot_dir, pattern = "\\.html$", full.names = FALSE)
    if (length(report) == 0) {
      return(empty_state("No rendered HTML report found."))
    }

    div(
      class = "plot-tile",
      a(
        href = paste0("analysis-output/", URLencode(report[[1]], reserved = TRUE)),
        target = "_blank",
        report[[1]]
      )
    )
  })

  output$report_zip_link <- renderUI({
    refresh_key()
    zip_path <- tryCatch(
      create_report_zip(),
      error = function(err) {
        return(empty_state(paste("Could not create report.zip:", conditionMessage(err))))
      }
    )

    if (is.na(zip_path)) {
      return(empty_state("No output files found to compress."))
    }

    zip_info <- file.info(zip_path)
    tagList(
      div(class = "section-title", "Compressed output"),
      div(
        class = "plot-tile",
        a(
          href = "analysis-output/report.zip",
          target = "_blank",
          "report.zip"
        ),
        div(
          class = "plot-meta",
          paste0(
            "Updated ", format(zip_info$mtime, "%Y-%m-%d %H:%M"), " | ",
            round(zip_info$size / 1024^2, 3), " MB"
          )
        )
      )
    )
  })

  output$analysis_log <- renderUI({
    log <- analysis_log()
    if (length(log) == 0) {
      return(NULL)
    }

    tagList(
      div(class = "section-title", "Analysis log"),
      tags$pre(paste(log, collapse = "\n"))
    )
  })
}

shinyApp(ui, server)
