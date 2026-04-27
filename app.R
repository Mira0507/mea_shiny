project_root <- normalizePath(".", mustWork = TRUE)
options(shiny.maxRequestSize = 1024^3)
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

zip_display_name <- function(upload_name) {
  basename(str_replace_all(upload_name, "\\\\", "/"))
}

input_subfolder_from_zip <- function(upload_name) {
  subfolder <- tools::file_path_sans_ext(zip_display_name(upload_name))
  subfolder <- str_trim(subfolder)
  if (!nzchar(subfolder)) {
    stop("Uploaded zip file must have a non-empty filename.")
  }
  subfolder
}

input_zip_path <- function(input_subfolder) {
  file.path(input_dir, paste0(input_subfolder, ".zip"))
}

analysis_input_folder_path <- function(input_subfolder) {
  file.path(input_dir, input_subfolder)
}

current_input_zip <- function() {
  zips <- list.files(input_dir, pattern = "\\.zip$", full.names = TRUE, recursive = FALSE)
  if (length(zips) == 0) return(NA_character_)
  zips[order(file.info(zips)$mtime, decreasing = TRUE)][[1]]
}

current_input_subfolder <- function() {
  folders <- list.dirs(input_dir, recursive = FALSE, full.names = TRUE)
  if (length(folders) == 0) return(NA_character_)
  folders[order(file.info(folders)$mtime, decreasing = TRUE)][[1]] |>
    basename()
}

validate_zip_entries <- function(zip_path) {
  entries <- utils::unzip(zip_path, list = TRUE)
  entry_names <- entries$Name
  normalized_names <- str_replace_all(entry_names, "\\\\", "/")
  path_parts <- strsplit(normalized_names, "/", fixed = TRUE)
  has_parent_dir <- vapply(path_parts, function(parts) ".." %in% parts, logical(1))
  is_absolute <- str_detect(normalized_names, "^/|^[A-Za-z]:")

  unsafe <- entry_names[has_parent_dir | is_absolute]
  if (length(unsafe) > 0) {
    stop("tables.zip contains unsafe paths: ", paste(unsafe, collapse = ", "))
  }

  entries
}

remove_macos_zip_artifacts <- function(path) {
  macos_dir <- file.path(path, "__MACOSX")
  if (dir.exists(macos_dir)) {
    unlink(macos_dir, recursive = TRUE, force = TRUE)
  }

  apple_double_files <- list.files(
    path,
    pattern = "^\\._",
    all.files = TRUE,
    recursive = TRUE,
    full.names = TRUE,
    include.dirs = TRUE,
    no.. = TRUE
  )
  if (length(apple_double_files) > 0) {
    unlink(apple_double_files, recursive = TRUE, force = TRUE)
  }
}

prepare_tables_input <- function(zip_path, input_subfolder) {
  analysis_input_folder <- analysis_input_folder_path(input_subfolder)

  if (!file.exists(zip_path)) {
    stop("Expected uploaded input zip at: ", relative_path(zip_path))
  }

  validate_zip_entries(zip_path)

  if (dir.exists(analysis_input_folder)) {
    unlink(analysis_input_folder, recursive = TRUE, force = TRUE)
  }
  dir.create(analysis_input_folder, recursive = TRUE, showWarnings = FALSE)

  utils::unzip(zip_path, exdir = analysis_input_folder)
  remove_macos_zip_artifacts(analysis_input_folder)

  metadata_files <- list.files(
    analysis_input_folder,
    pattern = "\\.xlsx$",
    recursive = FALSE,
    full.names = TRUE
  )
  plate_folders <- list.dirs(analysis_input_folder, recursive = FALSE, full.names = TRUE)
  plate_folders <- plate_folders[str_detect(str_to_lower(basename(plate_folders)), "plate\\d+")]

  if (length(metadata_files) == 0) {
    stop("tables.zip did not contain any top-level metadata .xlsx files.")
  }
  if (length(plate_folders) == 0) {
    stop("tables.zip did not contain any top-level MEA plate folders.")
  }

  invisible(analysis_input_folder)
}

reset_output_dir <- function() {
  if (dir.exists(plot_dir)) {
    unlink(plot_dir, recursive = TRUE, force = TRUE)
  }
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
  invisible(plot_dir)
}

reset_input_dir_for_upload <- function() {
  if (!dir.exists(input_dir)) {
    dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)
    return(invisible(input_dir))
  }

  entries <- list.files(input_dir, all.files = TRUE, full.names = TRUE, no.. = TRUE)
  if (length(entries) > 0) {
    unlink(entries, recursive = TRUE, force = TRUE)
  }

  invisible(input_dir)
}

accept_uploaded_tables_zip <- function(upload) {
  if (is.null(upload) || nrow(upload) == 0) {
    stop("No zip file was uploaded.")
  }
  upload_name <- zip_display_name(upload$name[[1]])
  input_subfolder <- input_subfolder_from_zip(upload_name)
  zip_path <- input_zip_path(input_subfolder)

  if (!str_detect(str_to_lower(upload_name), "\\.zip$")) {
    stop("Uploaded file must be a .zip file.")
  }

  reset_input_dir_for_upload()
  reset_output_dir()
  validate_zip_entries(upload$datapath[[1]])

  if (!file.copy(upload$datapath[[1]], zip_path, overwrite = TRUE)) {
    stop("Could not copy uploaded zip to: ", relative_path(zip_path))
  }

  prepare_tables_input(zip_path, input_subfolder)

  list(
    name = upload_name,
    input_subfolder = input_subfolder,
    zip_path = zip_path,
    size_mb = round(upload$size[[1]] / 1024^2, 2),
    metadata_rows = nrow(metadata_inventory()),
    input_files = nrow(input_file_inventory())
  )
}

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

heatmap_plate_gallery_ui <- function(rows) {
  if (nrow(rows) == 0) {
    return(empty_state("No heatmaps found."))
  }

  rows <- rows |>
    mutate(
      plate_key = if_else(is.na(plate) | plate == "", "Unlabeled plate", plate),
      plate_tab = coalesce(str_extract(plate_key, "PLATE\\d+$"), plate_key)
    ) |>
    arrange(plate_key, well, file)
  plate_tabs <- rows |>
    distinct(plate_key, plate_tab) |>
    arrange(plate_key)
  if (anyDuplicated(plate_tabs$plate_tab)) {
    plate_tabs <- plate_tabs |>
      mutate(plate_tab = plate_key)
  }

  tabs <- lapply(seq_len(nrow(plate_tabs)), function(i) {
    key <- plate_tabs$plate_key[[i]]
    label <- plate_tabs$plate_tab[[i]]
    plate_rows <- rows |>
      filter(plate_key == key) |>
      select(-plate_key, -plate_tab)

    nav_panel(
      label,
      gallery_ui(plate_rows, "heatmap-img")
    )
  })

  do.call(navset_tab, tabs)
}

heatmap_view_ui <- function(rows, legend_rows) {
  if (nrow(rows) == 0) {
    return(empty_state("No heatmaps found."))
  }

  div(
    class = "heatmap-view",
    div(
      class = "heatmap-main",
      heatmap_plate_gallery_ui(rows)
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
    "Upload",
    div(
      class = "page-wrap",
      div(class = "section-title", "Upload input zip"),
      textInput(
        "experiment_id",
        "Experiment ID",
        placeholder = "e.g. Exp6"
      ),
      fileInput(
        "tables_zip_upload",
        "Input zip",
        accept = ".zip",
        buttonLabel = "Browse",
        placeholder = "No zip selected"
      ),
      uiOutput("upload_status")
    )
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
      uiOutput("amplitude_heatmap_view")
    )
  ),
  nav_panel(
    "MFR Heatmaps",
    div(
      class = "page-wrap",
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
  upload_message <- reactiveVal(NULL)
  uploaded_input_subfolder <- reactiveVal(NULL)
  uploaded_zip_path <- reactiveVal(NULL)

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

  observeEvent(input$tables_zip_upload, {
    run_state("Preparing upload")
    analysis_log(character())
    upload_message(NULL)

    result <- tryCatch(
      {
        info <- accept_uploaded_tables_zip(input$tables_zip_upload)
        uploaded_input_subfolder(info$input_subfolder)
        uploaded_zip_path(info$zip_path)
        run_state("Input uploaded")
        list(
          ok = TRUE,
          text = paste0(
            "Uploaded ", info$name, " (", info$size_mb, " MB). ",
            "Input subfolder: ", info$input_subfolder, ". ",
            "Prepared ", info$metadata_rows, " metadata rows and ",
            info$input_files, " input files. Output was reset."
          )
        )
      },
      error = function(err) {
        run_state("Upload failed")
        list(ok = FALSE, text = paste("Upload failed:", conditionMessage(err)))
      }
    )

    upload_message(result)
    refresh_key(Sys.time())
  })

  output$upload_status <- renderUI({
    message <- upload_message()
    if (is.null(message)) {
      zip_path <- current_input_zip()
      if (!is.na(zip_path) && file.exists(zip_path)) {
        zip_info <- file.info(zip_path)
        return(div(
          class = "run-status",
          paste0(
            "Current input: ", relative_path(zip_path), " | ",
            round(zip_info$size / 1024^2, 2), " MB"
          )
        ))
      }
      return(empty_state("Upload a zip file before running analysis."))
    }

    div(
      class = if (isTRUE(message$ok)) "run-status" else "empty-state",
      message$text
    )
  })

  observeEvent(input$run_analysis, {
    req(file.exists(analysis_rmd))
    experiment_id <- input$experiment_id
    if (is.null(experiment_id)) experiment_id <- ""
    experiment_id <- str_trim(experiment_id)
    if (!nzchar(experiment_id)) {
      run_state("Failed")
      analysis_log("ERROR: Experiment ID is required.")
      return()
    }

    input_subfolder <- uploaded_input_subfolder()
    if (is.null(input_subfolder) || !nzchar(input_subfolder)) {
      input_subfolder <- current_input_subfolder()
    }
    zip_path <- uploaded_zip_path()
    if (is.null(zip_path) || !file.exists(zip_path)) {
      zip_path <- current_input_zip()
    }
    if (is.na(input_subfolder) || !nzchar(input_subfolder) || is.na(zip_path) || !file.exists(zip_path)) {
      run_state("Failed")
      analysis_log("ERROR: Upload a zip file before running analysis.")
      return()
    }

    run_state("Running")
    analysis_log(character())

    withProgress(message = "Running analysis", value = 0.2, {
      log <- tryCatch(
        {
          captured <- capture.output(
            {
              cat("Preparing fresh input from ", relative_path(zip_path), "\n")
              prepare_tables_input(zip_path, input_subfolder)
              cat("Resetting output directory\n")
              reset_output_dir()
              rmarkdown::render(
                input = analysis_rmd,
                output_dir = plot_dir,
                params = list(
                  input_subfolder = input_subfolder,
                  experiment_id = experiment_id
                ),
                envir = new.env(parent = globalenv()),
                quiet = TRUE
              )
            },
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

  all_heatmap_rows <- function(category_name) {
    heatmap_rows(category_name) |>
      arrange(protocol, experiment, plate, well, file)
  }

  output$amplitude_heatmap_view <- renderUI({
    rows <- all_heatmap_rows("amplitude_heatmap")
    legends <- heatmap_legend_rows("amplitude_heatmap", rows)
    tagList(
      div(class = "run-status", paste("Matching heatmaps:", nrow(rows), "| Plates:", n_distinct(rows$plate, na.rm = TRUE))),
      heatmap_view_ui(rows, legends)
    )
  })

  output$mfr_heatmap_view <- renderUI({
    rows <- all_heatmap_rows("mfr_heatmap")
    legends <- heatmap_legend_rows("mfr_heatmap", rows)
    tagList(
      div(class = "run-status", paste("Matching heatmaps:", nrow(rows), "| Plates:", n_distinct(rows$plate, na.rm = TRUE))),
      heatmap_view_ui(rows, legends)
    )
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
