# WASKITA Dashboard - Complete Application with LOCAL DATA ONLY
# Author: Dashboard Developer
# Date: July 2025
# IMPORTANT: Using REAL SoVI data for final project - LOCAL ONLY
# Data Source: Local directory D:\Perkuliahan Tingkat 2 Semester 4\WASKITA\data
# Metadata: https://www.sciencedirect.com/science/article/pii/S2352340921010180

# Print startup message
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("WASKITA Dashboard - Starting...\n")
cat("Wawasan Spasial Kerentanan Interaktif & Terpadu Analitik\n")
cat("LOCAL DATA VERSION - Final Project\n")
cat("Local Data Path: D:\\Perkuliahan Tingkat 2 Semester 4\\WASKITA\\data\n")
cat("Metadata: https://www.sciencedirect.com/science/article/pii/S2352340921010180\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

# Load required libraries with auto-install
required_packages <- c("shiny", "shinydashboard", "DT", "ggplot2", "dplyr", "readr", "plotly", "corrplot", "psych")

cat("Checking and installing required packages...\n")
for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Installing package:", pkg, "\n")
    install.packages(pkg, quiet = TRUE)
    library(pkg, character.only = TRUE)
  }
}
cat("✓ All packages loaded successfully!\n\n")

# Function to safely source files
safe_source <- function(file_path, required = FALSE) {
  if (file.exists(file_path)) {
    tryCatch({
      source(file_path)
      cat("✓ Loaded:", file_path, "\n")
      return(TRUE)
    }, error = function(e) {
      cat("✗ Error loading", file_path, ":", e$message, "\n")
      if (required) {
        stop(paste("Required file", file_path, "failed to load"))
      }
      return(FALSE)
    })
  } else {
    cat("✗ File not found:", file_path, "\n")
    if (required) {
      stop(paste("Required file", file_path, "not found"))
    }
    return(FALSE)
  }
}

# Track which files are loaded
files_loaded <- list()

# Load modules with fallbacks
cat("Loading modules...\n")
files_loaded$libraries <- safe_source("modules/libraries.R")
files_loaded$utils <- safe_source("modules/utils.R")
files_loaded$ui_components <- safe_source("modules/ui_components.R")
files_loaded$server_functions <- safe_source("modules/server_functions.R")
files_loaded$spatial_functions <- safe_source("modules/spatial_functions.R")

# LOCAL DATA LOADING FUNCTION ONLY
if (!files_loaded$utils || !exists("load_data")) {
  cat("Creating LOCAL ONLY data loading function...\n")
  load_data <- function() {
    
    # Define local data paths
    local_data_dir <- "D:/Perkuliahan Tingkat 2 Semester 4/WASKITA/data"
    local_sovi_path <- file.path(local_data_dir, "sovi_data.csv")
    local_distance_path <- file.path(local_data_dir, "distance.csv")
    
    cat("=== LOADING LOCAL DATA ONLY ===\n")
    cat("Local directory:", local_data_dir, "\n")
    cat("SoVI file:", local_sovi_path, "\n")
    cat("Distance file:", local_distance_path, "\n")
    
    # Check if directory exists
    if (!dir.exists(local_data_dir)) {
      cat("✗ ERROR: Local directory does not exist!\n")
      cat("Please create directory:", local_data_dir, "\n")
      return(NULL)
    }
    
    # Check if files exist
    if (!file.exists(local_sovi_path)) {
      cat("✗ ERROR: sovi_data.csv not found in local directory!\n")
      cat("Expected path:", local_sovi_path, "\n")
      return(NULL)
    }
    
    if (!file.exists(local_distance_path)) {
      cat("✗ ERROR: distance.csv not found in local directory!\n")
      cat("Expected path:", local_distance_path, "\n")
      return(NULL)
    }
    
    # Load local data
    tryCatch({
      cat("✓ Local files found! Loading from local directory...\n")
      
      # Load data with proper encoding
      sovi_data <- read_csv(local_sovi_path, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
      distance_data <- read_csv(local_distance_path, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
      
      cat("✓ LOCAL SoVI data loaded successfully!\n")
      cat("Data source: LOCAL DIRECTORY\n")
      cat("Data dimensions:", nrow(sovi_data), "rows x", ncol(sovi_data), "columns\n")
      cat("Columns found:", paste(names(sovi_data), collapse = ", "), "\n")
      
      # Process the data
      result <- process_sovi_data(sovi_data, distance_data, "LOCAL")
      return(result)
      
    }, error = function(e) {
      cat("✗ ERROR loading LOCAL data:", e$message, "\n")
      cat("Please check:\n")
      cat("1. File permissions\n")
      cat("2. File format (CSV with proper encoding)\n")
      cat("3. File corruption\n")
      return(NULL)
    })
  }
  
  # Function to process SoVI data according to metadata
  process_sovi_data <- function(sovi_data, distance_data, source_type) {
    cat("\n=== PROCESSING SOVI DATA ===\n")
    cat("Source:", source_type, "\n")
    
    # Expected SoVI variables based on metadata
    expected_sovi_vars <- c(
      "DISTRICTCODE",  # District identifier
      "CHILDREN",      # Proportion of children under 5
      "FEMALE",        # Proportion of females
      "ELDERLY",       # Proportion of elderly (65+)
      "FHEAD",         # Female-headed households
      "FAMILYSIZE",    # Average family size
      "NOELECTRIC",    # No access to electricity
      "LOWEDU",        # Low education level
      "GROWTH",        # Population growth rate
      "POVERTY",       # Poverty rate
      "ILLITERATE",    # Illiteracy rate
      "NOTRAINING",    # No vocational training
      "DPRONE",        # Disaster prone areas
      "RENTED",        # Rented housing
      "NOSEWER",       # No sewerage system
      "TAPWATER",      # Access to tap water
      "POPULATION"     # Total population
    )
    
    # Check data completeness
    found_cols <- names(sovi_data)
    missing_cols <- setdiff(expected_sovi_vars, found_cols)
    extra_cols <- setdiff(found_cols, expected_sovi_vars)
    
    cat("Expected variables:", length(expected_sovi_vars), "\n")
    cat("Found variables:", length(found_cols), "\n")
    
    if(length(missing_cols) > 0) {
      cat("⚠ Missing expected variables:", paste(missing_cols, collapse = ", "), "\n")
    } else {
      cat("✓ All expected SoVI variables found!\n")
    }
    
    if(length(extra_cols) > 0) {
      cat("ℹ Additional variables found:", paste(extra_cols, collapse = ", "), "\n")
    }
    
    # Clean and prepare the data
    sovi_data <- sovi_data %>%
      mutate(across(where(is.character), as.factor)) %>%
      mutate(across(where(is.numeric), ~as.numeric(.)))
    
    # Identify provinces from DISTRICTCODE - ALL INDONESIA PROVINCES
    if("DISTRICTCODE" %in% names(sovi_data)) {
      sovi_data <- sovi_data %>%
        mutate(
          PROVINCE_CODE = substr(as.character(DISTRICTCODE), 1, 2),
          PROVINCE = case_when(
            # Sumatera
            PROVINCE_CODE == "11" ~ "Aceh",
            PROVINCE_CODE == "12" ~ "Sumatera Utara",
            PROVINCE_CODE == "13" ~ "Sumatera Barat",
            PROVINCE_CODE == "14" ~ "Riau",
            PROVINCE_CODE == "15" ~ "Jambi",
            PROVINCE_CODE == "16" ~ "Sumatera Selatan",
            PROVINCE_CODE == "17" ~ "Bengkulu",
            PROVINCE_CODE == "18" ~ "Lampung",
            PROVINCE_CODE == "19" ~ "Kepulauan Bangka Belitung",
            PROVINCE_CODE == "21" ~ "Kepulauan Riau",
            
            # Jawa
            PROVINCE_CODE == "31" ~ "DKI Jakarta",
            PROVINCE_CODE == "32" ~ "Jawa Barat", 
            PROVINCE_CODE == "33" ~ "Jawa Tengah",
            PROVINCE_CODE == "34" ~ "DI Yogyakarta",
            PROVINCE_CODE == "35" ~ "Jawa Timur",
            PROVINCE_CODE == "36" ~ "Banten",
            
            # Bali & Nusa Tenggara
            PROVINCE_CODE == "51" ~ "Bali",
            PROVINCE_CODE == "52" ~ "Nusa Tenggara Barat",
            PROVINCE_CODE == "53" ~ "Nusa Tenggara Timur",
            
            # Kalimantan
            PROVINCE_CODE == "61" ~ "Kalimantan Barat",
            PROVINCE_CODE == "62" ~ "Kalimantan Tengah",
            PROVINCE_CODE == "63" ~ "Kalimantan Selatan",
            PROVINCE_CODE == "64" ~ "Kalimantan Timur",
            PROVINCE_CODE == "65" ~ "Kalimantan Utara",
            
            # Sulawesi
            PROVINCE_CODE == "71" ~ "Sulawesi Utara",
            PROVINCE_CODE == "72" ~ "Sulawesi Tengah",
            PROVINCE_CODE == "73" ~ "Sulawesi Selatan",
            PROVINCE_CODE == "74" ~ "Sulawesi Tenggara",
            PROVINCE_CODE == "75" ~ "Gorontalo",
            PROVINCE_CODE == "76" ~ "Sulawesi Barat",
            
            # Maluku
            PROVINCE_CODE == "81" ~ "Maluku",
            PROVINCE_CODE == "82" ~ "Maluku Utara",
            
            # Papua
            PROVINCE_CODE == "91" ~ "Papua Barat",
            PROVINCE_CODE == "94" ~ "Papua",
            
            TRUE ~ paste("Province", PROVINCE_CODE)
          ),
          REGION = case_when(
            PROVINCE_CODE %in% c("11", "12", "13", "14", "15", "16", "17", "18", "19", "21") ~ "Sumatera",
            PROVINCE_CODE %in% c("31", "32", "33", "34", "35", "36") ~ "Jawa",
            PROVINCE_CODE %in% c("51", "52", "53") ~ "Bali & Nusa Tenggara",
            PROVINCE_CODE %in% c("61", "62", "63", "64", "65") ~ "Kalimantan",
            PROVINCE_CODE %in% c("71", "72", "73", "74", "75", "76") ~ "Sulawesi",
            PROVINCE_CODE %in% c("81", "82") ~ "Maluku",
            PROVINCE_CODE %in% c("91", "94") ~ "Papua",
            TRUE ~ "Lainnya"
          )
        )
    }
    
    # Handle distance matrix
    if (ncol(distance_data) > 1) {
      if (!is.numeric(distance_data[[1]])) {
        rownames(distance_data) <- distance_data[[1]]
        distance_data <- distance_data[, -1]
      }
      distance_data <- as.data.frame(lapply(distance_data, as.numeric))
      cat("✓ Distance matrix processed:", nrow(distance_data), "x", ncol(distance_data), "\n")
    }
    
    # Add metadata information
    sovi_data$DATA_SOURCE <- source_type
    sovi_data$METADATA_URL <- "https://www.sciencedirect.com/science/article/pii/S2352340921010180"
    sovi_data$LOAD_TIMESTAMP <- Sys.time()
    
    cat("✓ Data preprocessing completed successfully!\n")
    cat("Final dataset dimensions:", nrow(sovi_data), "rows x", ncol(sovi_data), "columns\n")
    
    return(list(
      sovi = sovi_data, 
      distance = distance_data,
      source = source_type,
      metadata_url = "https://www.sciencedirect.com/science/article/pii/S2352340921010180"
    ))
  }
}

# Enhanced CSS with the specified color palette and improved design
custom_css <- "
  @import url('https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;500;600;700&family=Inter:wght@300;400;500;600&display=swap');
  
  /* Color Palette - Navy, Steel Blue, Warm Gray, Cream */
  :root {
    --navy: #1B3C53;
    --steel-blue: #456882;
    --warm-gray: #D2C1B6;
    --cream: #F9F3EF;
    --white: #ffffff;
    --text-dark: #1B3C53;
    --text-muted: #64748b;
    --border-light: #e2e8f0;
    --shadow-soft: 0 4px 6px -1px rgba(27, 60, 83, 0.1);
    --shadow-medium: 0 10px 15px -3px rgba(27, 60, 83, 0.1);
    --shadow-large: 0 20px 25px -5px rgba(27, 60, 83, 0.1);
  }

  /* Global Styles */
  body, .content-wrapper, .right-side {
    background: linear-gradient(135deg, #f8fafc 0%, #f1f5f9 100%) !important;
    font-family: 'Inter', sans-serif;
    color: var(--text-dark);
  }

  h1, h2, h3, h4, h5, h6 {
    font-family: 'Poppins', sans-serif;
    font-weight: 600;
    color: var(--navy);
  }

  /* Header & Sidebar */
  .main-header .navbar {
    background: linear-gradient(135deg, var(--navy) 0%, var(--steel-blue) 100%) !important;
    border: none !important;
    box-shadow: var(--shadow-soft);
  }

  .main-header .logo {
    background-color: var(--navy) !important;
    color: white !important;
    font-family: 'Poppins', sans-serif;
    font-weight: 700;
  }

  .main-sidebar {
    background-color: var(--navy) !important;
  }

  .sidebar-menu > li > a {
    color: rgba(255, 255, 255, 0.8) !important;
    font-family: 'Inter', sans-serif;
  }

  .sidebar-menu > li:hover > a,
  .sidebar-menu > li.active > a {
    background-color: var(--steel-blue) !important;
    color: white !important;
  }

 /* START: PENAMBAHAN CSS UNTUK HALAMAN MANAJEMEN DATA */
  .data-management-page .box {
      border-radius: 12px !important; /* Membuat box sedikit melengkung */
      border-top: 3px solid var(--steel-blue) !important;
      box-shadow: var(--shadow-soft) !important;
      border-color: #e2e8f0 !important;
  }
  
  .data-management-page .box.box-primary {
      border-top-color: var(--navy) !important;
  }
  
  .data-management-page .box.box-solid.box-primary > .box-header {
      background: var(--navy) !important;
      border-radius: 12px 12px 0 0 !important;
  }
  
  .data-management-page .box .box-header .box-title {
      color: var(--navy) !important; /* Warna judul box */
      font-family: 'Poppins', sans-serif !important;
      font-weight: 600 !important;
  }
  
  .data-management-page .box .box-body {
      background-color: var(--white) !important; /* Latar belakang isi box */
  }

  .data-management-page .nav-tabs-custom {
      border-radius: 12px !important;
      box-shadow: none !important;
  }

  .data-management-page .nav-tabs-custom > .nav-tabs > li.active {
      border-top-color: var(--navy) !important;
      border-bottom-color: transparent !important;
  }
  
  .data-management-page .nav-tabs-custom > .nav-tabs > li > a {
      color: var(--text-dark) !important;
      font-weight: 500 !important;
  }
  
  .data-management-page .nav-tabs-custom > .nav-tabs > li.active > a,
  .data-management-page .nav-tabs-custom > .nav-tabs > li.active:hover > a {
      background-color: var(--white) !important;
      color: var(--navy) !important;
  }

  /* Tombol Transformasi dan Kategorisasi dengan Warna Lebih Cerah */
  .data-management-page .btn-transform {
      background-color: #007bff !important;
      border-color: #007bff !important;
      color: white !important;
      font-weight: 500 !important;
      border-radius: 6px !important;
  }
  
  .data-management-page .btn-transform:hover {
      background-color: #0056b3 !important;
      border-color: #0056b3 !important;
      color: white !important;
      transform: translateY(-1px) !important;
      box-shadow: 0 4px 8px rgba(0, 123, 255, 0.3) !important;
  }
  
  .data-management-page .btn-categorize {
      background-color: #28a745 !important;
      border-color: #28a745 !important;
      color: white !important;
      font-weight: 500 !important;
      border-radius: 6px !important;
  }
  
  .data-management-page .btn-categorize:hover {
      background-color: #1e7e34 !important;
      border-color: #1e7e34 !important;
      color: white !important;
      transform: translateY(-1px) !important;
      box-shadow: 0 4px 8px rgba(40, 167, 69, 0.3) !important;
  }

  /* Logging Box Styling */
  .log-box {
    background-color: var(--cream) !important;
    border: 1px solid var(--warm-gray) !important;
    border-radius: 8px !important;
    padding: 15px !important;
    max-height: 200px !important;
    overflow-y: auto !important;
    color: var(--text-dark) !important;
    font-family: 'Courier New', Courier, monospace !important;
    font-size: 0.9em !important;
    line-height: 1.5 !important;
  }
  
  .log-box p {
    margin: 0 !important;
    padding: 0 !important;
  }

  /* Tombol Transformasi dan Kategorisasi dengan Warna Lebih Cerah */
  .data-management-page .btn-transform {
      background-color: #007bff !important;
      border-color: #007bff !important;
      color: white !important;
      font-weight: 500 !important;
      border-radius: 6px !important;
  }
  
  .data-management-page .btn-transform:hover {
      background-color: #0056b3 !important;
      border-color: #0056b3 !important;
      color: white !important;
      transform: translateY(-1px) !important;
      box-shadow: 0 4px 8px rgba(0, 123, 255, 0.3) !important;
  }
  
  .data-management-page .btn-categorize {
      background-color: #28a745 !important;
      border-color: #28a745 !important;
      color: white !important;
      font-weight: 500 !important;
      border-radius: 6px !important;
  }
  
  .data-management-page .btn-categorize:hover {
      background-color: #1e7e34 !important;
      border-color: #1e7e34 !important;
      color: white !important;
      transform: translateY(-1px) !important;
      box-shadow: 0 4px 8px rgba(40, 167, 69, 0.3) !important;
  }
  /* END: PENAMBAHAN CSS */

  /* Enhanced Hero Section with Modified Badge Layout */
  .hero-section {
    background: linear-gradient(135deg, var(--navy) 0%, var(--steel-blue) 100%);
    color: white;
    padding: 20px 40px;
    border-radius: 32px;
    margin-bottom: 60px;
    text-align: center;
    position: relative;
    overflow: hidden;
    box-shadow: var(--shadow-large);
  }

  .hero-section::before {
    content: '';
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    background: radial-gradient(circle at 30% 20%, rgba(255,255,255,0.1) 0%, transparent 50%),
                radial-gradient(circle at 70% 80%, rgba(255,255,255,0.08) 0%, transparent 50%);
    opacity: 0.6;
  }

  .hero-content {
    position: relative;
    z-index: 2;
  }

  /* Modified Badge Layout - Split Top */
  .hero-badges {
    display: flex;
    justify-content: space-between;
    align-items: flex-start;
    margin-bottom: 30px;
    position: relative;
    z-index: 3;
  }

  .hero-badge-left {
    display: inline-flex;
    align-items: center;
    gap: 8px;
    background: rgba(255, 255, 255, 0.15);
    backdrop-filter: blur(10px);
    border: 1px solid rgba(255, 255, 255, 0.2);
    padding: 12px 24px;
    border-radius: 50px;
    font-size: 0.95em;
    font-weight: 500;
    transition: all 0.3s ease;
    color: white;
  }

  .hero-badge-right {
    display: inline-flex;
    align-items: center;
    gap: 8px;
    background: rgba(34, 197, 94, 0.15);
    backdrop-filter: blur(10px);
    border: 1px solid rgba(34, 197, 94, 0.3);
    color: rgb(34, 197, 94);
    padding: 12px 24px;
    border-radius: 50px;
    font-size: 0.95em;
    font-weight: 500;
    transition: all 0.3s ease;
  }

  .hero-badge-right.error {
    background: rgba(239, 68, 68, 0.15);
    border-color: rgba(239, 68, 68, 0.3);
    color: rgb(239, 68, 68);
  }

  .hero-badge-left:hover,
  .hero-badge-right:hover {
    transform: translateY(-2px);
  }

  .hero-badge-left:hover {
    background: rgba(255, 255, 255, 0.2);
  }

  .hero-badge-right:hover {
    background: rgba(34, 197, 94, 0.2);
  }

  .hero-title {
    font-family: 'Poppins', sans-serif;
    font-weight: 700;
    font-size: 4.5em;
    margin-bottom: 20px;
    letter-spacing: -0.02em;
    text-shadow: 0 4px 8px rgba(0,0,0,0.1);
    color: white !important;
  }

  .hero-subtitle {
    font-size: 1.4em;
    font-weight: 400;
    opacity: 0.95;
    margin-bottom: 20px;
    color: white !important;
  }

  .hero-description {
    font-size: 1.15em;
    opacity: 0.85;
    line-height: 1.7;
    max-width: 700px;
    margin: 0 auto 40px auto;
    color: white !important;
  }

  .hero-stats {
    display: flex;
    justify-content: center;
    gap: 60px;
    margin-top: 40px;
  }

  .hero-stat {
    text-align: center;
  }

  .hero-stat-number {
    font-family: 'Poppins', sans-serif;
    font-weight: 700;
    font-size: 2.5em;
    display: block;
    margin-bottom: 8px;
    color: white !important;
  }

  .hero-stat-label {
    font-size: 0.9em;
    opacity: 0.8;
    text-transform: uppercase;
    letter-spacing: 1px;
    color: white !important;
  }

  /* Data Source Badge - Removed from original location */
  .data-source-badge {
    display: inline-flex;
    align-items: center;
    gap: 8px;
    background: rgba(34, 197, 94, 0.15);
    border: 1px solid rgba(34, 197, 94, 0.3);
    color: rgb(34, 197, 94);
    padding: 8px 16px;
    border-radius: 20px;
    font-size: 0.85em;
    font-weight: 500;
    margin-bottom: 20px;
  }

  .data-source-badge.error {
    background: rgba(239, 68, 68, 0.15);
    border-color: rgba(239, 68, 68, 0.3);
    color: rgb(239, 68, 68);
  }

  /* Quick Actions Section */
  .quick-actions {
    margin-bottom: 80px;
  }

  .actions-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
    gap: 30px;
    margin-top: 40px;
  }

  .action-card {
    background: var(--white);
    border-radius: 20px;
    padding: 30px 25px;
    text-align: center;
    box-shadow: var(--shadow-soft);
    border: 1px solid var(--border-light);
    transition: all 0.3s ease;
    cursor: pointer;
    position: relative;
  }

  .action-card:hover {
    transform: translateY(-8px);
    box-shadow: var(--shadow-medium);
    border-color: var(--steel-blue);
  }

  .action-icon {
    width: 60px;
    height: 60px;
    border-radius: 16px;
    display: flex;
    align-items: center;
    justify-content: center;
    font-size: 1.8em;
    margin: 0 auto 20px auto;
    color: white;
    background: linear-gradient(135deg, var(--steel-blue) 0%, var(--warm-gray) 100%);
  }

  .action-title {
    font-family: 'Poppins', sans-serif;
    font-weight: 600;
    font-size: 1.2em;
    color: var(--navy);
    margin-bottom: 12px;
  }

  .action-description {
    color: var(--text-muted);
    font-size: 0.95em;
    line-height: 1.5;
  }

  /* Enhanced Statistics Section */
  .stats-section {
    margin-bottom: 80px;
  }

  .stats-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(320px, 1fr));
    gap: 40px;
    margin-top: 50px;
  }

  .enhanced-infobox {
    background: var(--white);
    border-radius: 24px;
    padding: 40px 30px;
    text-align: center;
    box-shadow: var(--shadow-soft);
    border: 1px solid var(--border-light);
    transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1);
    position: relative;
    overflow: hidden;
  }

  .enhanced-infobox::before {
    content: '';
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    height: 6px;
    border-radius: 24px 24px 0 0;
  }

  .enhanced-infobox.navy::before {
    background: linear-gradient(90deg, var(--navy) 0%, var(--steel-blue) 100%);
  }

  .enhanced-infobox.steel::before {
    background: linear-gradient(90deg, var(--steel-blue) 0%, var(--warm-gray) 100%);
  }

  .enhanced-infobox.warm::before {
    background: linear-gradient(90deg, var(--warm-gray) 0%, var(--cream) 100%);
  }

  .enhanced-infobox:hover {
    transform: translateY(-12px);
    box-shadow: var(--shadow-large);
  }

  .infobox-icon {
    width: 80px;
    height: 80px;
    border-radius: 24px;
    display: flex;
    align-items: center;
    justify-content: center;
    font-size: 2.5em;
    margin: 0 auto 25px auto;
    color: white;
    position: relative;
    transition: all 0.3s ease;
  }

  .enhanced-infobox:hover .infobox-icon {
    transform: scale(1.1);
  }

  .enhanced-infobox.navy .infobox-icon {
    background: linear-gradient(135deg, var(--navy) 0%, var(--steel-blue) 100%);
  }

  .enhanced-infobox.steel .infobox-icon {
    background: linear-gradient(135deg, var(--steel-blue) 0%, var(--warm-gray) 100%);
  }

  .enhanced-infobox.warm .infobox-icon {
    background: linear-gradient(135deg, var(--warm-gray) 0%, var(--cream) 100%);
    color: var(--navy);
  }

  .infobox-number {
    font-family: 'Poppins', sans-serif;
    font-weight: 700;
    font-size: 3.5em;
    color: var(--navy);
    margin-bottom: 12px;
    line-height: 1;
  }

  .infobox-label {
    font-weight: 600;
    color: var(--text-muted);
    font-size: 1.1em;
    text-transform: uppercase;
    letter-spacing: 0.5px;
    margin-bottom: 15px;
  }

  .infobox-description {
    font-size: 1em;
    color: var(--text-muted);
    opacity: 0.8;
    line-height: 1.5;
  }

  /* Dashboard Metadata Section */
  .metadata-section {
    margin-bottom: 80px;
  }

  .metadata-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
    gap: 30px;
    margin-top: 40px;
  }

  .metadata-card {
    background: var(--white);
    border-radius: 20px;
    padding: 30px;
    box-shadow: var(--shadow-soft);
    border: 1px solid var(--border-light);
    transition: all 0.3s ease;
  }

  .metadata-card:hover {
    transform: translateY(-5px);
    box-shadow: var(--shadow-medium);
  }

  .metadata-header {
    display: flex;
    align-items: center;
    gap: 15px;
    margin-bottom: 20px;
  }

  .metadata-icon {
    width: 50px;
    height: 50px;
    border-radius: 12px;
    display: flex;
    align-items: center;
    justify-content: center;
    font-size: 1.5em;
    background: linear-gradient(135deg, var(--navy) 0%, var(--steel-blue) 100%);
    color: white;
  }

  .metadata-title {
    font-weight: 600;
    color: var(--navy);
    font-size: 1.2em;
  }

  .metadata-content {
    color: var(--text-muted);
    line-height: 1.6;
  }

  .metadata-list {
    list-style: none;
    padding: 0;
    margin: 0;
  }

  .metadata-list li {
    padding: 8px 0;
    border-bottom: 1px solid var(--border-light);
    display: flex;
    justify-content: space-between;
    align-items: center;
  }

  .metadata-list li:last-child {
    border-bottom: none;
  }

  .metadata-label {
    font-weight: 500;
    color: var(--text-dark);
  }

  .metadata-value {
    color: var(--text-muted);
    font-size: 0.95em;
  }

  /* Section Headers */
  .section-header {
    text-align: center;
    margin-bottom: 50px;
  }

  .section-title {
    font-family: 'Poppins', sans-serif;
    font-weight: 600;
    font-size: 2.5em;
    color: var(--navy);
    margin-bottom: 15px;
    display: flex;
    align-items: center;
    justify-content: center;
    gap: 20px;
  }

  .section-subtitle {
    font-size: 1.2em;
    color: var(--text-muted);
    max-width: 700px;
    margin: 0 auto;
    line-height: 1.7;
  }

  /* Footer */
  .dashboard-footer {
    background: linear-gradient(135deg, var(--navy) 0%, var(--steel-blue) 100%);
    color: white;
    margin: 60px -15px -15px -15px;
    padding: 40px 30px 25px 30px;
    border-radius: 0;
  }

  .footer-content {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
    gap: 35px;
    max-width: 1200px;
    margin: 0 auto;
  }

  .footer-section h4 {
    font-family: 'Poppins', sans-serif;
    font-weight: 600;
    margin-bottom: 20px;
    color: var(--cream);
    display: flex;
    align-items: center;
    gap: 8px;
  }

  .footer-section p,
  .footer-section li {
    color: rgba(255, 255, 255, 0.85);
    line-height: 1.6;
    font-size: 0.95em;
  }

  .footer-section ul {
    list-style: none;
    padding: 0;
  }

  .footer-section li {
    margin-bottom: 8px;
    padding-left: 20px;
    position: relative;
  }

  .footer-section li:before {
    content: '▸';
    color: var(--warm-gray);
    position: absolute;
    left: 0;
    font-weight: bold;
  }

  .footer-bottom {
    margin-top: 30px;
    padding-top: 20px;
    border-top: 1px solid rgba(255, 255, 255, 0.15);
    text-align: center;
    font-size: 0.9em;
    opacity: 0.8;
  }

  /* Error message styling */
  .error-message {
    background: #fee;
    border: 1px solid #fcc;
    border-radius: 8px;
    padding: 20px;
    margin: 20px;
    color: #c33;
    text-align: center;
  }

  /* Legacy styles for backward compatibility */
  .info-card {
    background: white;
    border-radius: 8px;
    padding: 20px;
    margin: 10px;
    box-shadow: 0 2px 8px rgba(0,0,0,0.1);
    border-left: 4px solid #1B3C53;
  }

  .metric-box {
    text-align: center;
    padding: 20px;
    background: white;
    border-radius: 8px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    margin: 10px;
  }

  .btn-primary {
    background-color: var(--navy);
    border-color: var(--navy);
  }

  /* Responsive Design */
  @media (max-width: 768px) {
    .hero-title {
      font-size: 3em;
    }
    
    .hero-stats {
      flex-direction: column;
      gap: 30px;
    }
    
    .hero-badges {
      flex-direction: column;
      align-items: center;
      gap: 15px;
    }
    
    .stats-grid,
    .actions-grid,
    .metadata-grid {
      grid-template-columns: 1fr;
      gap: 25px;
    }
    
    .hero-section {
      padding: 50px 25px;
    }
  }
"

# Enhanced UI components with fallbacks
if (!files_loaded$ui_components || !exists("custom_css")) {
  cat("Creating enhanced UI components with specified color palette...\n")
  
  create_info_card <- function(title, content) {
    div(class = "info-card",
        h4(title, style = "color: #1B3C53;"),
        content
    )
  }
  
  create_interpretation_box <- function(title, content) {
    div(style = "background-color: #F0F8FF; padding: 15px; border-radius: 5px; border-left: 4px solid #1B3C53; margin-top: 15px;",
        h5(title, style = "color: #1B3C53; font-weight: 600; margin-bottom: 10px;"),
        content
    )
  }
}

# Load UI modules with fallbacks
cat("Loading UI modules...\n")
files_loaded$ui_beranda <- safe_source("ui/ui_beranda.R")
files_loaded$ui_data_management <- safe_source("ui/ui_data_management.R")
files_loaded$ui_exploration <- safe_source("ui/ui_exploration.R")
files_loaded$ui_assumptions <- safe_source("ui/ui_assumptions.R")
files_loaded$ui_inference <- safe_source("ui/ui_inference.R")
files_loaded$ui_regression <- safe_source("ui/ui_regression.R")
files_loaded$ui_spatial <- safe_source("ui/ui_spatial.R")

# Enhanced Beranda Tab with MODIFIED LAYOUT - UAS LEFT, DATA LOCAL RIGHT
beranda_tab <- tabItem(tabName = "beranda",
                       # Hero Section with modified badge layout
                       div(class = "hero-section",
                           div(class = "hero-content",
                               # Modified Badges Layout - Split Top Left and Right
                               div(class = "hero-badges",
                                   # UAS Badge - LEFT SIDE
                                   div(class = "hero-badge-left",
                                       tags$i(class = "fas fa-graduation-cap"),
                                       " UAS Komputasi Statistik 2025"
                                   ),
                                   # Data Source Badge - RIGHT SIDE
                                   div(
                                     uiOutput("data_source_badge_top")
                                   )
                               ),
                               h1(class = "hero-title", "WASKITA"),
                               h3(class = "hero-subtitle", "Wawasan Spasial Kerentanan Interaktif & Terpadu Analitik"),
                               p(class = "hero-description",
                                 "Platform analisis modern untuk penelitian kerentanan sosial dengan tools statistik komprehensif, visualisasi interaktif, dan analisis spasial yang terintegrasi menggunakan data Social Vulnerability Index dari penelitian ilmiah."
                               ),
                           )
                       ),
                       
                       # Quick Actions Section
                       div(class = "quick-actions",
                           div(class = "section-header",
                               h2(class = "section-title",
                                  tags$i(class = "fas fa-rocket"),
                                  "Menu Analisis"
                               ),
                               p(class = "section-subtitle",
                                 "Pilihan menu analisis yang bisa Anda gunakan untuk memulai eksplorasi data kerentanan sosial berdasarkan penelitian ilmiah"
                               )
                           ),
                           div(class = "actions-grid",
                               div(class = "action-card", onclick = "document.getElementById('sidebar').querySelector('[data-value=\"data_management\"]').click();",
                                   div(class = "action-icon", tags$i(class = "fas fa-database")),
                                   h4(class = "action-title", "Kelola Data"),
                                   p(class = "action-description", "Preprocessing dan transformasi dataset")
                               ),
                               div(class = "action-card", onclick = "document.getElementById('sidebar').querySelector('[data-value=\"descriptive\"]').click();",
                                   div(class = "action-icon", tags$i(class = "fas fa-chart-bar")),
                                   h4(class = "action-title", "Eksplorasi"),
                                   p(class = "action-description", "Analisis deskriptif dan visualisasi data")
                               ),
                               div(class = "action-card", onclick = "document.getElementById('sidebar').querySelector('[data-value=\"normality\"]').click();",
                                   div(class = "action-icon", tags$i(class = "fas fa-check-circle")),
                                   h4(class = "action-title", "Uji Asumsi"),
                                   p(class = "action-description", "Validasi normalitas dan homogenitas")
                               ),
                               div(class = "action-card", onclick = "document.getElementById('sidebar').querySelector('[data-value=\"mean_test\"]').click();",
                                   div(class = "action-icon", tags$i(class = "fas fa-calculator")),
                                   h4(class = "action-title", "Inferensia"),
                                   p(class = "action-description", "Uji hipotesis dan estimasi")
                               ),
                               div(class = "action-card", onclick = "document.getElementById('sidebar').querySelector('[data-value=\"regression\"]').click();",
                                   div(class = "action-icon", tags$i(class = "fas fa-line-chart")),
                                   h4(class = "action-title", "Regresi"),
                                   p(class = "action-description", "Analisis hubungan antar variabel")
                               ),
                               div(class = "action-card", onclick = "document.getElementById('sidebar').querySelector('[data-value=\"spatial\"]').click();",
                                   div(class = "action-icon", tags$i(class = "fas fa-globe")),
                                   h4(class = "action-title", "Spasial"),
                                   p(class = "action-description", "Analisis geografis dan autokorelasi")
                               )
                           )
                       ),
                       
                       # Dataset Overview with Enhanced InfoBoxes - REAL DATA
                       div(class = "stats-section",
                           div(class = "section-header",
                               h2(class = "section-title",
                                  tags$i(class = "fas fa-chart-line"),
                                  "Overview Dataset"
                               ),
                               p(class = "section-subtitle",
                                 "Ringkasan komprehensif dari dataset Social Vulnerability Index yang digunakan untuk analisis berdasarkan penelitian ilmiah"
                               )
                           ),
                           div(class = "stats-grid",
                               div(class = "enhanced-infobox navy",
                                   div(class = "infobox-icon", tags$i(class = "fas fa-table")),
                                   div(class = "infobox-number", textOutput("total_obs")),
                                   div(class = "infobox-label", "Total Observasi"),
                                   div(class = "infobox-description", "Kabupaten/Kota dalam dataset")
                               ),
                               div(class = "enhanced-infobox steel",
                                   div(class = "infobox-icon", tags$i(class = "fas fa-columns")),
                                   div(class = "infobox-number", textOutput("total_vars")),
                                   div(class = "infobox-label", "Indikator SoVI"),
                                   div(class = "infobox-description", "Variabel kerentanan sosial")
                               ),
                               div(class = "enhanced-infobox warm",
                                   div(class = "infobox-icon", tags$i(class = "fas fa-map-marked-alt")),
                                   div(class = "infobox-number", textOutput("total_provinces")),
                                   div(class = "infobox-label", "Provinsi Indonesia"),
                                   div(class = "infobox-description", "Cakupan wilayah dalam data")
                               )
                           )
                       ),
                       
                       # Scientific Metadata Section
                       div(class = "metadata-section",
                           div(class = "section-header",
                               h2(class = "section-title",
                                  tags$i(class = "fas fa-microscope"),
                                  "Metadata Ilmiah"
                               ),
                               p(class = "section-subtitle",
                                 "Informasi lengkap mengenai dataset Social Vulnerability Index berdasarkan publikasi ilmiah di ScienceDirect"
                               )
                           ),
                           div(class = "metadata-grid",
                               div(class = "metadata-card",
                                   div(class = "metadata-header",
                                       div(class = "metadata-icon", tags$i(class = "fas fa-book-open")),
                                       h4(class = "metadata-title", "Publikasi Ilmiah")
                                   ),
                                   div(class = "metadata-content",
                                       tags$ul(class = "metadata-list",
                                               tags$li(
                                                 span(class = "metadata-label", "Journal:"),
                                                 span(class = "metadata-value", "Data in Brief")
                                               ),
                                               tags$li(
                                                 span(class = "metadata-label", "Publisher:"),
                                                 span(class = "metadata-value", "Elsevier")
                                               ),
                                               tags$li(
                                                 span(class = "metadata-label", "DOI:"),
                                                 span(class = "metadata-value", "10.1016/j.dib.2021.107618")
                                               ),
                                               tags$li(
                                                 span(class = "metadata-label", "URL:"),
                                                 tags$a(href = "https://www.sciencedirect.com/science/article/pii/S2352340921010180", 
                                                        target = "_blank", class = "metadata-value", "ScienceDirect Link")
                                               )
                                       )
                                   )
                               ),
                               div(class = "metadata-card",
                                   div(class = "metadata-header",
                                       div(class = "metadata-icon", tags$i(class = "fas fa-users")),
                                       h4(class = "metadata-title", "Indikator Demografis")
                                   ),
                                   div(class = "metadata-content",
                                       tags$ul(class = "metadata-list",
                                               tags$li(
                                                 span(class = "metadata-label", "CHILDREN:"),
                                                 span(class = "metadata-value", "Proporsi Anak < 5 tahun")
                                               ),
                                               tags$li(
                                                 span(class = "metadata-label", "FEMALE:"),
                                                 span(class = "metadata-value", "Proporsi Perempuan")
                                               ),
                                               tags$li(
                                                 span(class = "metadata-label", "ELDERLY:"),
                                                 span(class = "metadata-value", "Proporsi Lansia 65+")
                                               ),
                                               tags$li(
                                                 span(class = "metadata-label", "FHEAD:"),
                                                 span(class = "metadata-value", "KRT Perempuan")
                                               ),
                                               tags$li(
                                                 span(class = "metadata-label", "FAMILYSIZE:"),
                                                 span(class = "metadata-value", "Rata-rata Ukuran Keluarga")
                                               )
                                       )
                                   )
                               ),
                               div(class = "metadata-card",
                                   div(class = "metadata-header",
                                       div(class = "metadata-icon", tags$i(class = "fas fa-graduation-cap")),
                                       h4(class = "metadata-title", "Indikator Sosial-Ekonomi")
                                   ),
                                   div(class = "metadata-content",
                                       tags$ul(class = "metadata-list",
                                               tags$li(
                                                 span(class = "metadata-label", "LOWEDU:"),
                                                 span(class = "metadata-value", "Pendidikan Rendah")
                                               ),
                                               tags$li(
                                                 span(class = "metadata-label", "POVERTY:"),
                                                 span(class = "metadata-value", "Tingkat Kemiskinan")
                                               ),
                                               tags$li(
                                                 span(class = "metadata-label", "ILLITERATE:"),
                                                 span(class = "metadata-value", "Tingkat Buta Huruf")
                                               ),
                                               tags$li(
                                                 span(class = "metadata-label", "NOTRAINING:"),
                                                 span(class = "metadata-value", "Tanpa Pelatihan Vokasi")
                                               ),
                                               tags$li(
                                                 span(class = "metadata-label", "GROWTH:"),
                                                 span(class = "metadata-value", "Tingkat Pertumbuhan")
                                               )
                                       )
                                   )
                               ),
                               div(class = "metadata-card",
                                   div(class = "metadata-header",
                                       div(class = "metadata-icon", tags$i(class = "fas fa-home")),
                                       h4(class = "metadata-title", "Indikator Infrastruktur")
                                   ),
                                   div(class = "metadata-content",
                                       tags$ul(class = "metadata-list",
                                               tags$li(
                                                 span(class = "metadata-label", "NOELECTRIC:"),
                                                 span(class = "metadata-value", "Tanpa Akses Listrik")
                                               ),
                                               tags$li(
                                                 span(class = "metadata-label", "RENTED:"),
                                                 span(class = "metadata-value", "Rumah Sewa/Kontrak")
                                               ),
                                               tags$li(
                                                 span(class = "metadata-label", "NOSEWER:"),
                                                 span(class = "metadata-value", "Tanpa Sistem Sanitasi")
                                               ),
                                               tags$li(
                                                 span(class = "metadata-label", "TAPWATER:"),
                                                 span(class = "metadata-value", "Akses Air Bersih")
                                               ),
                                               tags$li(
                                                 span(class = "metadata-label", "DPRONE:"),
                                                 span(class = "metadata-value", "Rawan Bencana Alam")
                                               )
                                       )
                                   )
                               )
                           )
                       ),
                       
                       # Enhanced Footer with metadata reference
                       div(class = "dashboard-footer",
                           div(class = "footer-content",
                               div(class = "footer-section",
                                   h4(tags$i(class = "fas fa-university"), " Informasi Akademis"),
                                   p("Politeknik Statistika STIS"),
                                   p("Program Studi Komputasi Statistik D-IV"),
                                   p("Mata Kuliah: Komputasi Statistik"),
                                   p("Semester Genap TA. 2024/2025")
                               ),
                               div(class = "footer-section",
                                   h4(tags$i(class = "fas fa-calendar-alt"), " Jadwal UAS"),
                                   tags$ul(
                                     tags$li("Tanggal: 23 Juli 2025"),
                                     tags$li("Waktu: 10:30 - 12:30 WIB"),
                                     tags$li("Durasi: 120 menit"),
                                     tags$li("Sifat: Tidak Terstruktur")
                                   )
                               ),
                               div(class = "footer-section",
                                   h4(tags$i(class = "fas fa-database"), " Data Source"),
                                   tags$ul(
                                     tags$li("Dataset: Social Vulnerability Index"),
                                     tags$li("Local: D:/Perkuliahan.../WASKITA/data"),
                                     tags$li("Format: CSV Files"),
                                     tags$li("Metadata: ScienceDirect Publication")
                                   )
                               ),
                               div(class = "footer-section",
                                   h4(tags$i(class = "fas fa-book"), " Referensi Ilmiah"),
                                   tags$ul(
                                     tags$li("Journal: Data in Brief"),
                                     tags$li("Publisher: Elsevier"),
                                     tags$li("DOI: 10.1016/j.dib.2021.107618"),
                                     tags$li(tags$a(href = "https://www.sciencedirect.com/science/article/pii/S2352340921010180", 
                                                    target = "_blank", "View Publication"))
                                   )
                               )
                           ),
                           div(class = "footer-bottom",
                               "© 2025 Dashboard WASKITA - Final Project using SoVI Data with Scientific Metadata"
                           )
                       )
)

# Create other fallback tabs (keeping original structure)
if (!exists("data_management_tab")) {
  data_management_tab <- tabItem(tabName = "data_management",
                                 h2("Manajemen Data"),
                                 box(title = "Preview Data", status = "primary", solidHeader = TRUE, width = NULL,
                                     DT::dataTableOutput("data_preview_basic"),
                                     hr(),
                                     p("Menampilkan data Social Vulnerability Index yang sesungguhnya dari penelitian ilmiah.")
                                 ),
                                 box(title = "Informasi Dataset", status = "info", solidHeader = TRUE, width = NULL,
                                     verbatimTextOutput("data_info_basic")
                                 )
  )
}

if (!exists("descriptive_tab")) {
  descriptive_tab <- tabItem(tabName = "descriptive",
                             h2("Statistik Deskriptif"),
                             box(title = "Ringkasan Statistik", status = "primary", solidHeader = TRUE, width = NULL,
                                 verbatimTextOutput("basic_summary"),
                                 hr(),
                                 p("Analisis deskriptif menggunakan data Social Vulnerability Index yang sesungguhnya dari publikasi ilmiah.")
                             )
  )
}

# Load server modules
cat("Loading server modules...\n")
files_loaded$server_beranda <- safe_source("server/server_beranda.R")
files_loaded$server_data_management <- safe_source("server/server_data_management.R")
files_loaded$server_exploration <- safe_source("server/server_exploration.R")
files_loaded$server_assumptions <- safe_source("server/server_assumptions.R")
files_loaded$server_inference <- safe_source("server/server_inference.R")
files_loaded$server_regression <- safe_source("server/server_regression.R")
files_loaded$server_spatial <- safe_source("server/server_spatial.R")

# Main UI
ui <- dashboardPage(
  dashboardHeader(
    title = tags$span(
      tags$i(class = "fas fa-globe"),
      "WASKITA",
      style = "font-family: 'Poppins', sans-serif; font-weight: 600;"
    ),
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem(" BERANDA", tabName = "beranda", icon = icon("home")),
      menuItem("MANAJEMEN DATA", tabName = "data_management", icon = icon("database")),
      menuItem("EKSPLORASI DATA", tabName = "exploration", icon = icon("search"),
               menuSubItem("Statistik Deskriptif", tabName = "descriptive"),
               menuSubItem("Visualisasi Data", tabName = "visualization"),
               menuSubItem("Tabel Interaktif", tabName = "tables")
      ),
      menuItem(" UJI ASUMSI DATA", tabName = "assumptions", icon = icon("check-circle"),
               menuSubItem("Uji Normalitas", tabName = "normality"),
               menuSubItem("Uji Homogenitas", tabName = "homogeneity")
      ),
      menuItem(" STATISTIK INFERENSIA", tabName = "inference", icon = icon("chart-bar"),
               menuSubItem("Uji Beda Rata-rata", tabName = "mean_test"),
               menuSubItem("Uji Proporsi & Variance", tabName = "prop_var_test"),
               menuSubItem("ANOVA", tabName = "anova")
      ),
      menuItem(" REGRESI LINEAR", tabName = "regression", icon = icon("line-chart")),
      menuItem(" ANALISIS SPASIAL", tabName = "spatial", icon = icon("map"))
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML(custom_css))),
    
    tabItems(
      beranda_tab,
      data_management_tab,
      descriptive_tab,
      visualization_tab,
      tables_tab,
      normality_tab,
      homogeneity_tab,
      mean_test_tab,
      prop_var_test_tab,
      anova_tab,
      regression_tab,
      spatial_tab
    )
  )
)

# Main Server with LOCAL DATA ONLY
server <- function(input, output, session) {
  
  cat("Initializing server with LOCAL DATA ONLY...\n")
  
  # Load LOCAL data with comprehensive error handling
  data_list <- NULL
  original_data <- NULL
  distance_data <- NULL
  current_data <- reactiveVal()
  data_loaded <- reactiveVal(FALSE)
  data_source <- reactiveVal("UNKNOWN")
  
  tryCatch({
    data_list <- load_data()
    
    if (is.null(data_list)) {
      cat("✗ CRITICAL: Local data loading failed!\n")
      data_loaded(FALSE)
      data_source("ERROR")
    } else {
      original_data <- reactive({ data_list$sovi })
      distance_data <- reactive({ data_list$distance })
      data_source(data_list$source)
      
      observe({
        current_data(original_data())
      })
      
      data_loaded(TRUE)
      cat("✓ LOCAL Data initialized successfully!\n")
    }
    
  }, error = function(e) {
    cat("✗ CRITICAL Error initializing LOCAL data:", e$message, "\n")
    data_loaded(FALSE)
    data_source("ERROR")
  })
  
  # Modified data source badge for top-right position
  output$data_source_badge_top <- renderUI({
    if (data_loaded()) {
      div(class = "hero-badge-right",
          tags$i(class = "fas fa-folder"),
          "Data Local"
      )
    } else {
      div(class = "hero-badge-right error",
          tags$i(class = "fas fa-exclamation-triangle"),
          "Data Error"
      )
    }
  })
  
  # Keep original data source badge for backward compatibility
  output$data_source_badge <- renderUI({
    if (data_loaded()) {
      div(class = "data-source-badge",
          tags$i(class = "fas fa-folder"),
          "Data Source: LOCAL DIRECTORY"
      )
    } else {
      div(class = "data-source-badge error",
          tags$i(class = "fas fa-exclamation-triangle"),
          "Data Source: ERROR - Check Local Files"
      )
    }
  })
  
  # Enhanced outputs for LOCAL data - Hero Section
  output$total_obs_hero <- renderText({
    if (data_loaded() && !is.null(current_data())) {
      as.character(nrow(current_data()))
    } else {
      "Error"
    }
  })
  
  output$total_vars_hero <- renderText({
    if (data_loaded() && !is.null(current_data())) {
      # Count actual SoVI variables (excluding DISTRICTCODE and derived columns)
      sovi_vars <- c("CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE", 
                     "NOELECTRIC", "LOWEDU", "GROWTH", "POVERTY", "ILLITERATE", 
                     "NOTRAINING", "DPRONE", "RENTED", "NOSEWER", "TAPWATER", "POPULATION")
      available_vars <- intersect(sovi_vars, names(current_data()))
      as.character(length(available_vars))
    } else {
      "Error"
    }
  })
  
  output$total_provinces <- renderText({
    if (data_loaded() && !is.null(current_data())) {
      data <- current_data()
      
      # Try to get provinces from PROVINCE column if it exists
      if ("PROVINCE" %in% names(data)) {
        unique_provinces <- unique(data$PROVINCE)
        # Filter out any NA or empty values
        valid_provinces <- unique_provinces[!is.na(unique_provinces) & unique_provinces != ""]
        return(as.character(length(valid_provinces)))
      }
      
      # If PROVINCE column doesn't exist, try to derive from DISTRICTCODE
      if ("DISTRICTCODE" %in% names(data)) {
        # Extract province codes (first 2 digits)
        province_codes <- substr(as.character(data$DISTRICTCODE), 1, 2)
        unique_codes <- unique(province_codes[!is.na(province_codes) & province_codes != ""])
        
        # Map to actual Indonesian provinces
        indonesia_provinces <- c()
        for (code in unique_codes) {
          province_name <- case_when(
            # Sumatera
            code == "11" ~ "Aceh",
            code == "12" ~ "Sumatera Utara",
            code == "13" ~ "Sumatera Barat",
            code == "14" ~ "Riau",
            code == "15" ~ "Jambi",
            code == "16" ~ "Sumatera Selatan",
            code == "17" ~ "Bengkulu",
            code == "18" ~ "Lampung",
            code == "19" ~ "Kepulauan Bangka Belitung",
            code == "21" ~ "Kepulauan Riau",
            
            # Jawa
            code == "31" ~ "DKI Jakarta",
            code == "32" ~ "Jawa Barat",
            code == "33" ~ "Jawa Tengah",
            code == "34" ~ "DI Yogyakarta",
            code == "35" ~ "Jawa Timur",
            code == "36" ~ "Banten",
            
            # Bali & Nusa Tenggara
            code == "51" ~ "Bali",
            code == "52" ~ "Nusa Tenggara Barat",
            code == "53" ~ "Nusa Tenggara Timur",
            
            # Kalimantan
            code == "61" ~ "Kalimantan Barat",
            code == "62" ~ "Kalimantan Tengah",
            code == "63" ~ "Kalimantan Selatan",
            code == "64" ~ "Kalimantan Timur",
            code == "65" ~ "Kalimantan Utara",
            
            # Sulawesi
            code == "71" ~ "Sulawesi Utara",
            code == "72" ~ "Sulawesi Tengah",
            code == "73" ~ "Sulawesi Selatan",
            code == "74" ~ "Sulawesi Tenggara",
            code == "75" ~ "Gorontalo",
            code == "76" ~ "Sulawesi Barat",
            
            # Maluku
            code == "81" ~ "Maluku",
            code == "82" ~ "Maluku Utara",
            
            # Papua
            code == "91" ~ "Papua Barat",
            code == "94" ~ "Papua",
            
            TRUE ~ paste("Province", code)
          )
          
          if (!is.na(province_name) && province_name != "") {
            indonesia_provinces <- c(indonesia_provinces, province_name)
          }
        }
        
        return(as.character(length(unique(indonesia_provinces))))
      }
      
      # Fallback if no province information available
      return("N/A")
    } else {
      "Error"
    }
  })
  
  # Main section outputs - LOCAL DATA
  output$total_obs <- renderText({
    if (data_loaded() && !is.null(current_data())) {
      as.character(nrow(current_data()))
    } else {
      "Data Loading Failed"
    }
  })
  
  output$total_vars <- renderText({
    if (data_loaded() && !is.null(current_data())) {
      # Count actual SoVI variables
      sovi_vars <- c("CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE", 
                     "NOELECTRIC", "LOWEDU", "GROWTH", "POVERTY", "ILLITERATE", 
                     "NOTRAINING", "DPRONE", "RENTED", "NOSEWER", "TAPWATER", "POPULATION")
      available_vars <- intersect(sovi_vars, names(current_data()))
      as.character(length(available_vars))
    } else {
      "Error"
    }
  })
  
  output$data_preview_basic <- DT::renderDataTable({
    if (data_loaded() && !is.null(current_data())) {
      DT::datatable(current_data(),
                    options = list(
                      scrollX = TRUE,
                      pageLength = 10,
                      dom = 'Bfrtip',
                      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                    ),
                    caption = "Social Vulnerability Index Data - Source: LOCAL DIRECTORY")
    } else {
      DT::datatable(data.frame(Error = "Failed to load LOCAL data. Please check file paths and permissions."),
                    caption = "Data Loading Error")
    }
  })
  
  output$data_info_basic <- renderPrint({
    if (data_loaded() && !is.null(current_data())) {
      data <- current_data()
      cat("LOCAL DATASET INFORMATION:\n")
      cat("=====================================\n")
      cat("Dataset: Social Vulnerability Index (LOCAL)\n")
      cat("Data Source: LOCAL DIRECTORY\n")
      cat("Path: D:/Perkuliahan Tingkat 2 Semester 4/WASKITA/data\n")
      cat("Metadata URL: https://www.sciencedirect.com/science/article/pii/S2352340921010180\n")
      if("LOAD_TIMESTAMP" %in% names(data)) {
        cat("Load Timestamp:", as.character(data$LOAD_TIMESTAMP[1]), "\n")
      }
      cat("Rows:", nrow(data), "\n")
      cat("Columns:", ncol(data), "\n")
      cat("Numeric variables:", sum(sapply(data, is.numeric)), "\n")
      cat("Factor variables:", sum(sapply(data, is.factor)), "\n")
      cat("Character variables:", sum(sapply(data, is.character)), "\n")
      cat("\nActual column names:\n")
      cat(paste(names(data), collapse = ", "))
      
      if ("PROVINCE" %in% names(data)) {
        cat("\n\nProvince distribution:\n")
        print(table(data$PROVINCE))
      }
      
      cat("\n\nFirst few rows of LOCAL data:\n")
      print(head(data, 3))
    } else {
      cat("CRITICAL ERROR: Cannot display LOCAL data information.\n")
      cat("Please check:\n")
      cat("1. Local files exist in: D:/Perkuliahan Tingkat 2 Semester 4/WASKITA/data\n")
      cat("2. Files: sovi_data.csv and distance.csv\n")
      cat("3. File permissions allow reading\n")
      cat("4. Files are in proper CSV format\n")
    }
  })
  
  output$basic_summary <- renderPrint({
    if (data_loaded() && !is.null(current_data())) {
      cat("LOCAL DATA Statistical Summary:\n")
      cat("===============================\n")
      cat("Data Source: LOCAL DIRECTORY\n")
      cat("Metadata: https://www.sciencedirect.com/science/article/pii/S2352340921010180\n\n")
      summary(current_data())
    } else {
      cat("Cannot generate summary: LOCAL data loading failed.\n")
      cat("Please ensure files exist in: D:/Perkuliahan Tingkat 2 Semester 4/WASKITA/data\n")
    }
  })
  
  # Update choices with LOCAL data
  observe({
    if (data_loaded() && !is.null(current_data())) {
      tryCatch({
        if (exists("update_all_choices")) {
          update_all_choices(session, current_data)
        } else {
          # Basic choice updates for LOCAL data
          data <- current_data()
          numeric_vars <- names(data)[sapply(data, is.numeric)]
          
          if (length(numeric_vars) > 0) {
            # Update only if inputs exist
            if (!is.null(input$spatial_var)) {
              updateSelectInput(session, "spatial_var", choices = numeric_vars)
            }
          }
        }
      }, error = function(e) {
        cat("Error updating choices with LOCAL data:", e$message, "\n")
      })
    }
  })
  
  # Load server modules with LOCAL data
  tryCatch({
    if (files_loaded$server_beranda && exists("beranda_server")) {
      beranda_server(input, output, session, current_data)
      cat("✓ Beranda server loaded with LOCAL data\n")
    }
    
    if (files_loaded$server_data_management && exists("data_management_server")) {
      data_management_server(input, output, session, current_data, original_data)
      cat("✓ Data management server loaded with LOCAL data\n")
    }
    
    if (files_loaded$server_exploration && exists("exploration_server")) {
      exploration_server(input, output, session, current_data)
      cat("✓ Exploration server loaded with LOCAL data\n")
    }
    
    if (files_loaded$server_assumptions && exists("assumptions_server")) {
      assumptions_server(input, output, session, current_data)
      cat("✓ Assumptions server loaded with LOCAL data\n")
    }
    
    if (files_loaded$server_inference && exists("inference_server")) {
      inference_server(input, output, session, current_data)
      cat("✓ Inference server loaded with LOCAL data\n")
    }
    
    if (files_loaded$server_regression && exists("regression_server")) {
      regression_server(input, output, session, current_data)
      cat("✓ Regression server loaded with LOCAL data\n")
    }
    
    if (files_loaded$server_spatial && exists("spatial_server")) {
      spatial_server(input, output, session, current_data, distance_data)
      cat("✓ Spatial server loaded with LOCAL data\n")
    }
    
  }, error = function(e) {
    cat("Error loading server modules with LOCAL data:", e$message, "\n")
  })
  
  cat("✓ Server initialization complete with LOCAL DATA!\n")
}

# Run the application with LOCAL DATA ONLY
cat("Starting WASKITA Dashboard with SoVI DATA...\n")
cat("This is for FINAL PROJECT - using authentic local data with scientific metadata!\n")
cat("Local Path: D:/Perkuliahan Tingkat 2 Semester 4/WASKITA/data\n")
cat("Metadata: https://www.sciencedirect.com/science/article/pii/S2352340921010180\n")
tryCatch({
  shinyApp(ui = ui, server = server)
}, error = function(e) {
  cat("CRITICAL ERROR starting application with LOCAL data:", e$message, "\n")
  cat("Please check:\n")
  cat("1. R installation and required packages\n")
  cat("2. Local files exist in: D:/Perkuliahan Tingkat 2 Semester 4/WASKITA/data\n")
  cat("3. Files: sovi_data.csv and distance.csv\n")
  cat("4. File permissions and accessibility\n")
})