# app.R
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(lubridate)
library(zoo)
library(DT)
library(tidyr)

detect_var_types <- function(df) {
  if (is.null(df)) return(list(numeric = character(0),
                               categorical = character(0),
                               date = character(0)))
  numeric_vars <- names(df)[sapply(df, is.numeric)]
  categorical_vars <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
  date_candidates <- names(df)[sapply(df, function(x) inherits(x, "Date") || inherits(x, "POSIXt"))]
  if (length(date_candidates) == 0) {
    char_vars <- names(df)[sapply(df, is.character)]
    date_like <- sapply(char_vars, function(v) {
      x <- suppressWarnings(ymd(df[[v]]))
      mean(!is.na(x)) > 0.7
    })
    date_candidates <- char_vars[date_like]
  }
  list(
    numeric = numeric_vars,
    categorical = categorical_vars,
    date = date_candidates
  )
}

safe_numeric     <- function(vars) if (length(vars) == 0) NULL else vars
safe_categorical <- function(vars) if (length(vars) == 0) NULL else vars
safe_date        <- function(vars) if (length(vars) == 0) NULL else vars

generate_demo_data <- function() {
  set.seed(123)
  n <- 500
  dates <- seq.Date(from = Sys.Date() - 365, by = "day", length.out = n)
  groups <- sample(c("A", "B", "C"), n, replace = TRUE)
  categories <- sample(c("Segment 1", "Segment 2", "Segment 3", "Segment 4"), n, replace = TRUE)
  value1 <- cumsum(rnorm(n, mean = 0.5, sd = 2)) + 50
  value2 <- cumsum(rnorm(n, mean = 0.2, sd = 1.5)) + 30
  data.frame(
    date = dates,
    group = groups,
    category = categories,
    value1 = round(value1, 2),
    value2 = round(value2, 2),
    stringsAsFactors = FALSE
  )
}

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = span("R Project"),
    tags$li(
      class = "dropdown",
      div(
        class = "theme-switch-wrapper",
        tags$label(
          class = "theme-toggle-premium",
          `for` = "theme_toggle",
          tags$input(
            id = "theme_toggle",
            type = "checkbox",
            class = "theme-toggle-input"
          ),
          span(class = "theme-toggle-rail"),
          span(class = "theme-toggle-knob"),
          span(class = "theme-toggle-label-left", "L"),
          span(class = "theme-toggle-label-right", "D")
        ),
        tags$span(class = "theme-label", "Dark mode")
      )
    )
  ),
  dashboardSidebar(
    width = 260,
    sidebarMenu(
      id = "tabs",
      menuItem("Project Overview", tabName = "overview", icon = icon("circle-play")),
      menuItem("Data Import", tabName = "import", icon = icon("file-arrow-up")),
      menuItem("Data Transformation", tabName = "transform", icon = icon("wand-magic-sparkles")),
      menuItem("Data Summary", tabName = "summary", icon = icon("table-cells")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Forecasting Models", tabName = "forecast", icon = icon("chart-line-up")),
      menuItem("Clustering (K-means)", tabName = "clustering", icon = icon("object-group")),
      menuItem("PCA Analysis", tabName = "pca", icon = icon("project-diagram")),
      menuItem("Video Overview", tabName = "video", icon = icon("video")),
      menuItem("Contact", tabName = "contact", icon = icon("address-card"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        :root {
          --header-bg: #1976D2;
          --sidebar-bg: #E3F2FD;
          --active-bg: #1565C0;
          --body-bg: #F5F7FB;
          --text-main: #1F2933;
          --box-bg: #FFFFFF;
        }
        [data-theme='dark'] {
          --header-bg: #111827;
          --sidebar-bg: #020617;
          --active-bg: #1D4ED8;
          --body-bg: #020617;
          --text-main: #E5E7EB;
          --box-bg: #020617;
        }
        body, .content-wrapper, .right-side {
          background-color: var(--body-bg);
          color: var(--text-main);
          font-family: 'Segoe UI','Roboto',sans-serif;
        }
        .skin-black .main-header .logo {
          background-color: var(--header-bg) !important;
          color: #FFFFFF !important;
          font-weight: 600;
        }
        .skin-black .main-header .navbar {
          background-color: var(--header-bg) !important;
        }
        .skin-black .main-sidebar {
          background-color: var(--sidebar-bg) !important;
        }
        .content-wrapper, .right-side {
          background-color: var(--body-bg) !important;
        }

        /* ----- SIDEBAR MENU ANIMATIONS ----- */
        .skin-black .main-sidebar .sidebar-menu > li {
          margin: 2px 8px;
        }
        .skin-black .sidebar-menu > li > a {
          position: relative;
          overflow: hidden;
          border-radius: 999px;
          margin: 2px 0;
          padding: 10px 16px;
          display: flex;
          align-items: center;
          gap: 8px;
          color: #1F2933 !important;
          background-color: transparent;
          transition:
            background-color 220ms ease-out,
            color 220ms ease-out,
            padding-left 220ms ease-out,
            box-shadow 220ms ease-out,
            transform 180ms ease-out;
        }
        [data-theme='dark'] .skin-black .sidebar-menu > li > a {
          color: #E5E7EB !important;
        }
        .skin-black .sidebar-menu > li > a:hover {
          padding-left: 24px;
          background-color: rgba(59,130,246,0.08);
          box-shadow: inset 3px 0 0 rgba(56,189,248,0.9);
          transform: translateX(1px);
        }
        [data-theme='dark'] .skin-black .sidebar-menu > li > a:hover {
          background: radial-gradient(circle at left, rgba(37,99,235,0.36), transparent 70%);
        }
        .skin-black .sidebar-menu > li.active > a {
          padding-left: 26px;
          background-color: var(--active-bg) !important;
          color: #FFFFFF !important;
          box-shadow:
            inset 4px 0 0 rgba(248,250,252,0.92),
            0 0 18px rgba(59,130,246,0.55);
        }
        [data-theme='dark'] .skin-black .sidebar-menu > li.active > a {
          background: linear-gradient(90deg,#1D4ED8,#22C55E);
          box-shadow:
            0 0 18px rgba(59,130,246,0.7),
            0 0 32px rgba(34,197,94,0.5);
        }
        .skin-black .sidebar-menu > li > a > .fa {
          width: 20px;
          text-align: center;
          transition: transform 220ms ease-out, color 220ms ease-out;
        }
        .skin-black .sidebar-menu > li > a:hover > .fa {
          transform: translateX(3px) scale(1.03);
        }
        .skin-black .sidebar-menu > li.active > a > .fa {
          transform: translateX(3px);
        }
        .skin-black .sidebar-menu > li.active > a,
        .skin-black .sidebar-menu > li:hover > a {
          border-left: none;
        }

        /* ----- BOX STYLING + ENTRANCE ANIMATION ----- */
        @keyframes boxSoftIn {
          0% {
            opacity: 0;
            transform: translateY(12px) scale(0.97);
            filter: blur(2px);
          }
          70% {
            opacity: 1;
            transform: translateY(-2px) scale(1.01);
            filter: blur(0px);
          }
          100% {
            opacity: 1;
            transform: translateY(0) scale(1);
          }
        }

        .box {
          border-radius: 14px;
          border: 1px solid rgba(15,23,42,0.16);
          background-color: var(--box-bg);
          box-shadow: 0 18px 45px rgba(15,23,42,0.35);
          animation: boxSoftIn 420ms cubic-bezier(0.16, 1, 0.3, 1);
          animation-fill-mode: both;
        }
        [data-theme='dark'] .box {
          border: 1px solid #1F2937;
          box-shadow: 0 18px 55px rgba(0,0,0,0.85);
        }
        .box-header {
          border-bottom: 1px solid rgba(148,163,184,0.35);
        }
        h1, h2, h3, h4 {
          color: var(--text-main);
          font-weight: 600;
        }

        /* ----- KPI VALUEBOX ENTRANCE + HOVER ----- */
        @keyframes kpiFadeLift {
          0% {
            opacity: 0;
            transform: translateY(14px) scale(0.94);
            filter: blur(2px);
          }
          55% {
            opacity: 1;
            transform: translateY(-3px) scale(1.02);
            filter: blur(0px);
          }
          100% {
            opacity: 1;
            transform: translateY(0px) scale(1);
          }
        }

        .small-box {
          position: relative;
          overflow: hidden;
          border-radius: 18px;
          transition:
            transform 180ms cubic-bezier(0.16, 1, 0.3, 1),
            box-shadow 200ms ease-out,
            border-color 200ms ease-out,
            background 220ms ease-out;
          border: 1px solid rgba(148, 163, 184, 0.45);
          opacity: 0;
          transform: translateY(12px) scale(0.96);
          animation: kpiFadeLift 520ms cubic-bezier(0.16, 1, 0.3, 1) forwards;
        }

        .row .small-box:nth-child(1) { animation-delay: 40ms; }
        .row .small-box:nth-child(2) { animation-delay: 90ms; }
        .row .small-box:nth-child(3) { animation-delay: 140ms; }
        .row .small-box:nth-child(4) { animation-delay: 190ms; }

        .small-box::before,
        .small-box::after {
          content: '';
          position: absolute;
          inset: -40%;
          background: radial-gradient(circle at top left,
                      rgba(255,255,255,0.55),
                      transparent 65%);
          opacity: 0;
          transform: translateX(-40%) rotate(-18deg);
          transition: opacity 220ms ease-out, transform 420ms ease-out;
          pointer-events: none;
        }

        .small-box::after {
          inset: auto;
          left: -40%;
          right: -40%;
          top: -10%;
          bottom: -10%;
          background: linear-gradient(120deg,
                      rgba(56,189,248,0.45),
                      rgba(129,140,248,0.45),
                      rgba(16,185,129,0.4));
          mix-blend-mode: screen;
        }

        .small-box:hover {
          transform: translateY(-4px) scale(1.06) rotate(-0.4deg);
          box-shadow:
            0 18px 40px rgba(15,23,42,0.35),
            0 0 0 1px rgba(59,130,246,0.35);
          border-color: rgba(59,130,246,0.65);
        }

        .small-box:hover::before,
        .small-box:hover::after {
          opacity: 1;
          transform: translateX(35%) rotate(-12deg);
        }

        [data-theme='dark'] .small-box {
          border-color: rgba(31, 41, 55, 0.9);
          background: radial-gradient(circle at top left, #0F172A 0, #020617 55%);
        }

        [data-theme='dark'] .small-box:hover {
          box-shadow:
            0 24px 60px rgba(0,0,0,0.9),
            0 0 0 1px rgba(59,130,246,0.75);
        }

        /* ----- TAB TRANSITION ANIMATION ----- */
        @keyframes tabSlideFade {
          0% {
            opacity: 0;
            transform: translateY(10px);
          }
          100% {
            opacity: 1;
            transform: translateY(0px);
          }
        }

        .tab-content > .tab-pane {
          opacity: 0;
          transform: translateY(10px);
          transition: opacity 240ms ease-out, transform 240ms ease-out;
        }

        .tab-content > .active {
          opacity: 1;
          transform: translateY(0px);
          animation: tabSlideFade 260ms ease-out;
        }

        /* ----- PLOT FADE-IN ANIMATION ----- */
        @keyframes plotFadeIn {
          0% {
            opacity: 0;
            transform: translateY(6px);
          }
          100% {
            opacity: 1;
            transform: translateY(0px);
          }
        }

        .shiny-plot-output {
          background-color: #FFFFFF;
          border-radius: 8px;
          opacity: 0;
          transform: translateY(4px);
          animation: plotFadeIn 260ms ease-out forwards;
        }
        [data-theme='dark'] .shiny-plot-output {
          background-color: #020617;
        }

        /* ----- LIGHT MODE TABLE CARDS ----- */
        #data_table_container, #data_table_container2 {
          background-color: #FFFFFF;
          color: #020617;
          padding: 10px;
          border-radius: 12px;
          border: 1px solid #E0E0E0;
        }

        /* ----- DARK MODE TABLE CARDS ----- */
        [data-theme='dark'] #data_table_container,
        [data-theme='dark'] #data_table_container2 {
          background: radial-gradient(circle at top left, #1F2937 0, #020617 52%);
          color: #E5E7EB;
          padding: 12px;
          border-radius: 14px;
          border: 1px solid #1F2937;
        }
        [data-theme='dark'] #data_table_container .dataTables_wrapper,
        [data-theme='dark'] #data_table_container2 .dataTables_wrapper {
          color: #E5E7EB;
        }
        [data-theme='dark'] table.dataTable {
          background-color: #020617 !important;
          color: #E5E7EB !important;
          border-collapse: separate !important;
          border-spacing: 0 !important;
        }
        [data-theme='dark'] table.dataTable thead th {
          background-color: #0F172A !important;
          color: #F9FAFB !important;
          border-bottom: 1px solid #1F2937 !important;
        }
        [data-theme='dark'] table.dataTable tbody tr {
          background-color: #020617 !important;
        }
        [data-theme='dark'] table.dataTable tbody tr:hover {
          background-color: rgba(56,189,248,0.10) !important;
        }
        [data-theme='dark'] table.dataTable td,
        [data-theme='dark'] table.dataTable th {
          border-color: #020617 !important;
        }
        [data-theme='dark'] .dataTables_wrapper .dataTables_info,
        [data-theme='dark'] .dataTables_wrapper .dataTables_length label,
        [data-theme='dark'] .dataTables_wrapper .dataTables_filter label {
          color: #E5E7EB !important;
        }
        [data-theme='dark'] .dataTables_wrapper .dataTables_filter input,
        [data-theme='dark'] .dataTables_wrapper .dataTables_length select {
          background-color: #020617 !important;
          color: #E5E7EB !important;
          border: 1px solid #1F2937 !important;
          border-radius: 8px;
          padding: 3px 6px;
        }
        [data-theme='dark'] .dataTables_wrapper .dataTables_paginate .paginate_button {
          color: #E5E7EB !important;
          border-radius: 999px !important;
          border: 1px solid #1F2937 !important;
          background: #020617 !important;
          padding: 4px 10px !important;
          margin: 0 2px !important;
        }
        [data-theme='dark'] .dataTables_wrapper .dataTables_paginate .paginate_button.current,
        [data-theme='dark'] .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
          color: #020617 !important;
          background: linear-gradient(90deg,#22C55E,#EAB308) !important;
          border-color: transparent !important;
        }

        /* ----- DATATABLE ROW HOVER ANIMATION ----- */
        table.dataTable tbody tr {
          transition:
            background-color 160ms ease-out,
            transform 140ms ease-out,
            box-shadow 160ms ease-out;
        }
        table.dataTable tbody tr:hover {
          background-color: rgba(59,130,246,0.06);
          transform: translateY(-1px);
          box-shadow: 0 6px 14px rgba(15,23,42,0.18);
        }
        [data-theme='dark'] table.dataTable tbody tr:hover {
          background-color: rgba(59,130,246,0.14) !important;
        }

        /* ----- GLOBAL INPUT / BUTTON ANIMATIONS (SAFE) ----- */
        .shiny-input-container input,
        .shiny-input-container select,
        .shiny-input-container textarea {
          border-radius: 8px !important;
          transition:
            border-color 180ms ease-out,
            box-shadow 180ms ease-out,
            background-color 180ms ease-out;
        }
        .shiny-input-container input:focus,
        .shiny-input-container select:focus,
        .shiny-input-container textarea:focus {
          outline: none !important;
          box-shadow: 0 0 0 2px rgba(59,130,246,0.45);
          border-color: #2563EB;
        }
        [data-theme='dark'] .shiny-input-container input:focus,
        [data-theme='dark'] .shiny-input-container select:focus,
        [data-theme='dark'] .shiny-input-container textarea:focus {
          box-shadow: 0 0 0 2px rgba(56,189,248,0.6);
          border-color: #38BDF8;
          background-color: #020617;
        }

        .btn,
        .btn-default,
        .btn-primary {
          position: relative;
          overflow: hidden;
          border-radius: 999px;
          padding: 7px 18px;
          font-weight: 500;
          border: 1px solid transparent;
          transition:
            background-color 200ms ease-out,
            color 200ms ease-out,
            box-shadow 200ms ease-out,
            transform 140ms ease-out,
            border-color 200ms ease-out;
        }
        .btn-primary,
        .btn-default,
        .btn-info {
          background: linear-gradient(90deg,#1D4ED8,#0EA5E9);
          color: #FFFFFF;
          border-color: #1D4ED8;
        }
        .btn:hover,
        .btn-primary:hover,
        .btn-default:hover {
          transform: translateY(-1px);
          box-shadow: 0 10px 20px rgba(15,23,42,0.3);
        }
        .btn:active,
        .btn-primary:active,
        .btn-default:active {
          transform: translateY(0);
          box-shadow: 0 4px 10px rgba(15,23,42,0.25) inset;
        }
        .btn::before,
        .btn-primary::before,
        .btn-default::before {
          content: '';
          position: absolute;
          top: 0;
          left: -120%;
          width: 60%;
          height: 100%;
          background: linear-gradient(
            120deg,
            rgba(255,255,255,0.1),
            rgba(255,255,255,0.65),
            rgba(255,255,255,0.05)
          );
          transform: skewX(-20deg);
          opacity: 0;
          transition: opacity 180ms ease-out, left 480ms ease-out;
        }
        .btn:hover::before,
        .btn-primary:hover::before,
        .btn-default:hover::before {
          opacity: 1;
          left: 130%;
        }
        [data-theme='dark'] .btn,
        [data-theme='dark'] .btn-default,
        [data-theme='dark'] .btn-primary {
          background: linear-gradient(90deg,#2563EB,#22C55E);
          border-color: #1D4ED8;
          box-shadow: 0 10px 25px rgba(15,23,42,0.8);
        }
        .btn-file {
          position: relative;
          overflow: hidden;
          border-radius: 999px !important;
        }
        .btn-file input[type=file] {
          cursor: pointer;
        }
        [data-theme='dark'] .btn-file {
          background: linear-gradient(90deg,#2563EB,#22C55E);
          border-color: #1D4ED8;
        }

        /* ---- PREMIUM MINIMAL THEME TOGGLE ---- */
        .theme-switch-wrapper {
          display: flex;
          align-items: center;
          padding: 15px 20px 0 0;
        }
        .theme-label {
          margin-left: 10px;
          color: #FFFFFF;
          font-size: 13px;
          letter-spacing: 0.04em;
          text-transform: uppercase;
          opacity: 0.9;
        }
        .theme-toggle-premium {
          position: relative;
          width: 66px;
          height: 26px;
          display: inline-block;
          cursor: pointer;
        }
        .theme-toggle-input {
          position: absolute;
          opacity: 0;
          width: 0;
          height: 0;
        }
        .theme-toggle-rail {
          position: absolute;
          inset: 0;
          border-radius: 999px;
          background: linear-gradient(90deg,#E5E7EB,#E0F2FE);
          box-shadow:
            0 3px 7px rgba(15,23,42,0.28),
            inset 0 0 0 1px rgba(148,163,184,0.55);
          transition:
            background 180ms ease-out,
            box-shadow 180ms ease-out;
        }
        .theme-toggle-knob {
          position: absolute;
          top: 3px;
          left: 3px;
          width: 20px;
          height: 20px;
          border-radius: 50%;
          background: #FFFFFF;
          box-shadow:
            0 2px 6px rgba(15,23,42,0.45),
            0 0 0 1px rgba(148,163,184,0.6);
          transition:
            transform 180ms cubic-bezier(0.16, 1, 0.3, 1),
            box-shadow 180ms ease-out,
            background 180ms ease-out;
        }
        .theme-toggle-label-left,
        .theme-toggle-label-right {
          position: absolute;
          top: 50%;
          transform: translateY(-50%);
          font-size: 9px;
          font-weight: 600;
          letter-spacing: 0.08em;
          color: #6B7280;
          pointer-events: none;
          transition: opacity 160ms ease-out, color 160ms ease-out;
        }
        .theme-toggle-label-left {
          left: 9px;
          opacity: 0.9;
        }
        .theme-toggle-label-right {
          right: 9px;
          opacity: 0.25;
        }
        .theme-toggle-input:checked ~ .theme-toggle-rail {
          background: linear-gradient(90deg,#0F172A,#1D4ED8);
          box-shadow:
            0 4px 10px rgba(0,0,0,0.85),
            inset 0 0 0 1px rgba(30,64,175,0.9);
        }
        .theme-toggle-input:checked ~ .theme-toggle-knob {
          transform: translateX(40px);
          background: #020617;
          box-shadow:
            0 2px 7px rgba(0,0,0,0.85),
            0 0 0 1px rgba(30,64,175,0.9);
        }
        .theme-toggle-input:checked ~ .theme-toggle-label-left {
          opacity: 0.25;
          color: #9CA3AF;
        }
        .theme-toggle-input:checked ~ .theme-toggle-label-right {
          opacity: 0.95;
          color: #F9FAFB;
        }

        .profile-img {
          width: 170px;
          height: 170px;
          border-radius: 50%;
          object-fit: cover;
          border: 3px solid #1976D2;
          box-shadow: 0 4px 12px rgba(15,23,42,0.35);
          margin-bottom: 15px;
        }

        /* ---- NEW CONTACT ROW ANIMATIONS ---- */
        .contact-row {
          display: flex;
          align-items: center;
          gap: 10px;
          padding: 10px 14px;
          border-radius: 999px;
          background: rgba(148, 163, 184, 0.08);
          cursor: pointer;
          transition:
            background-color 200ms ease-out,
            transform 160ms ease-out,
            box-shadow 200ms ease-out;
          margin-bottom: 15px;
        }
        [data-theme='dark'] .contact-row {
          background: radial-gradient(circle at left,
                    rgba(37, 99, 235, 0.30),
                    rgba(15, 23, 42, 0.95));
        }
        .contact-row:hover {
          background: linear-gradient(90deg, #1D4ED8, #0EA5E9);
          transform: translateY(-1px);
          box-shadow: 0 10px 25px rgba(15, 23, 42, 0.35);
        }
        [data-theme='dark'] .contact-row:hover {
          box-shadow: 0 14px 30px rgba(0, 0, 0, 0.85);
        }

        .contact-icon {
          font-size: 24px;
          margin-right: 4px;
          transition:
            transform 220ms ease-out,
            color 200ms ease-out,
            text-shadow 220ms ease-out;
        }
        .contact-row:hover .contact-icon {
          transform: translateX(3px) scale(1.08) rotate(-2deg);
          color: #F9FAFB;
          text-shadow: 0 0 12px rgba(248, 250, 252, 0.9);
        }

        .contact-link {
          font-size: 16px;
          font-weight: 500;
          color: #1F2933;
          text-decoration: none;
          position: relative;
          transition:
            color 200ms ease-out,
            letter-spacing 160ms ease-out;
        }
        [data-theme='dark'] .contact-link {
          color: #E5E7EB;
        }
        .contact-row:hover .contact-link {
          color: #F9FAFB;
          letter-spacing: 0.03em;
        }
        .contact-link::after {
          content: '';
          position: absolute;
          left: 0;
          bottom: -3px;
          width: 0;
          height: 2px;
          background: linear-gradient(90deg, #FACC15, #22C55E);
          transition: width 260ms cubic-bezier(0.16, 1, 0.3, 1);
        }
        .contact-row:hover .contact-link::after {
          width: 100%;
        }

        .contact-row.linkedin-row {
          animation: contactPulse 2600ms ease-in-out infinite;
        }
        @keyframes contactPulse {
          0% { box-shadow: 0 0 0 0 rgba(59, 130, 246, 0.0); }
          40% { box-shadow: 0 0 0 8px rgba(59, 130, 246, 0.15); }
          100% { box-shadow: 0 0 0 0 rgba(59, 130, 246, 0.0); }
        }

        /* ----- BOX HEADER TITLE SLIDE-IN ----- */
        .box-header .box-title {
          position: relative;
          opacity: 0;
          transform: translateY(8px);
          animation: titleSlideIn 340ms ease-out forwards;
        }
        .box:nth-of-type(1) .box-header .box-title { animation-delay: 60ms; }
        .box:nth-of-type(2) .box-header .box-title { animation-delay: 120ms; }
        .box:nth-of-type(3) .box-header .box-title { animation-delay: 180ms; }

        @keyframes titleSlideIn {
          0% {
            opacity: 0;
            transform: translateY(8px);
          }
          100% {
            opacity: 1;
            transform: translateY(0);
          }
        }
      ")),
      tags$script(HTML("
        document.addEventListener('DOMContentLoaded', function() {
          const checkbox = document.getElementById('theme_toggle');
          document.documentElement.setAttribute('data-theme', 'light');
          checkbox.addEventListener('change', function(e) {
            if (e.target.checked) {
              document.documentElement.setAttribute('data-theme', 'dark');
            } else {
              document.documentElement.setAttribute('data-theme', 'light');
            }
          });
        });
      "))
    ),
    
    tabItems(
      tabItem(
        tabName = "overview",
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE,
            title = "Project Overview",
            p("This dashboard provides a complete workflow: Import \u2192 Transform \u2192 Summary \u2192 Analysis \u2192 Forecasting \u2192 Clustering \u2192 PCA."),
            tags$ul(
              tags$li("Dataset-agnostic: auto-detects numeric, categorical, and date variables."),
              tags$li("Built-in demo dataset used until you upload your own on the Data Import page."),
              tags$li("Light / Dark theme switch for presentations.")
            )
          )
        ),
        fluidRow(
          valueBoxOutput("kpi_rows", width = 3),
          valueBoxOutput("kpi_cols", width = 3),
          valueBoxOutput("kpi_numeric", width = 3),
          valueBoxOutput("kpi_categorical", width = 3)
        ),
        fluidRow(
          valueBoxOutput("kpi_date", width = 3),
          valueBoxOutput("kpi_demo", width = 3),
          valueBoxOutput("kpi_missing", width = 3),
          valueBoxOutput("kpi_unique_cats", width = 3)
        ),
        fluidRow(
          box(
            title = "Active Dataset Summary", width = 6, status = "warning", solidHeader = TRUE,
            uiOutput("var_type_overview"),
            verbatimTextOutput("data_summary_basic")
          ),
          box(
            title = "Dataset Preview (Bright Table)", width = 6, status = "info", solidHeader = TRUE,
            p("High-contrast table preview for the dataset currently driving all pages."),
            div(
              id = "data_table_container",
              DTOutput("data_table_preview", height = "320px")
            )
          )
        ),
        fluidRow(
          box(
            title = "Variable Selection Controls", width = 12, status = "primary", solidHeader = TRUE,
            fluidRow(
              column(4, h4("Numeric Variables"), uiOutput("numeric_selector_overview")),
              column(4, h4("Categorical Variables"), uiOutput("categorical_selector_overview")),
              column(4, h4("Date Variables"), uiOutput("date_selector_overview"))
            ),
            hr(),
            h4("Data Glimpse"),
            verbatimTextOutput("data_glimpse")
          )
        )
      ),
      
      tabItem(
        tabName = "import",
        fluidRow(
          box(
            title = "Import & Clean Options", width = 6, status = "primary", solidHeader = TRUE,
            fileInput("file_import", "Upload CSV (replaces demo data)", accept = ".csv"),
            checkboxInput("header_import", "Header row", TRUE),
            radioButtons("sep_import", "Separator",
                         choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                         selected = ",", inline = TRUE),
            br(),
            actionButton("clean_data", "Clean dataset (remove missing values)",
                         icon = icon("broom")),
            helpText("Upload here once. The active/cleaned dataset is used by all other pages.")
          ),
          box(
            title = "Imported / Active Data Preview", width = 6, status = "info", solidHeader = TRUE,
            div(
              id = "data_table_container2",
              DTOutput("data_import_preview", height = "350px")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "transform",
        fluidRow(
          box(
            title = "Quick Transformations", width = 4, status = "primary", solidHeader = TRUE,
            uiOutput("transform_numeric"),
            sliderInput("transform_scale", "Multiply selected numeric by:",
                        min = 0.1, max = 5, value = 1, step = 0.1),
            checkboxInput("transform_center", "Center (subtract mean)", FALSE),
            checkboxInput("transform_scale_std", "Standardize (divide by sd)", FALSE),
            actionButton("apply_transform", "Apply Transformation", icon = icon("wand-magic-sparkles"))
          ),
          box(
            title = "Before vs After (Histogram)", width = 8, status = "info", solidHeader = TRUE,
            plotOutput("transform_hist")
          )
        )
      ),
      
      tabItem(
        tabName = "summary",
        fluidRow(
          box(
            title = "Descriptive Statistics", width = 6, status = "primary", solidHeader = TRUE,
            uiOutput("summary_numeric_sel"),
            verbatimTextOutput("summary_numeric_stats")
          ),
          box(
            title = "Class-Interval Frequency Table", width = 6, status = "info", solidHeader = TRUE,
            uiOutput("summary_cut_var"),
            sliderInput("summary_bins", "Number of bins:", min = 4, max = 20, value = 10),
            DTOutput("summary_freq_table")
          )
        )
      ),
      
      tabItem(
        tabName = "analysis",
        fluidRow(
          box(
            title = "Visual Analysis Controls", width = 12, status = "primary", solidHeader = TRUE,
            fluidRow(
              column(4, uiOutput("viz1_numeric_x"), uiOutput("viz1_numeric_y")),
              column(4, uiOutput("viz1_categorical"), uiOutput("viz1_color_by")),
              column(4, uiOutput("viz1_date"))
            ),
            helpText("Explore distributions, relationships, and time patterns.")
          )
        ),
        fluidRow(
          box(title = "Density Plot", width = 6, status = "info", solidHeader = TRUE,
              plotOutput("viz1_density")),
          box(title = "Boxplot by Category", width = 6, status = "info", solidHeader = TRUE,
              plotOutput("viz1_box"))
        ),
        fluidRow(
          box(title = "Scatter Plot", width = 6, status = "info", solidHeader = TRUE,
              plotOutput("viz1_scatter")),
          box(title = "Time Series", width = 6, status = "info", solidHeader = TRUE,
              plotOutput("viz1_time"))
        )
      ),
      
      tabItem(
        tabName = "forecast",
        fluidRow(
          box(
            title = "Time-Series Controls", width = 12, status = "primary", solidHeader = TRUE,
            fluidRow(
              column(4, uiOutput("adv_date"), uiOutput("adv_numeric")),
              column(4, sliderInput("adv_smooth_span", "Smoothing Span (0.1 - 1)",
                                    min = 0.1, max = 1, value = 0.4, step = 0.1)),
              column(4, uiOutput("adv_compare_group"))
            ),
            helpText("Trend, smoothing, and rolling-average views.")
          )
        ),
        fluidRow(
          box(title = "Time Series Line", width = 6, status = "info", solidHeader = TRUE,
              plotOutput("adv_time_series")),
          box(title = "Smoothed Trend", width = 6, status = "info", solidHeader = TRUE,
              plotOutput("adv_smooth_trend"))
        ),
        fluidRow(
          box(title = "Rolling Average", width = 6, status = "info", solidHeader = TRUE,
              plotOutput("adv_rolling_avg")),
          box(title = "Group Trends", width = 6, status = "info", solidHeader = TRUE,
              plotOutput("adv_group_trend"))
        )
      ),
      
      tabItem(
        tabName = "clustering",
        fluidRow(
          box(
            title = "Clustering Controls (K-means)", width = 4, status = "primary", solidHeader = TRUE,
            uiOutput("cluster_x"),
            uiOutput("cluster_y"),
            numericInput("cluster_k", "Number of clusters (k):", value = 3, min = 1, max = 9, step = 1),
            helpText("Pick two numeric variables and choose k to see how the data naturally groups.")
          ),
          box(
            title = "K-means Cluster Plot", width = 8, status = "info", solidHeader = TRUE,
            plotOutput("cluster_plot", height = 420)
          )
        )
      ),
      
      tabItem(
        tabName = "pca",
        fluidRow(
          box(
            title = "PCA Settings", width = 4, status = "primary", solidHeader = TRUE,
            uiOutput("pca_numeric_vars"),
            checkboxInput("pca_center", "Center variables", TRUE),
            checkboxInput("pca_scale", "Scale to unit variance", TRUE),
            helpText("PCA summarises many numeric variables into a few principal components.")
          ),
          box(
            title = "PCA Scatter (PC1 vs PC2)", width = 8, status = "info", solidHeader = TRUE,
            plotOutput("pca_scatter", height = 380)
          )
        ),
        fluidRow(
          box(
            title = "Explained Variance (Scree Plot)", width = 12, status = "info", solidHeader = TRUE,
            plotOutput("pca_scree", height = 280)
          )
        )
      ),
      
      tabItem(
        tabName = "video",
        fluidRow(
          box(
            title = "Dashboard Video Overview", width = 12, status = "primary", solidHeader = TRUE,
            p("Screen recording and voice explanation of the dashboard."),
            tags$br(),
            tags$video(
              src = "tutorial.mp4",
              type = "video/mp4",
              width = "100%",
              controls = "controls",
              style = "border-radius:10px; box-shadow:0 4px 12px rgba(0,0,0,0.25);"
            )
          )
        )
      ),
      
      tabItem(
        tabName = "contact",
        fluidRow(
          box(
            title = "About Muneeb ur Rehman", width = 6, status = "primary", solidHeader = TRUE,
            div(style = "text-align:center;",
                tags$img(
                  src = "profile.jpg",
                  class = "profile-img",
                  alt = "Muneeb ur Rehman"
                ),
                h2("Muneeb ur Rehman"),
                h4("B.S. Statistics ( Specialization in Data Science ) – COMSATS University Islamabad")
            ),
            p("Aspiring data professional with a strong foundation in statistical thinking and hands-on experience in data analytics and visualization using R, Python, and Power BI."),
            hr(),
            h4("Education"),
            tags$ul(
              tags$li("B.S. in Statistics (Data Science specialization), COMSATS University Islamabad – in progress."),
              tags$li("Schooling and college from Army Public School and College.")
            ),
            h4("Key Skills"),
            tags$ul(
              tags$li("Data analytics and exploratory data analysis."),
              tags$li("Data visualization and dashboarding in Power BI."),
              tags$li("Statistical analysis and modeling in R."),
              tags$li("Data manipulation and scripting in Python.")
            )
          ),
          box(
            title = "Connect with Me", width = 6, status = "info", solidHeader = TRUE,
            tags$div(
              class = "contact-row",
              icon("envelope", class = "contact-icon"),
              tags$a(
                href = "mailto:muneeb56569@gmail.com",
                class = "contact-link",
                "muneeb56569@gmail.com"
              )
            ),
            tags$div(
              class = "contact-row linkedin-row",
              icon("linkedin", class = "contact-icon"),
              tags$a(
                href = "https://www.linkedin.com/in/muneeb-ur-rehman-11a0a6271",
                target = "_blank",
                class = "contact-link",
                "LinkedIn: Muneeb ur Rehman"
              )
            ),
            tags$hr(),
            p("Feel free to reach out for opportunities, collaborations, or discussions related to data analytics, visualization, and applied statistics.")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  current_data <- reactiveVal(generate_demo_data())
  
  read_uploaded_csv <- function(file, header = TRUE, sep = ",") {
    ext <- tools::file_ext(file$name)
    validate(need(ext == "csv", "Please upload a .csv file."))
    tryCatch(
      read.csv(file$datapath, header = header, sep = sep, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
  }
  
  observeEvent(input$file_import, {
    df <- read_uploaded_csv(input$file_import,
                            header = input$header_import,
                            sep = input$sep_import)
    if (!is.null(df)) current_data(df)
  })
  
  observeEvent(input$clean_data, {
    df <- current_data()
    req(df)
    df_clean <- df %>% drop_na()
    current_data(df_clean)
  })
  
  raw_data  <- reactive(current_data())
  var_types <- reactive(detect_var_types(raw_data()))
  
  output$data_import_preview <- renderDT({
    datatable(raw_data(),
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$kpi_rows <- renderValueBox({
    df <- raw_data()
    valueBox(format(nrow(df), big.mark = ","), "Rows", icon = icon("table"), color = "aqua")
  })
  output$kpi_cols <- renderValueBox({
    df <- raw_data()
    valueBox(format(ncol(df), big.mark = ","), "Columns", icon = icon("columns"), color = "blue")
  })
  output$kpi_numeric <- renderValueBox({
    vt <- var_types()
    valueBox(length(vt$numeric), "Numeric Variables", icon = icon("hashtag"), color = "green")
  })
  output$kpi_categorical <- renderValueBox({
    vt <- var_types()
    valueBox(length(vt$categorical), "Categorical / Other", icon = icon("tags"), color = "yellow")
  })
  output$kpi_date <- renderValueBox({
    vt <- var_types()
    valueBox(length(vt$date), "Date / Time Variables", icon = icon("calendar-days"), color = "red")
  })
  output$kpi_demo <- renderValueBox({
    valueBox("Yes", "Demo Fallback Available", icon = icon("life-ring"), color = "purple")
  })
  output$kpi_missing <- renderValueBox({
    df <- raw_data()
    valueBox(sum(is.na(df)), "Missing Values", icon = icon("circle-exclamation"), color = "yellow")
  })
  output$kpi_unique_cats <- renderValueBox({
    vt <- var_types()
    df <- raw_data()
    n <- if (length(vt$categorical) == 0) 0 else max(sapply(df[vt$categorical], function(x) length(unique(x))))
    valueBox(n, "Max Unique Categories", icon = icon("list"), color = "aqua")
  })
  
  output$var_type_overview <- renderUI({
    df <- raw_data()
    vt <- var_types()
    tagList(
      h4("Automatic Dataset Summary"),
      p(paste("Rows:", nrow(df), "| Columns:", ncol(df))),
      p(paste("Numeric variables:", length(vt$numeric))),
      p(paste("Categorical variables:", length(vt$categorical))),
      p(paste("Date variables:", length(vt$date)))
    )
  })
  
  output$data_summary_basic <- renderPrint({
    summary(raw_data())
  })
  
  output$data_table_preview <- renderDT({
    df <- raw_data()
    datatable(
      df,
      options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE),
      rownames = FALSE,
      class = "display"
    )
  })
  
  output$numeric_selector_overview <- renderUI({
    vt <- var_types()
    selectInput("overview_numeric", "Select a numeric variable (optional):",
                choices = c("None", safe_numeric(vt$numeric)), selected = "None")
  })
  output$categorical_selector_overview <- renderUI({
    vt <- var_types()
    selectInput("overview_categorical", "Select a categorical variable (optional):",
                choices = c("None", safe_categorical(vt$categorical)), selected = "None")
  })
  output$date_selector_overview <- renderUI({
    vt <- var_types()
    selectInput("overview_date", "Select a date variable (optional):",
                choices = c("None", safe_date(vt$date)), selected = "None")
  })
  
  output$data_glimpse <- renderPrint({
    utils::str(head(raw_data(), 6))
  })
  
  output$transform_numeric <- renderUI({
    vt <- var_types()
    selectInput("transform_num_var", "Numeric variable:",
                choices = safe_numeric(vt$numeric))
  })
  transformed_data <- reactiveVal(NULL)
  
  observeEvent(input$apply_transform, {
    df  <- raw_data()
    vt  <- var_types()
    var <- input$transform_num_var
    req(var, var %in% vt$numeric)
    x <- df[[var]]
    x_new <- x * input$transform_scale
    if (isTRUE(input$transform_center)) x_new <- x_new - mean(x_new, na.rm = TRUE)
    if (isTRUE(input$transform_scale_std)) x_new <- x_new / sd(x_new, na.rm = TRUE)
    df[[paste0(var, "_transf")]] <- x_new
    transformed_data(df)
    current_data(df)
  })
  
  output$transform_hist <- renderPlot({
    df_base <- raw_data()
    df_trans <- transformed_data()
    var <- input$transform_num_var
    req(var)
    p1 <- ggplot(df_base, aes(x = .data[[var]])) +
      geom_histogram(fill = "#90CAF9", color = "white", bins = 30, alpha = 0.8) +
      labs(title = paste("Original:", var), x = var, y = "Count") +
      theme_minimal()
    if (!is.null(df_trans)) {
      var_new <- paste0(var, "_transf")
      p2 <- ggplot(df_trans, aes(x = .data[[var_new]])) +
        geom_histogram(fill = "#FF8A65", color = "white", bins = 30, alpha = 0.8) +
        labs(title = paste("Transformed:", var_new), x = var_new, y = "Count") +
        theme_minimal()
      gridExtra::grid.arrange(p1, p2, ncol = 2)
    } else {
      p1
    }
  })
  
  output$summary_numeric_sel <- renderUI({
    vt <- var_types()
    selectInput("summary_num_var", "Numeric variable:",
                choices = safe_numeric(vt$numeric))
  })
  output$summary_numeric_stats <- renderPrint({
    df <- raw_data()
    var <- input$summary_num_var
    req(var)
    summary(df[[var]])
  })
  output$summary_cut_var <- renderUI({
    vt <- var_types()
    selectInput("summary_cut_choice", "Variable for class intervals:",
                choices = safe_numeric(vt$numeric))
  })
  output$summary_freq_table <- renderDT({
    df <- raw_data()
    var <- input$summary_cut_choice
    req(var)
    x <- df[[var]]
    bins <- input$summary_bins
    brks <- pretty(range(x, na.rm = TRUE), n = bins)
    cut_x <- cut(x, breaks = brks, include.lowest = TRUE)
    tab <- as.data.frame(table(cut_x))
    names(tab) <- c("Class Interval", "Frequency")
    datatable(tab, options = list(dom = "t", pageLength = nrow(tab)), rownames = FALSE)
  })
  
  output$viz1_numeric_x <- renderUI({
    vt <- var_types()
    selectInput("viz1_num_x", "Numeric variable (X / density):",
                choices = safe_numeric(vt$numeric))
  })
  output$viz1_numeric_y <- renderUI({
    vt <- var_types()
    selectInput("viz1_num_y", "Numeric variable (Y for scatter):",
                choices = safe_numeric(vt$numeric))
  })
  output$viz1_categorical <- renderUI({
    vt <- var_types()
    selectInput("viz1_cat", "Categorical variable (for boxplot):",
                choices = safe_categorical(vt$categorical))
  })
  output$viz1_color_by <- renderUI({
    selectInput("viz1_color", "Color/grouping (optional):",
                choices = c("None", names(raw_data())), selected = "None")
  })
  output$viz1_date <- renderUI({
    vt <- var_types()
    selectInput("viz1_date_var", "Date variable (for time series):",
                choices = c("None", safe_date(vt$date)), selected = "None")
  })
  
  output$viz1_density <- renderPlot({
    df <- raw_data()
    req(input$viz1_num_x)
    validate(need(is.numeric(df[[input$viz1_num_x]]), "Numeric variable required."))
    ggplot(df, aes(x = .data[[input$viz1_num_x]])) +
      geom_density(fill = "#FFB74D", alpha = 0.6) +
      theme_minimal() +
      labs(x = input$viz1_num_x, y = "Density", title = paste("Density of", input$viz1_num_x))
  })
  output$viz1_box <- renderPlot({
    df <- raw_data()
    req(input$viz1_cat, input$viz1_num_x)
    validate(
      need(input$viz1_cat %in% names(df), "Categorical variable not found."),
      need(is.numeric(df[[input$viz1_num_x]]), "Numeric variable required.")
    )
    ggplot(df, aes(x = .data[[input$viz1_cat]], y = .data[[input$viz1_num_x]])) +
      geom_boxplot(fill = "#4DB6AC") +
      theme_minimal() +
      labs(x = input$viz1_cat, y = input$viz1_num_x,
           title = paste("Boxplot of", input$viz1_num_x, "by", input$viz1_cat)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  output$viz1_scatter <- renderPlot({
    df <- raw_data()
    req(input$viz1_num_x, input$viz1_num_y)
    validate(
      need(is.numeric(df[[input$viz1_num_x]]), "X must be numeric."),
      need(is.numeric(df[[input$viz1_num_y]]), "Y must be numeric.")
    )
    p <- ggplot(df, aes(x = .data[[input$viz1_num_x]], y = .data[[input$viz1_num_y]]))
    if (!is.null(input$viz1_color) && input$viz1_color != "None" &&
        input$viz1_color %in% names(df)) {
      p <- p + geom_point(aes(color = .data[[input$viz1_color]]), alpha = 0.7)
    } else {
      p <- p + geom_point(color = "#42A5F5", alpha = 0.7)
    }
    p + theme_minimal() +
      labs(x = input$viz1_num_x, y = input$viz1_num_y,
           title = paste("Scatter:", input$viz1_num_x, "vs", input$viz1_num_y))
  })
  output$viz1_time <- renderPlot({
    df <- raw_data()
    req(input$viz1_date_var, input$viz1_num_x)
    validate(need(input$viz1_date_var != "None", "Select a date variable."))
    validate(need(input$viz1_date_var %in% names(df), "Date variable not found."))
    date_vec <- suppressWarnings(ymd(df[[input$viz1_date_var]]))
    validate(need(any(!is.na(date_vec)), "Unable to parse date variable."))
    temp <- df %>%
      mutate(.date = date_vec) %>%
      filter(!is.na(.date)) %>%
      group_by(.date) %>%
      summarise(value = mean(.data[[input$viz1_num_x]], na.rm = TRUE), .groups = "drop")
    ggplot(temp, aes(x = .date, y = value)) +
      geom_line(color = "#EF5350", size = 1) +
      theme_minimal() +
      labs(x = input$viz1_date_var,
           y = paste("Average", input$viz1_num_x),
           title = paste("Time Series of", input$viz1_num_x))
  })
  
  output$adv_date <- renderUI({
    vt <- var_types()
    selectInput("adv_date_var", "Date variable:",
                choices = c("None", safe_date(vt$date)),
                selected = if (length(vt$date) > 0) vt$date[1] else "None")
  })
  output$adv_numeric <- renderUI({
    vt <- var_types()
    selectInput("adv_num_var", "Numeric variable (for trends):",
                choices = safe_numeric(vt$numeric))
  })
  output$adv_compare_group <- renderUI({
    vt <- var_types()
    selectInput("adv_compare_var", "Comparison group (optional):",
                choices = c("None", safe_categorical(vt$categorical)), selected = "None")
  })
  
  advanced_time_data <- reactive({
    df <- raw_data()
    req(input$adv_date_var, input$adv_num_var)
    validate(need(input$adv_date_var != "None", "Select a date variable."))
    validate(need(input$adv_date_var %in% names(df), "Date variable not found."))
    date_vec <- suppressWarnings(ymd(df[[input$adv_date_var]]))
    validate(need(any(!is.na(date_vec)), "Unable to parse date variable."))
    df %>%
      mutate(.date = date_vec) %>%
      filter(!is.na(.date))
  })
  
  output$adv_time_series <- renderPlot({
    df <- advanced_time_data()
    req(input$adv_num_var)
    validate(need(is.numeric(df[[input$adv_num_var]]), "Numeric variable required."))
    agg <- df %>%
      group_by(.date) %>%
      summarise(value = mean(.data[[input$adv_num_var]], na.rm = TRUE), .groups = "drop")
    ggplot(agg, aes(x = .date, y = value)) +
      geom_line(color = "#42A5F5") +
      theme_minimal() +
      labs(x = input$adv_date_var,
           y = paste("Average", input$adv_num_var),
           title = paste("Time Series of", input$adv_num_var))
  })
  
  output$adv_smooth_trend <- renderPlot({
    df <- advanced_time_data()
    req(input$adv_num_var)
    validate(need(is.numeric(df[[input$adv_num_var]]), "Numeric variable required."))
    agg <- df %>%
      group_by(.date) %>%
      summarise(value = mean(.data[[input$adv_num_var]], na.rm = TRUE), .groups = "drop")
    ggplot(agg, aes(x = .date, y = value)) +
      geom_point(alpha = 0.4, color = "#90CAF9") +
      geom_smooth(method = "loess", span = input$adv_smooth_span,
                  se = FALSE, color = "#2E7D32") +
      theme_minimal() +
      labs(x = input$adv_date_var,
           y = paste("Average", input$adv_num_var),
           title = paste("Smoothed Trend of", input$adv_num_var))
  })
  
  output$adv_rolling_avg <- renderPlot({
    df <- advanced_time_data()
    req(input$adv_num_var)
    validate(need(is.numeric(df[[input$adv_num_var]]), "Numeric variable required."))
    agg <- df %>%
      group_by(.date) %>%
      summarise(value = mean(.data[[input$adv_num_var]], na.rm = TRUE), .groups = "drop") %>%
      arrange(.date)
    window_size <- 7
    if (nrow(agg) >= window_size) {
      agg$rolling <- zoo::rollmean(agg$value, k = window_size, fill = NA, align = "right")
    } else {
      agg$rolling <- agg$value
    }
    ggplot(agg, aes(x = .date)) +
      geom_line(aes(y = value), color = "#B0BEC5") +
      geom_line(aes(y = rolling), color = "#FF7043", size = 1) +
      theme_minimal() +
      labs(x = input$adv_date_var,
           y = paste("Rolling average of", input$adv_num_var),
           title = paste("Rolling Average (7 days) of", input$adv_num_var))
  })
  
  output$adv_group_trend <- renderPlot({
    df <- advanced_time_data()
    req(input$adv_num_var)
    validate(need(is.numeric(df[[input$adv_num_var]]), "Numeric variable required."))
    if (is.null(input$adv_compare_var) || input$adv_compare_var == "None" ||
        !(input$adv_compare_var %in% names(df))) {
      validate("Select a comparison group variable.")
    }
    agg <- df %>%
      group_by(.date, .group = .data[[input$adv_compare_var]]) %>%
      summarise(value = mean(.data[[input$adv_num_var]], na.rm = TRUE), .groups = "drop")
    ggplot(agg, aes(x = .date, y = value, color = .group)) +
      geom_line(size = 1) +
      theme_minimal() +
      labs(x = input$adv_date_var,
           y = paste("Average", input$adv_num_var),
           color = input$adv_compare_var,
           title = paste("Group Trends of", input$adv_num_var))
  })
  
  output$cluster_x <- renderUI({
    vt <- var_types()
    selectInput("cluster_x_var", "X variable:", choices = safe_numeric(vt$numeric))
  })
  output$cluster_y <- renderUI({
    vt <- var_types()
    selectInput("cluster_y_var", "Y variable:", choices = safe_numeric(vt$numeric))
  })
  
  output$cluster_plot <- renderPlot({
    df <- raw_data()
    req(input$cluster_x_var, input$cluster_y_var, input$cluster_k)
    validate(
      need(is.numeric(df[[input$cluster_x_var]]), "X variable must be numeric."),
      need(is.numeric(df[[input$cluster_y_var]]), "Y variable must be numeric."),
      need(nrow(df) >= input$cluster_k, "Number of clusters must be <= number of rows.")
    )
    selected <- na.omit(df[, c(input$cluster_x_var, input$cluster_y_var)])
    validate(need(nrow(selected) > 1, "Not enough non-missing rows for clustering."))
    km <- kmeans(selected, centers = input$cluster_k)
    selected$cluster <- factor(km$cluster)
    centers <- as.data.frame(km$centers)
    ggplot(selected, aes_string(x = input$cluster_x_var, y = input$cluster_y_var, color = "cluster")) +
      geom_point(alpha = 0.7, size = 2.5) +
      geom_point(data = centers, aes_string(x = input$cluster_x_var, y = input$cluster_y_var),
                 color = "black", size = 4, shape = 4, stroke = 1.2) +
      theme_minimal() +
      labs(
        title = paste("K-means Clustering (k =", input$cluster_k, ")"),
        x = input$cluster_x_var,
        y = input$cluster_y_var,
        color = "Cluster"
      )
  })
  
  output$pca_numeric_vars <- renderUI({
    vt <- var_types()
    selectInput("pca_vars", "Numeric variables for PCA:",
                choices = safe_numeric(vt$numeric),
                selected = safe_numeric(vt$numeric),
                multiple = TRUE)
  })
  
  pca_result <- reactive({
    df <- raw_data()
    req(input$pca_vars)
    num_df <- df[, input$pca_vars, drop = FALSE]
    num_df <- num_df[, sapply(num_df, is.numeric), drop = FALSE]
    validate(need(ncol(num_df) >= 2, "Select at least two numeric variables for PCA."))
    prcomp(na.omit(num_df), center = isTRUE(input$pca_center), scale. = isTRUE(input$pca_scale))
  })
  
  output$pca_scatter <- renderPlot({
    pca <- pca_result()
    scores <- as.data.frame(pca$x)
    validate(need(ncol(scores) >= 2, "PCA produced fewer than 2 components."))
    ggplot(scores, aes(x = PC1, y = PC2)) +
      geom_point(color = "#42A5F5", alpha = 0.7) +
      theme_minimal() +
      labs(
        title = "PCA Scatter Plot (PC1 vs PC2)",
        x = "PC1",
        y = "PC2"
      )
  })
  
  output$pca_scree <- renderPlot({
    pca <- pca_result()
    var_explained <- pca$sdev^2 / sum(pca$sdev^2)
    df <- data.frame(
      PC = paste0("PC", seq_along(var_explained)),
      Var = var_explained
    )
    ggplot(df, aes(x = PC, y = Var)) +
      geom_bar(stat = "identity", fill = "#26A69A") +
      geom_line(aes(group = 1), color = "#37474F") +
      geom_point(color = "#37474F") +
      theme_minimal() +
      scale_y_continuous(labels = scales::percent) +
      labs(
        title = "Scree Plot (Explained Variance)",
        x = "Principal Component",
        y = "Variance Explained"
      )
  })
}

shinyApp(ui, server)
