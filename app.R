library(shiny)
library(ggplot2)
library(bslib)
library(dplyr)

# XGBoost is only required when the saved model bundle is present.
has_xgboost <- requireNamespace("xgboost", quietly = TRUE)

# ---------------------------------------------------------------------
# THEME
# ---------------------------------------------------------------------
navy      <- "#0B3D62"
uwf_green <- "#146C43"
bg_grey   <- "#F5F7F8"
ink       <- "#22303F"

app_theme <- bs_theme(
  version = 5,

primary = navy,
  success = uwf_green,
  bg = "#FFFFFF",
  fg = ink,
  base_font = font_collection("system-ui", "-apple-system", "Segoe UI", "Helvetica Neue", "Arial", "sans-serif"),
  heading_font = font_collection("system-ui", "-apple-system", "Segoe UI", "Helvetica Neue", "Arial", "sans-serif"),
  "navbar-bg" = navy,
  "border-radius" = "0.6rem",
  "card-border-color" = "#E3E8EC"
) |>
  bs_add_rules(sprintf("
    body { background-color: %s; }
    .navbar-brand { font-weight: 700; letter-spacing: 0.02em; }
    .card { box-shadow: 0 1px 3px rgba(0,0,0,0.06); border: 1px solid #E3E8EC; margin-bottom: 1.25rem; }
    .card-header { background-color: #FBFCFC; font-weight: 600; color: %s; border-bottom: 1px solid #E3E8EC; }
    .section-lede { color: #5A6B7A; max-width: 1000px; }
    .accent-rule { border: none; border-top: 3px solid %s; width: 64px; margin: 0.25rem 0 1.25rem 0; }
    .cohort-toggle .form-check-label { font-weight: 500; }
    .btn-primary { background-color: %s; border-color: %s; }
    .btn-primary:hover { background-color: #082C48; border-color: #082C48; }
    .metric-label { color:#64748B; font-size:0.82rem; text-transform:uppercase; letter-spacing:.04em; }
    .metric-value { color:%s; font-size:1.8rem; font-weight:700; }
    .status-good { color:%s; font-weight:700; }
    .status-warn { color:#A05A00; font-weight:700; }
    footer.app-footer { color: #8A97A3; font-size: 0.85rem; padding: 2rem 0 1rem 0; text-align: center; }
  ", bg_grey, navy, uwf_green, navy, navy, navy, uwf_green))

# ---------------------------------------------------------------------
# HELPERS
# ---------------------------------------------------------------------
section_header <- function(title, lede = NULL) {
  tagList(
    h3(title),
    if (!is.null(lede)) p(class = "section-lede", lede),
    tags$hr(class = "accent-rule")
  )
}

cohort_toggle <- function(input_id) {
  div(
    class = "cohort-toggle",
    radioButtons(
      input_id, "Population view",
      choices = c("Full National Cohort", "Florida", "North Florida (broad region)", "Northwest Florida"),
      selected = "Full National Cohort",
      inline = TRUE
    )
  )
}

# Original cohort toggle for LCA (two choices)
cohort_toggle_lca <- function(input_id) {
  div(class = "cohort-toggle",
      radioButtons(input_id, "Cohort",
                   choices = c("Full National Cohort", "Northwest Florida Cohort"),
                   selected = "Full National Cohort",
                   inline = TRUE)
  )
}

metric_box <- function(label, value, note = NULL) {
  card(
    card_body(
      div(class = "metric-label", label),
      div(class = "metric-value", value),
      if (!is.null(note)) p(class = "text-muted small mb-0", note)
    )
  )
}

placeholder_plot <- function(msg) {
  ggplot() +
    annotate("text", x = 0, y = 0, label = msg, size = 4.5, color = "grey40", lineheight = 1.15) +
    xlim(-1, 1) + ylim(-1, 1) +
    theme_void()
}

safe_rds_plot <- function(path, missing_msg) {
  if (file.exists(path)) readRDS(path) else placeholder_plot(missing_msg)
}

# Five LCA phenotypes from technical report (Table 3)
lca_classes <- list(
  list(pct = "33.2%", title = "High Multidomain Burden", color = navy,
       desc = "Broadest concurrent burden: mental health (72%), obesity (72%), hypertension (95%), diabetes (60%), dyslipidemia (45%), OA (85%), cancer (25%)"),
  list(pct = "21.9%", title = "Hypertension-Dominant", color = uwf_green,
       desc = "Hypertension (100%), moderate OA (35%), low metabolic/mental burden otherwise"),
  list(pct = "19.6%", title = "Obesity-Dominant", color = navy,
       desc = "Obesity (100%), elevated mental health burden (54%), moderate OA (30%)"),
  list(pct = "14.9%", title = "Diabetes-Dominant", color = uwf_green,
       desc = "Diabetes (100%), hypertension (71%), moderate mental health burden (34%)"),
  list(pct = "10.5%", title = "Dyslipidemia/OA-Dominant", color = navy,
       desc = "Dyslipidemia (100%), OA (53%), hypertension (54%), cancer (21%)")
)

lca_value_box <- function(cls) {
  value_box(
    title = cls$title,
    value = cls$pct,
    theme = value_box_theme(bg = cls$color, fg = "white"),
    showcase = NULL,
    full_screen = FALSE
  )
}

team_members <- list(
  list(name = "Achraf Cohen, PhD", role = "Statistics and AI", affiliation = "Machine learning, uncertainty quantification, statistical modeling", email = "acohen@uwf.edu", pic = "team/Cohen.png"),
  list(name = "Karishma Chhabria Unrue, PhD", role = "Public Health", affiliation = "Cancer, metabolic burden, mental health", email = "kchhabria@uwf.edu", pic = "team/Chabbria.png"),
  list(name = "Armaghan Mahmoudian, PhD", role = "Movement Sciences and Health", affiliation = "Osteoarthritis, movement science, physical health", email = "amahmoudian@uwf.edu", pic = "team/Mahmoudian.png"),
  list(name = "Shrishti Sharma", role = "Statistics and Data Science (Student)", affiliation = "Statistical modeling, machine learning", email = "fs56@students.uwf.edu", pic = "team/shrishti.png"),
  list(name = "Emmanuel Paalam", role = "Computer Science and Data Science (Student)", affiliation = "Dashboard development, data engineering", email = "ejp25@students.uwf.edu", pic = "team/emmanuel.png")
)

community_advisors <- list(
  list(name = "Licheng \u201cTony\u201d Lee, M.D., FACC", role = "Community Advisor", affiliation = "Baptist Health", email = "licheng.lee@bhcpns.org", pic = NULL)
)

team_card <- function(member) {
  avatar <- if (!is.null(member$pic) && file.exists(file.path("www", member$pic))) {
    tags$img(src = member$pic, style = "width:120px; height:120px; border-radius:50%; object-fit:cover; margin:0 auto 12px auto; display:block;")
  } else {
    div(
      style = paste0("width:120px; height:120px; border-radius:50%; margin:0 auto 12px auto; background:", navy,
                     "; color:white; display:flex; align-items:center; justify-content:center; font-size:2rem; font-weight:600;"),
      toupper(substr(member$name, 1, 1))
    )
  }
  card(
    class = "text-center",
    card_body(
      avatar,
      h5(member$name),
      p(class = "mb-0", strong(member$role)),
      p(class = "text-muted", member$affiliation),
      if (!is.null(member$email)) p(class = "small", tags$a(href = paste0("mailto:", member$email), member$email))
    )
  )
}

# ---------------------------------------------------------------------
# RESULTS ALREADY ESTABLISHED IN THE TECHNICAL REPORT
# ---------------------------------------------------------------------
lca_indicator <- data.frame(
  Indicator = c("Mental-health burden", "Obesity", "Hypertension", "Diabetes", "Dyslipidemia", "Osteoarthritis", "Cancer"),
  PresentPct = c(51.8, 51.0, 76.5, 35.5, 28.3, 50.1, 15.6)
)

wearable_compare <- data.frame(
  Group = c("Other", "High Multidomain Burden"),
  MedianSteps = c(6351, 5003),
  LowActivity = c(13.3, 23.2),
  HighActivity = c(12.6, 4.2)
)

florida_compare <- data.frame(
  Group = c("Florida", "Other states"),
  N = c(497, 15568),
  HighBurdenPct = c(32.2, 28.4),
  MedianAge = c(63, 65),
  MedianSteps = c(5670, 5982),
  LowActivity = c(17.3, 15.6),
  HighActivity = c(7.69, 9.78),
  StepCV = c(.499, .490)
)

# ---------------------------------------------------------------------
# PLOTS BUILT FROM ESTABLISHED RESULTS
# ---------------------------------------------------------------------
plot_indicator_prevalence <- function() {
  ggplot(lca_indicator, aes(x = reorder(Indicator, PresentPct), y = PresentPct)) +
    geom_col() +
    coord_flip() +
    labs(x = NULL, y = "Participants with indicator (%)") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}

plot_wearable_compare <- function() {
  dat <- bind_rows(
    wearable_compare |> transmute(Group, Measure = "Days <3,000 steps", Value = LowActivity),
    wearable_compare |> transmute(Group, Measure = "Days >=10,000 steps", Value = HighActivity)
  )
  ggplot(dat, aes(x = Group, y = Value, fill = Measure)) +
    geom_col(position = "dodge") +
    labs(x = NULL, y = "Median proportion of days (%)", fill = NULL) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), legend.position = "top")
}

plot_florida_compare <- function() {
  ggplot(florida_compare, aes(x = Group, y = HighBurdenPct)) +
    geom_col() +
    geom_text(aes(label = paste0(HighBurdenPct, "%")), vjust = -0.4, size = 4) +
    ylim(0, 40) +
    labs(x = NULL, y = "High Multidomain Burden (%)") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}

# ---------------------------------------------------------------------
# MODEL BUNDLE
# Expected fields: model (xgb.Booster or raw bytes from xgb.save.raw()),
# columns, factor_levels, qhat_other, qhat_high, threshold.
# ---------------------------------------------------------------------
model_bundle_path <- "data/model/metasense_model_bundle.rds"
model_bundle <- NULL
model_bundle_error <- NULL

if (file.exists(model_bundle_path)) {
  model_bundle <- tryCatch(readRDS(model_bundle_path), error = function(e) NULL)

  if (!is.null(model_bundle) && is.raw(model_bundle)) {
    # The file holds only the raw booster bytes (e.g. saved via
    # saveRDS(xgb.save.raw(xgb_tuned), path)), not the full bundle list.
    # Wrap it so downstream code has a consistent list shape, but the
    # columns/factor_levels/thresholds metadata is unrecoverable from here.
    model_bundle <- list(model = model_bundle)
    model_bundle_error <- paste(
      "The saved file only contains the raw XGBoost booster bytes \u2014 the columns,",
      "factor_levels, and conformal thresholds metadata is missing, so predictions cannot",
      "be aligned to the training features. Re-save the full bundle list, e.g.:",
      "model_bundle$model <- xgboost::xgb.save.raw(xgb_tuned); saveRDS(model_bundle, \"data/model/metasense_model_bundle.rds\")."
    )
  }

  if (!is.null(model_bundle) && is.list(model_bundle) && has_xgboost) {
    # xgb.Booster external pointers do not survive plain readRDS() across
    # sessions. If the bundle stored raw bytes (xgb.save.raw()), rebuild the
    # booster now. If it stored the live Booster object directly, its
    # pointer may have failed to unserialize (an empty list/NULL result).
    booster <- model_bundle$model
    if (is.raw(booster)) {
      # xgb.load.raw() returns class "xgb.Booster" in newer xgboost, or
      # "xgb.Booster.handle" in older versions (e.g. 1.7.x); predict() works
      # on either, so both are accepted below.
      model_bundle$model <- tryCatch(xgboost::xgb.load.raw(booster), error = function(e) NULL)
    } else if (!inherits(booster, c("xgb.Booster", "xgb.Booster.handle"))) {
      model_bundle$model <- NULL
      if (is.null(model_bundle_error)) {
        model_bundle_error <- paste(
          "The saved model bundle's xgb.Booster could not be restored",
          "(readRDS cannot reliably unserialize xgboost's raw pointer across sessions).",
          "Re-save the bundle with model = xgboost::xgb.save.raw(xgb_tuned) so it can be reloaded with xgb.load.raw()."
        )
      }
    }
  } else if (!is.null(model_bundle) && !is.list(model_bundle)) {
    model_bundle <- NULL
    model_bundle_error <- "The saved model bundle file is not in a recognized format (expected a list or raw booster bytes)."
  }
}

model_ready <- !is.null(model_bundle) && has_xgboost &&
  inherits(model_bundle$model, c("xgb.Booster", "xgb.Booster.handle")) &&
  !is.null(model_bundle$columns)

# ---------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------
ui <- page_navbar(
  title = "MetaSense AI",
  theme = app_theme,
  fillable = FALSE,
  bg = navy,

  # -----------------------------------------------------------------
  # ABOUT (from metasense_revised)
  # -----------------------------------------------------------------
  nav_panel("About",
    div(class = "container-fluid py-4",
      section_header(
        "MetaSense: Trustworthy Wearable Intelligence for Multimorbidity Stratification",
        "A prototype that discovers clinically meaningful multimorbidity patterns and uses wearable activity plus demographics to estimate High Multidomain Burden with explicit uncertainty."
      ),
      layout_columns(
        col_widths = c(8, 4),
        card(
          card_header("What MetaSense Does"),
          card_body(
            p("MetaSense separates phenotype discovery from prediction. Clinical indicators first define five multimorbidity phenotypes using latent class analysis. A separate machine-learning model then uses wearable-derived activity and demographic information to estimate the probability that a participant belongs to the High Multidomain Burden phenotype."),
            p("The model does not use the diagnoses that define the latent classes as predictors. This prevents target leakage and makes the wearable model a true screening and stratification prototype rather than a diagnostic tool."),
            p("Class-conditional conformal prediction adds a trust layer so the system can return a lower-burden pattern supported, an elevated-burden signal, or an uncertain result that recommends additional information rather than forcing a classification.")
          )
        ),
        card(
          card_header("Current Evidence Base"),
          card_body(
            p(strong("Phenotype discovery:"), " N = 218,619"),
            p(strong("Wearable prediction:"), " N = 16,065"),
            p(strong("Florida wearable cohort:"), " N = 497"),
            p(strong("Northwest Florida (ZIP3 324/325):"), " N = 5"),
            div(class = "alert alert-warning small",
                "Northwest Florida is too sparse for reliable local estimates in the current All of Us wearable cohort. Florida-wide results are shown as the current regional benchmark.")
          )
        )
      ),
      card(
        card_header("How MetaSense Works"),
        card_body(
          layout_columns(
            col_widths = c(3, 3, 3, 3),
            card(card_body(h5("1. Discover"), p("Identify five multimorbidity phenotypes from mental-health, metabolic, OA, and cancer indicators."))),
            card(card_body(h5("2. Prioritize"), p("Focus prediction on High Multidomain Burden, the broadest and most clinically actionable phenotype."))),
            card(card_body(h5("3. Predict"), p("Use demographics and seven wearable activity features to estimate High Multidomain Burden probability."))),
            card(card_body(h5("4. Trust + Translate"), p("Apply calibration and conformal prediction, then translate findings to Florida and future Northwest Florida validation.")))
          )
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Inputs Used by the Final ML Model"),
          card_body(
            tags$ul(
              tags$li(strong("Demographics: "), "age, sex at birth, race/ethnicity, education, marital status, broad geographic region."),
              tags$li(strong("Wearable activity: "), "mean steps, step CV, proportion of days <3,000 steps, proportion of days >=10,000 steps, weekend-weekday difference, valid activity days, observation density."),
              tags$li(strong("Not used as predictors: "), "mental-health burden, obesity, hypertension, diabetes, dyslipidemia, OA, cancer, or ZIP code.")
            )
          )
        ),
        card(
          card_header("For Health Leadership"),
          card_body(
            p(strong("Interpretation:"), " MetaSense is a screening and population-stratification tool, not a diagnostic classifier."),
            p(strong("Best current use:"), " identify groups with meaningfully different multimorbidity burden and flag when wearable information is insufficient for a definitive signal."),
            p(strong("Local translation:"), " Florida-wide results are available now; Northwest Florida requires a larger local validation dataset.")
          )
        )
      )
    )
  ),

  # -----------------------------------------------------------------
  # DESCRIPTIVE DATA (from metasense_revised)
  # -----------------------------------------------------------------
  nav_panel("Descriptive Data",
    div(class = "container-fluid py-4",
      section_header(
        "Population Overview",
        "Descriptive summaries. National results describe the full MetaSense cohorts; Florida is an exploratory regional benchmark; Northwest Florida is explicitly flagged when the data are too sparse."
      ),
      card(card_body(cohort_toggle("desc_region_toggle"))),
      uiOutput("desc_summary_boxes"),
      uiOutput("desc_message"),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Multimorbidity Burden"), card_body(plotOutput("desc_plot1", height = 360))),
        card(card_header("Wearable Activity"), card_body(plotOutput("desc_plot2", height = 360)))
      ),
      card(
        card_header("Data Needed for North / Northwest Florida Expansion"),
        card_body(
          p("To populate local descriptive panels beyond the current Florida benchmark, create a participant-level file using the same wearable cohort definitions and add:"),
          tags$ul(
            tags$li("ZIP3 and broad location_region"),
            tags$li("age, sex at birth, race/ethnicity, education, marital status"),
            tags$li("LCA profile / High Multidomain Burden indicator"),
            tags$li("activity_days, steps_mean, steps_cv, pct_lt_3000, pct_ge_10000, weekend_difference, wear_density")
          ),
          p(class = "small text-muted", "")
        )
      )
    )
  ),

  # -----------------------------------------------------------------
  # LCA SUBGROUPS (from app.R)
  # -----------------------------------------------------------------
  nav_panel("LCA Subgroups",
    div(class = "container-fluid py-4",
      section_header(
        "Latent Class Analysis of Mental-Metabolic Chronic Disease Phenotypes",
        "LCA identified five clinically interpretable multimorbidity phenotypes from 218,619 participants using seven binary indicators: mental-health burden, obesity, hypertension, diabetes, dyslipidemia, osteoarthritis, and cancer."
      ),

      card(card_header("Five Mental-Metabolic Phenotypes (N = 218,619)"),
        card_body(
          p(class = "section-lede", "The five-class solution was selected based on entropy (0.769), mean posterior probability (0.849), and clinical interpretability. The High Multidomain Burden phenotype represents the broadest concurrent burden and serves as the primary prediction target."),
          layout_columns(col_widths = c(12, 6, 6, 6, 6), !!!lapply(lca_classes, lca_value_box))
        )
      ),

      card(card_header("LCA Indicator Distribution"),
        card_body(
          p("Seven binary indicators used to define latent phenotypes:"),
          tags$table(class = "table table-sm table-striped",
            tags$thead(tags$tr(tags$th("Indicator"), tags$th("Present, n (%)"), tags$th("Absent, n (%)"))),
            tags$tbody(
              tags$tr(tags$td("Mental-health burden"), tags$td("113,193 (51.8%)"), tags$td("105,426 (48.2%)")),
              tags$tr(tags$td("Obesity"), tags$td("111,545 (51.0%)"), tags$td("107,074 (49.0%)")),
              tags$tr(tags$td("Hypertension"), tags$td("167,351 (76.5%)"), tags$td("51,268 (23.5%)")),
              tags$tr(tags$td("Diabetes"), tags$td("77,666 (35.5%)"), tags$td("140,953 (64.5%)")),
              tags$tr(tags$td("Dyslipidemia"), tags$td("61,888 (28.3%)"), tags$td("156,731 (71.7%)")),
              tags$tr(tags$td("Osteoarthritis"), tags$td("109,539 (50.1%)"), tags$td("109,080 (49.9%)")),
              tags$tr(tags$td("Cancer (top 10)"), tags$td("34,183 (15.6%)"), tags$td("184,436 (84.4%)"))
            )
          )
        )
      ),

      card(card_header("LCA Phenotype Profiles"),
        card_body(
          div(class = "text-center",
              imageOutput("lca_figure", height = "auto")
          )
        )
      ),

      card(
        card_header("Sociodemographic & Geographic Breakdown"),
        card_body(
          p(class = "section-lede", "Compare LCA subgroup composition between the general MetS population and those located in Northwest Florida (ZIP codes 324**, 325**)."),
          cohort_toggle_lca("region_toggle"),
          plotOutput("geographic_distribution")
        )
      ),

      card(card_header("Class Composition by Demographic"),
        card_body(
          layout_columns(col_widths = c(6, 6), plotOutput("age_plot"), plotOutput("sex_plot")),
          layout_columns(col_widths = c(6, 6), plotOutput("race_plot"), plotOutput("education_plot")),
          plotOutput("marital_plot")
        )
      )
    )
  ),

  # -----------------------------------------------------------------
  # RISK PREDICTION (from app.R)
  # -----------------------------------------------------------------
  nav_panel("Risk Prediction",
    div(class = "container-fluid py-4",
      section_header(
        "High Multidomain Burden Risk Prediction",
        "MetaSense predicts probability of High Multidomain Burden phenotype membership using wearable-derived activity and demographic features. Model AUC = 0.691 on held-out test data (N = 16,065 wearable cohort)."
      ),

      # Model performance summary card
      card(card_header("Model Performance Summary"),
        card_body(
          layout_columns(
            col_widths = c(3, 3, 3, 3),
            value_box(title = "Test ROC AUC", value = "0.691", theme = value_box_theme(bg = navy, fg = "white")),
            value_box(title = "Sensitivity", value = "71.5%", theme = value_box_theme(bg = uwf_green, fg = "white")),
            value_box(title = "Specificity", value = "55.1%", theme = value_box_theme(bg = navy, fg = "white")),
            value_box(title = "NPV", value = "82.9%", theme = value_box_theme(bg = uwf_green, fg = "white"))
          ),
          p(class = "text-muted small", "Operating threshold = 0.266 (Youden's index). MetaSense is designed as a screening/stratification tool, not a diagnostic classifier. Positive results should prompt additional assessment.")
        )
      ),

      layout_sidebar(
        sidebar = sidebar(
          width = 400,
          title = "Input Features",
          open = "always",

          h6("Demographics"),
          numericInput("ml_age", "Age (years)", value = 65, min = 18, max = 100),
          selectInput("ml_sex", "Sex at Birth", choices = c("Female", "Male")),
          selectInput("ml_race", "Race/Ethnicity", choices = c("White", "Black or African American", "Hispanic", "Other")),
          selectInput("ml_education", "Education", choices = c("College graduate or higher", "Some college", "High school or less")),
          selectInput("ml_marital", "Marital Status", choices = c("Married/Partnered", "Never Married", "Previously Married")),
          selectInput("ml_region", "Broad Geographic Region", choices = c(
            "Pacific/West", "Central/South Central", "Upper Midwest/Northern Plains",
            "Mid-Atlantic/Southeast", "Great Lakes/Ohio Valley", "Eastern ZIP Zone 1",
            "Mountain/Southwest", "Eastern ZIP Zone 0", "North Florida"
          )),

          tags$hr(),
          h6("Fitbit Wearable Activity Features"),
          numericInput("ml_steps", "Mean Daily Steps", value = 5968, min = 0, max = 30000, step = 100),
          numericInput("ml_step_cv", "Daily Step Variability (CV)", value = 0.49, min = 0, max = 2, step = 0.01),
          numericInput("ml_low_days", "% Days < 3,000 Steps", value = 15.7, min = 0, max = 100, step = 0.1),
          numericInput("ml_high_days", "% Days \u2265 10,000 Steps", value = 9.7, min = 0, max = 100, step = 0.1),
          numericInput("ml_wknd_diff", "Weekend\u2013Weekday Step Difference", value = -208, min = -5000, max = 5000, step = 10),
          numericInput("ml_activity_days", "Valid Fitbit Activity Days", value = 353, min = 30, max = 2000),
          numericInput("ml_wear_density", "Wearable Observation Density (0-1)", value = 0.80, min = 0, max = 1, step = 0.01),

          tags$hr(),
          actionButton("ml_predict", "Estimate Risk", class = "btn-primary w-100")
        ),

        div(
          uiOutput("ml_model_status"),
          card(
            card_header("Model Output"),
            card_body(
              uiOutput("ml_output")
            )
          )
        )
      )
    )
  ),

  # -----------------------------------------------------------------
  # TEAM (from metasense_revised)
  # -----------------------------------------------------------------
  nav_panel("Team",
    div(class = "container-fluid py-4",
      section_header("Project Team", "Researchers and collaborators behind MetaSense at the University of West Florida."),
      layout_columns(col_widths = c(4, 4, 4), !!!lapply(team_members, team_card)),
      br(), h4("Community Advisor"), tags$hr(class = "accent-rule"),
      layout_columns(col_widths = c(4), !!!lapply(community_advisors, team_card))
    )
  ),

  nav_spacer(),
  nav_item(tags$span(class = "navbar-text text-white-50 small", "University of West Florida"))
)

ui <- tagList(
  ui,
  tags$footer(class = "app-footer", "MetaSense AI \u00b7 University of West Florida")
)

# ---------------------------------------------------------------------
# SERVER
# ---------------------------------------------------------------------
server <- function(input, output, session) {

  # ---------------------------
  # DESCRIPTIVE DATA (from metasense_revised)
  # ---------------------------
  output$desc_summary_boxes <- renderUI({
    view <- input$desc_region_toggle
    if (view == "Full National Cohort") {
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        metric_box("LCA discovery cohort", "218,619", "Clinical-indicator cohort"),
        metric_box("Wearable ML cohort", "16,065", "At least 30 valid activity days"),
        metric_box("High burden", "28.5%", "4,581 of 16,065 wearable participants"),
        metric_box("Median daily steps", "5,968", "Wearable prediction cohort")
      )
    } else if (view == "Florida") {
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        metric_box("Florida wearable N", "497"),
        metric_box("High burden", "32.2%", "160 of 497"),
        metric_box("Median daily steps", "5,670"),
        metric_box("Low-activity days", "17.3%", "Median % of days <3,000 steps")
      )
    } else if (view == "North Florida (broad region)") {
      layout_columns(
        col_widths = c(4, 4, 4),
        metric_box("North Florida N", "161", "Broad location_region category"),
        metric_box("High burden", "37.9%", "61 of 161"),
        metric_box("Status", "Exploratory", "Not equivalent to ZIP3-defined Northwest Florida")
      )
    } else {
      layout_columns(
        col_widths = c(4, 4, 4),
        metric_box("Northwest Florida N", "5", "ZIP3 324/325"),
        metric_box("ZIP3 324", "2"),
        metric_box("ZIP3 325", "3")
      )
    }
  })

  output$desc_message <- renderUI({
    view <- input$desc_region_toggle
    if (view == "Florida") {
      div(class = "alert alert-info",
          "Florida is presented as an exploratory regional benchmark. High Multidomain Burden was 32.2% in Florida versus 28.4% outside Florida; these are descriptive comparisons and not evidence of a geographic disparity.")
    } else if (view == "North Florida (broad region)") {
      div(class = "alert alert-info",
          "The broad All of Us location_region category labeled 'North Florida' includes 161 wearable-cohort participants, with 37.9% classified as High Multidomain Burden. This category is broader than Northwest Florida and should not be presented as the Pensacola/Panhandle service area. Additional participant-level summaries are still needed for age, demographics, and wearable activity.")
    } else if (view == "Northwest Florida") {
      div(class = "alert alert-warning",
          strong("Local data are too sparse for reliable estimates. "),
          "Only five wearable-cohort participants were identified in ZIP3 324/325. No Northwest Florida prevalence, demographic, or model-performance estimates are displayed. A larger regional dataset is needed for validation.")
    } else {
      NULL
    }
  })

  output$desc_plot1 <- renderPlot({
    view <- input$desc_region_toggle
    if (view == "Full National Cohort") {
      plot_indicator_prevalence()
    } else if (view == "Florida") {
      plot_florida_compare()
    } else if (view == "North Florida (broad region)") {
      ggplot(data.frame(Group = c("North Florida", "Full wearable cohort"), Value = c(37.9, 28.5)),
             aes(x = Group, y = Value)) +
        geom_col() +
        geom_text(aes(label = paste0(Value, "%")), vjust = -0.4) +
        ylim(0, 45) +
        labs(x = NULL, y = "High Multidomain Burden (%)") +
        theme_minimal(base_size = 12) +
        theme(panel.grid.minor = element_blank())
    } else {
      placeholder_plot("Northwest Florida: n = 5\nNo stable local burden estimate is reported.")
    }
  })

  output$desc_plot2 <- renderPlot({
    view <- input$desc_region_toggle
    if (view == "Full National Cohort") {
      plot_wearable_compare()
    } else if (view == "Florida") {
      dat <- bind_rows(
        florida_compare |> transmute(Group, Measure = "Days <3,000 steps", Value = LowActivity),
        florida_compare |> transmute(Group, Measure = "Days >=10,000 steps", Value = HighActivity)
      )
      ggplot(dat, aes(x = Group, y = Value, fill = Measure)) +
        geom_col(position = "dodge") +
        labs(x = NULL, y = "Median proportion of days (%)", fill = NULL) +
        theme_minimal(base_size = 12) +
        theme(panel.grid.minor = element_blank(), legend.position = "top")
    } else if (view == "North Florida (broad region)") {
      placeholder_plot("North Florida wearable summaries are not yet available.\nPull the same activity features used for the national and Florida cohorts.")
    } else {
      placeholder_plot("Add a larger Northwest Florida participant-level wearable extract\nbefore displaying local activity distributions.")
    }
  })

  # ---------------------------
  # LCA SUBGROUPS (from app.R)
  # ---------------------------
  output$lca_figure <- renderImage({
    list(
      src = "data/lca/LCA.png",
      width = "60%",
      alt = "LCA Mental-Metabolic Phenotype Profiles"
    )
  }, deleteFile = FALSE)

  is_national_lca <- reactive(input$region_toggle == "Full National Cohort")

  output$geographic_distribution <- renderPlot({
    if (is_national_lca()) {
      safe_rds_plot("data/lca/class_dist_full.rds", "Class distribution plot not found.\nAdd data/lca/class_dist_full.rds")
    } else {
      safe_rds_plot("data/lca/class_dist_panhandle.rds", "Northwest Florida class distribution not found.\nAdd data/lca/class_dist_panhandle.rds")
    }
  })

  output$age_plot <- renderPlot({
    if (is_national_lca()) {
      safe_rds_plot("data/lca/class_age.rds", "Age-by-class plot not found.\nAdd data/lca/class_age.rds")
    } else {
      safe_rds_plot("data/lca/class_age_panhandle.rds", "Northwest Florida age-by-class plot not found.\nAdd data/lca/class_age_panhandle.rds")
    }
  })

  output$sex_plot <- renderPlot({
    if (is_national_lca()) {
      safe_rds_plot("data/lca/class_sex.rds", "Sex-by-class plot not found.\nAdd data/lca/class_sex.rds")
    } else {
      safe_rds_plot("data/lca/class_sex_panhandle.rds", "Northwest Florida sex-by-class plot not found.\nAdd data/lca/class_sex_panhandle.rds")
    }
  })

  output$race_plot <- renderPlot({
    if (is_national_lca()) {
      safe_rds_plot("data/lca/class_race.rds", "Race/ethnicity-by-class plot not found.\nAdd data/lca/class_race.rds")
    } else {
      safe_rds_plot("data/lca/class_race_panhandle.rds", "Northwest Florida race/ethnicity-by-class plot not found.\nAdd data/lca/class_race_panhandle.rds")
    }
  })

  output$education_plot <- renderPlot({
    if (is_national_lca()) {
      safe_rds_plot("data/lca/class_education.rds", "Education-by-class plot not found.\nAdd data/lca/class_education.rds")
    } else {
      safe_rds_plot("data/lca/class_edu_panhandle.rds", "Northwest Florida education-by-class plot not found.\nAdd data/lca/class_edu_panhandle.rds")
    }
  })

  output$marital_plot <- renderPlot({
    if (is_national_lca()) {
      safe_rds_plot("data/lca/class_marital.rds", "Marital-status-by-class plot not found.\nAdd data/lca/class_marital.rds")
    } else {
      safe_rds_plot("data/lca/class_mar_panhandle.rds", "Northwest Florida marital-status-by-class plot not found.\nAdd data/lca/class_mar_panhandle.rds")
    }
  })

  # ---------------------------
  # RISK PREDICTION (model connected from app_metasense_revised)
  # ---------------------------
  output$ml_model_status <- renderUI({
    if (!has_xgboost) {
      div(class = "alert alert-danger",
          "The R package 'xgboost' is not installed in this session, so the saved model cannot be used.")
    } else if (!is.null(model_bundle_error)) {
      div(class = "alert alert-danger", strong("Model file could not be loaded. "), model_bundle_error)
    } else if (model_ready) {
      div(class = "alert alert-success",
          strong("MetaSense model connected. "),
          "Predictions use the saved XGBoost model and class-conditional conformal thresholds.")
    } else {
      div(class = "alert alert-warning",
          strong("Model file not yet connected. "),
          "Add data/model/metasense_model_bundle.rds. The interface is already configured for the final model predictors and conformal output.")
    }
  })

  observeEvent(input$ml_predict, {
    if (!model_ready) {
      output$ml_output <- renderUI({
        div(class = "alert alert-secondary",
            "Prediction unavailable until a valid model bundle is loaded (see status message above).")
      })
      return()
    }

    # Build exactly the variables used by the final model.
    newdata <- data.frame(
      age = input$ml_age,
      sex_at_birth = input$ml_sex,
      race_ethnicity = input$ml_race,
      education = input$ml_education,
      marital_status = input$ml_marital,
      location_region = input$ml_region,
      steps_mean = input$ml_steps,
      steps_cv = input$ml_step_cv,
      pct_lt_3000 = input$ml_low_days / 100,
      pct_ge_10000 = input$ml_high_days / 100,
      weekend_difference = input$ml_wknd_diff,
      activity_days = input$ml_activity_days,
      wear_density = input$ml_wear_density,
      stringsAsFactors = FALSE
    )

    # Apply saved factor levels so dummy columns line up with training.
    if (!is.null(model_bundle$factor_levels)) {
      for (v in names(model_bundle$factor_levels)) {
        if (v %in% names(newdata)) {
          newdata[[v]] <- factor(newdata[[v]], levels = model_bundle$factor_levels[[v]])
        }
      }
    }

    mm <- model.matrix(~ . - 1, data = newdata)
    needed <- model_bundle$columns
    if (is.null(needed)) {
      output$ml_output <- renderUI(div(class = "alert alert-danger", "Model bundle is missing the training matrix column names."))
      return()
    }

    missing_cols <- setdiff(needed, colnames(mm))
    if (length(missing_cols) > 0) {
      zeros <- matrix(0, nrow = 1, ncol = length(missing_cols), dimnames = list(NULL, missing_cols))
      mm <- cbind(mm, zeros)
    }
    mm <- mm[, needed, drop = FALSE]

    p_high <- as.numeric(predict(model_bundle$model, xgboost::xgb.DMatrix(mm)))

    q_other <- if (!is.null(model_bundle$qhat_other)) model_bundle$qhat_other else 0.4618295
    q_high  <- if (!is.null(model_bundle$qhat_high))  model_bundle$qhat_high  else 0.8234986
    threshold <- if (!is.null(model_bundle$threshold)) model_bundle$threshold else 0.266

    include_other <- p_high <= q_other
    include_high  <- (1 - p_high) <= q_high

    conformal_set <- if (include_high && include_other) {
      "Uncertain \u2014 additional information recommended"
    } else if (include_high) {
      "Elevated-burden signal"
    } else if (include_other) {
      "Lower-burden pattern supported"
    } else {
      "No class met the conformal criterion"
    }

    threshold_signal <- if (p_high >= threshold) "Above screening threshold" else "Below screening threshold"

    output$ml_output <- renderUI({
      tagList(
        layout_columns(
          col_widths = c(4, 4, 4),
          metric_box("Predicted High Burden", paste0(round(100 * p_high, 1), "%"), "XGBoost probability"),
          metric_box("Screening signal", threshold_signal, paste0("Operating threshold = ", round(threshold, 3))),
          metric_box("Conformal interpretation", conformal_set, "90% class-conditional framework")
        ),
        div(
          if (conformal_set == "Elevated-burden signal") {
            div(class = "alert alert-warning", "The wearable and demographic pattern supports an elevated-burden signal. This is not confirmation of disease or phenotype membership and should prompt additional assessment rather than automated action.")
          } else if (conformal_set == "Lower-burden pattern supported") {
            div(class = "alert alert-success", "The available wearable and demographic information supports the lower-burden pattern. This does not rule out individual chronic conditions.")
          } else if (grepl("Uncertain", conformal_set)) {
            div(class = "alert alert-info", "The model intentionally retains both possible classes. Additional clinical or contextual information is recommended rather than forcing a binary classification.")
          } else {
            div(class = "alert alert-secondary", "The current input falls outside both class-conditional conformal criteria. Review model preprocessing and input ranges.")
          },
          p(class = "small text-muted mb-0", "MetaSense estimates current High Multidomain Burden phenotype membership. It does not predict future disease onset and is not intended to diagnose or replace clinician judgment.")
        )
      )
    })
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)
