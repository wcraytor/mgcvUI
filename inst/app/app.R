library(mgcvUI)

# Load Roboto Condensed for R graphics (ggplot2 + base R)
if (requireNamespace("sysfonts", quietly = TRUE) &&
    requireNamespace("showtext", quietly = TRUE)) {
  sysfonts::font_add_google("Roboto Condensed", "Roboto Condensed")
  showtext::showtext_auto()
  ggplot2::theme_set(
    ggplot2::theme_minimal(base_family = "Roboto Condensed")
  )
} else {
  ggplot2::theme_set(ggplot2::theme_minimal(base_family = "sans"))
}

# Enable thematic so ggplot2 auto-adapts to current theme
if (requireNamespace("thematic", quietly = TRUE)) {
  thematic::thematic_shiny()
}

# Font family helper
mgcv_font_family_ <- function() {
  if (requireNamespace("sysfonts", quietly = TRUE) &&
      "Roboto Condensed" %in% sysfonts::font_families()) {
    "Roboto Condensed"
  } else {
    "sans"
  }
}

# Nord Light theme
nord_light <- bslib::bs_theme(
  version = 5,
  bg = "#eceff4",
  fg = "#2e3440",

  primary = "#5e81ac",
  secondary = "#81a1c1",
  success = "#a3be8c",
  info = "#88c0d0",
  warning = "#ebcb8b",
  danger = "#bf616a",
  base_font = bslib::font_google("Roboto Condensed")
)

# Nord Dark theme
nord_dark <- bslib::bs_theme(
  version = 5,

  bg = "#2e3440",
  fg = "#d8dee9",
  primary = "#88c0d0",
  secondary = "#81a1c1",
  success = "#a3be8c",
  info = "#5e81ac",
  warning = "#ebcb8b",
  danger = "#bf616a",
  base_font = bslib::font_google("Roboto Condensed")
) |>
  bslib::bs_add_rules("
    .navbar { background-color: #242933 !important; }
    .card   { background-color: #3b4252 !important; border-color: #434c5e; }
  ")

ui <- fluidPage(
  theme = nord_light,
  withMathJax(),
  tags$head(
    tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
    tags$style(HTML("
    /* --- Full-width container (match earthUI) --- */
    .container-fluid { max-width: 100% !important; padding: 0 15px; }

    /* --- Initial window sizing --- */
    body { min-width: 1400px; }

    /* --- Variable table --- */
    .mgcv-var-row {
      display: flex; align-items: center;
      padding: 3px 0; border-bottom: 1px solid var(--bs-border-color);
    }
    .mgcv-var-header {
      font-weight: bold; border-bottom: 2px solid var(--bs-border-color);
      background: var(--bs-tertiary-bg); padding: 6px 0;
      position: sticky; top: 0; z-index: 2;
    }
    .mgcv-cell-inc    { width: 35px; text-align: center; }
    .mgcv-cell-name   { flex: 1; min-width: 80px; overflow: hidden;
                        text-overflow: ellipsis; white-space: nowrap;
                        padding: 0 6px;
                        font-family: 'Roboto Condensed', monospace;
                        font-size: 0.85em; }
    .mgcv-cell-type   { width: 85px; }
    .mgcv-cell-na     { width: 75px; text-align: right; padding-right: 4px;
                        font-size: 0.75em; white-space: nowrap;
                        font-family: 'Roboto Condensed', monospace; }
    .mgcv-cell-factor { width: 45px; text-align: center; padding-right: 12px; }
    .mgcv-cell-linear { width: 45px; text-align: center; padding-right: 0; }
    .mgcv-cell-special { width: 110px; }
    .mgcv-var-row select {
      width: 100%; padding: 1px 2px; font-size: 0.8em;
      border: 1px solid var(--bs-border-color); border-radius: 3px;
      appearance: auto; -webkit-appearance: auto;
      background-color: var(--bs-body-bg); color: var(--bs-body-color);
    }
    .mgcv-var-row input[type='checkbox'] {
      width: 15px; height: 15px; cursor: pointer;
    }

    /* --- DataTables --- */
    .dataTable td, .dataTable th { padding: 4px 8px !important; }
    .dataTables_wrapper { font-size: 0.9em; overflow-x: auto; }

    /* DT DataTables: adapt to current theme */
    .dataTables_wrapper { color: var(--bs-body-color); }
    table.dataTable { color: var(--bs-body-color); border-color: var(--bs-border-color); }
    table.dataTable thead th,
    table.dataTable thead td {
      background-color: var(--bs-tertiary-bg) !important;
      color: var(--bs-body-color) !important;
      border-color: var(--bs-border-color) !important;
    }
    table.dataTable tbody td {
      background-color: var(--bs-body-bg);
      color: var(--bs-body-color);
      border-color: var(--bs-border-color);
    }
    table.dataTable tbody tr:hover td { background-color: var(--bs-tertiary-bg); }
    table.dataTable tbody tr.odd td { background-color: var(--bs-secondary-bg); }
    .dataTables_info, .dataTables_length, .dataTables_filter, .dataTables_paginate {
      color: var(--bs-body-color);
    }
    .dataTables_paginate .paginate_button {
      color: var(--bs-body-color) !important;
      background: var(--bs-body-bg); border-color: var(--bs-border-color);
    }
    .dataTables_paginate .paginate_button.current {
      color: var(--bs-body-color) !important;
      background: var(--bs-tertiary-bg) !important;
      border-color: var(--bs-border-color) !important;
    }
    .dataTables_filter input, .dataTables_length select {
      background-color: var(--bs-body-bg);
      color: var(--bs-body-color);
      border-color: var(--bs-border-color);
    }

    /* --- Collapsible sections --- */
    .mgcv-section > summary { cursor: pointer; list-style: none; }
    .mgcv-section > summary::-webkit-details-marker { display: none; }
    .mgcv-section > summary h4::before {
      content: '\\25B6\\00a0\\00a0\\00a0';
      font-size: 0.7em;
      transition: transform 0.2s;
      display: inline-block;
    }
    .mgcv-section[open] > summary h4::before { transform: rotate(90deg); }

    /* --- Interaction matrix: earth-sourced cells --- */
    .mgcv-int-earth-cell {
      background-color: rgba(235, 203, 139, 0.35);
    }
    .mgcv-int-earth {
      cursor: not-allowed;
      opacity: 0.8;
    }
    [data-mgcv-theme='dark'] .mgcv-int-earth-cell {
      background-color: rgba(235, 203, 139, 0.2);
    }

    /* --- Top navbar --- */
    .mgcv-navbar { display: flex; align-items: center; padding: 10px 15px; gap: 8px; flex-wrap: wrap; }
    .mgcv-navbar .mgcv-brand { font-size: 1.3em; font-weight: bold; margin-right: 8px; white-space: nowrap; margin: 0; }
    .mgcv-navbar .mgcv-brand small { font-size: 0.55em; color: var(--bs-secondary-color); font-weight: normal; }
    .mgcv-navbar .mgcv-brand img { height: 26px; margin-right: 6px; vertical-align: middle; }
    .mgcv-navbar .dropdown { position: relative; }
    .mgcv-navbar .mgcv-menu-btn { background: none; border: 1px solid var(--bs-border-color); color: var(--bs-body-color); font-size: 0.9em; padding: 6px 12px; cursor: pointer; border-radius: 4px; }
    .mgcv-navbar .mgcv-menu-btn:hover { background: var(--bs-tertiary-bg); }
    .mgcv-navbar .mgcv-dropdown-menu { display: none; position: absolute; top: 100%; left: 0; background: var(--bs-body-bg); border: 1px solid var(--bs-border-color); border-radius: 6px; padding: 12px 16px; min-width: 280px; z-index: 10001; box-shadow: 0 4px 16px rgba(0,0,0,0.2); }
    .mgcv-navbar .dropdown.open .mgcv-dropdown-menu { display: block; }
    .mgcv-navbar .mgcv-spacer { flex: 1; }
    #mgcv-theme-toggle {
      width: 38px; height: 38px; border-radius: 50%;
      border: 2px solid var(--bs-border-color);
      background: var(--bs-body-bg); color: var(--bs-body-color);
      font-size: 18px; cursor: pointer;
      display: flex; align-items: center; justify-content: center;
      box-shadow: 0 2px 6px rgba(0,0,0,0.15); transition: all 0.3s;
    }
    #mgcv-theme-toggle:hover { box-shadow: 0 2px 10px rgba(0,0,0,0.25); }

    /* --- Dark mode --- */
    [data-mgcv-theme='dark'] .mgcv-var-header { background: #3b4252; }
    [data-mgcv-theme='dark'] .mgcv-var-row { border-color: #434c5e; }
    [data-mgcv-theme='dark'] .mgcv-var-row select {
      background: #3b4252 !important; color: #d8dee9 !important;
      border-color: #434c5e !important;
    }
    [data-mgcv-theme='dark'] details > summary { color: #d8dee9 !important; }
    [data-mgcv-theme='dark'] .nav-tabs .nav-link.active {
      color: #d8dee9 !important; background-color: #2e3440 !important;
      border-color: #434c5e #434c5e #2e3440 !important;
    }
    [data-mgcv-theme='dark'] .nav-tabs .nav-link { color: #81a1c1; }
    [data-mgcv-theme='dark'] .nav-tabs .nav-link:hover {
      color: #d8dee9; border-color: #434c5e;
    }

    /* --- Parameter help icons (match earthUI) --- */
    .mgcv-param-help {
      position: absolute; top: 0; right: 0;
      width: 18px; height: 18px; border-radius: 50%;
      background: #88c0d0; color: #fff;
      font-size: 11px; font-weight: bold;
      text-align: center; line-height: 18px;
      cursor: pointer; z-index: 10;
    }
    .mgcv-param-help:hover { background: #5e81ac; }

    /* --- shinyFiles browse: mimic native fileInput appearance --- */
    .input-group .shinyFiles {
      width: auto !important;
      flex: 0 0 auto;
      border-radius: 0.375rem 0 0 0.375rem !important;
      font-size: 0.875em;
      background-color: var(--bs-tertiary-bg) !important;
      border: 1px solid var(--bs-border-color) !important;
      color: var(--bs-secondary-color) !important;
    }
    .input-group .shinyFiles:hover {
      background-color: var(--bs-secondary-bg) !important;
    }
    .input-group .form-control {
      background-color: var(--bs-body-bg);
      color: var(--bs-secondary-color);
      border-color: var(--bs-border-color);
      font-size: 0.875em;
    }
  "))),

  # --- Top Menu Bar ---
  tags$nav(class = "mgcv-navbar",
    tags$h2(class = "mgcv-brand",
      tags$img(src = "logo.png"),
      "mgcvUI",
      tags$small(" - GAM Builder"),
      style = "margin: 0;"
    ),
    tags$div(class = "dropdown", id = "mgcv-settings-dropdown",
      tags$button(class = "mgcv-menu-btn",
                  onclick = "mgcvToggleDropdown('mgcv-settings-dropdown')",
                  HTML("&#9881; Settings")),
      tags$div(class = "mgcv-dropdown-menu",
        selectInput("locale_country", "Country",
                    choices = mgcvUI:::locale_country_choices_(),
                    selected = "us", width = "100%"),
        selectInput("locale_paper", "Paper",
                    choices = c("Letter" = "letter", "A4" = "a4"),
                    selected = "letter", width = "100%"),
        actionLink("locale_save_default", "Save as my default",
                   style = "font-size: 0.85em; color: #5e81ac; display: block; margin-top: 4px;")
      )
    ),
    tags$div(class = "mgcv-spacer"),
    tags$button(id = "mgcv-theme-toggle", onclick = "mgcvToggleTheme()",
                HTML("&#9790;"))
  ),
  tags$script(HTML("
    var mgcvCurrentMode = 'light';

    function mgcvToggleDropdown(id) {
      var el = document.getElementById(id);
      if (el) el.classList.toggle('open');
    }
    document.addEventListener('click', function(e) {
      var dropdowns = document.querySelectorAll('.mgcv-navbar .dropdown');
      dropdowns.forEach(function(dd) {
        if (!dd.contains(e.target)) dd.classList.remove('open');
      });
    });

    function mgcvToggleTheme() {
      mgcvCurrentMode = (mgcvCurrentMode === 'dark') ? 'light' : 'dark';
      document.body.setAttribute('data-mgcv-theme', mgcvCurrentMode);
      Shiny.setInputValue('dark_mode', mgcvCurrentMode, {priority: 'event'});
      var btn = document.getElementById('mgcv-theme-toggle');
      if (btn) btn.innerHTML = (mgcvCurrentMode === 'dark') ? '\\u2600' : '\\u263E';
      try { localStorage.setItem('mgcvUI_theme', mgcvCurrentMode); } catch(e) {}
    }

    // Toggle element visibility
    Shiny.addCustomMessageHandler('mgcv_toggle_el', function(msg) {
      var el = document.getElementById(msg.id);
      if (el) el.style.display = msg.show ? 'block' : 'none';
    });

    // Update preset description text
    Shiny.addCustomMessageHandler('mgcv_preset_desc', function(msg) {
      var el = document.getElementById('vars-preset_desc');
      if (el) el.textContent = msg;
    });

    // Append white checkmark inside a button (no duplicates)
    function mgcvAddCheck(btnId) {
      var btn = document.getElementById(btnId);
      if (btn && !btn.querySelector('.mgcv-check')) {
        btn.insertAdjacentHTML('beforeend',
          ' <span class=\"mgcv-check\" style=\"color:#fff;\">&#10003;</span>');
      }
    }
    Shiny.addCustomMessageHandler('mgcv_show_check', function(msg) {
      mgcvAddCheck(msg.id);
    });
    Shiny.addCustomMessageHandler('download_check', function(msg) {
      mgcvAddCheck(msg.id);
    });

    // --- Fitting progress modal (matches earthUI) ---
    Shiny.addCustomMessageHandler('fitting_start', function(msg) {
      $('#mgcv-fitting-modal, #mgcv-fitting-backdrop').remove();
      clearInterval(window.mgcvTimerInterval);
      clearInterval(window.mgcvFittingTabsPoll);
      document.querySelectorAll('.mgcv-check').forEach(function(el) { el.remove(); });

      var start = Date.now();
      var modal = $(
        '<div id=\\\"mgcv-fitting-modal\\\" style=\\\"position:fixed;top:50%;left:50%;transform:translate(-50%,-50%);z-index:10001;' +
        'background:#2e3440;color:#d8dee9;border-radius:8px;box-shadow:0 4px 24px rgba(0,0,0,0.5);' +
        'width:520px;max-width:90vw;font-family:monospace;overflow:hidden;\\\">' +
          '<div style=\\\"background:#3b4252;padding:10px 16px;display:flex;justify-content:space-between;align-items:center;\\\">' +
            '<span style=\\\"font-size:0.95em;font-weight:bold;\\\">Fitting GAM Model</span>' +
            '<span id=\\\"mgcv-timer\\\" style=\\\"font-size:0.85em;color:#81a1c1;\\\">0s</span>' +
            '&nbsp;&nbsp;<span id=\\\"mgcv-fitting-abort\\\" style=\\\"color:#bf616a;cursor:pointer;' +
              'font-size:0.85em;font-weight:bold;margin-left:8px;\\\">Abort</span>' +
            '<span id=\\\"mgcv-fitting-close\\\" style=\\\"display:none;color:#81a1c1;cursor:pointer;' +
              'font-size:1.3em;line-height:1;margin-left:12px;\\\">&times;</span>' +
          '</div>' +
          '<div id=\\\"mgcv-trace-log\\\" style=\\\"padding:8px 12px;height:300px;overflow-y:auto;font-size:0.78em;line-height:1.5;\\\"></div>' +
        '</div>'
      );
      $('<div id=\\\"mgcv-fitting-backdrop\\\" style=\\\"position:fixed;top:0;left:0;width:100%;height:100%;' +
        'background:rgba(0,0,0,0.4);z-index:10000;\\\"></div>').appendTo('body');
      modal.appendTo('body');

      $('#mgcv-fitting-close').on('click', function() {
        clearInterval(window.mgcvTimerInterval);
        clearInterval(window.mgcvFittingTabsPoll);
        window.mgcvTimerInterval = null;
        window.mgcvFittingTabsPoll = null;
        $('#mgcv-fitting-modal, #mgcv-fitting-backdrop').fadeOut(300, function(){ $(this).remove(); });
      });

      $('#mgcv-fitting-abort').on('click', function() {
        Shiny.setInputValue('model-abort_fit', Date.now(), {priority: 'event'});
        $('#mgcv-trace-log').append($('<div style=\\\"color:#bf616a;font-weight:bold;margin-top:4px;\\\">').text('Aborting...'));
        $(this).css({opacity: 0.5, cursor: 'default'}).off('click');
      });

      $('#mgcv-trace-log').append($('<div style=\\\"color:#88c0d0;\\\">').text('Starting model fit...'));

      window.mgcvTimerInterval = setInterval(function() {
        var s = Math.floor((Date.now() - start) / 1000);
        var m = Math.floor(s / 60);
        var label = m > 0 ? m + 'm ' + (s % 60) + 's' : s + 's';
        $('#mgcv-timer').text(label);
      }, 1000);
    });

    Shiny.addCustomMessageHandler('trace_line', function(msg) {
      var $log = $('#mgcv-trace-log');
      if ($log.length) {
        var color = '#a3be8c';
        if (msg.text.match(/^Starting|^Fitting/)) color = '#88c0d0';
        else if (msg.text.match(/^\\\\s*CV fold/i)) color = '#ebcb8b';
        else if (msg.text.match(/error|fail/i)) color = '#bf616a';
        var $line = $('<div style=\\\"color:' + color + ';\\\">').text(msg.text);
        $log.append($line);
        $log.scrollTop($log[0].scrollHeight);
      }
    });

    Shiny.addCustomMessageHandler('fitting_done', function(msg) {
      var $log = $('#mgcv-trace-log');
      var hasError = msg.text === 'Error' || (msg.text && msg.text.match(/error|fail/i));
      var color = hasError ? '#bf616a' : '#a3be8c';
      $log.append($('<div style=\\\"color:' + color + ';font-weight:bold;margin-top:4px;\\\">').text(msg.text));
      $log.scrollTop($log[0].scrollHeight);
      if (hasError) {
        clearInterval(window.mgcvTimerInterval);
        $('#mgcv-fitting-close').show();
      } else {
        $log.append($('<div style=\\\"color:#88c0d0;margin-top:2px;\\\">').text('Now completing the tabs.'));
        $log.scrollTop($log[0].scrollHeight);
        var tabPollCount = 0;
        window.mgcvFittingTabsPoll = setInterval(function() {
          tabPollCount++;
          var still = $('.recalculating').length;
          if (still === 0 || tabPollCount > 100) {
            clearInterval(window.mgcvFittingTabsPoll);
            window.mgcvFittingTabsPoll = null;
            clearInterval(window.mgcvTimerInterval);
            window.mgcvTimerInterval = null;
            var elapsed = $('#mgcv-timer').text();
            $log.append($('<div style=\\\"color:#a3be8c;font-weight:bold;margin-top:2px;\\\">').text('Tabs complete. (' + elapsed + ')'));
            $log.scrollTop($log[0].scrollHeight);
            $('#mgcv-fitting-close').show();
          }
        }, 300);
      }
    });

    $(document).on('shiny:connected', function() {
      var saved = null;
      try { saved = localStorage.getItem('mgcvUI_theme'); } catch(e) {}
      if (saved === 'dark') {
        mgcvCurrentMode = 'dark';
        document.body.setAttribute('data-mgcv-theme', 'dark');
        var btn = document.getElementById('mgcv-theme-toggle');
        if (btn) btn.innerHTML = '\\u2600';
        Shiny.setInputValue('dark_mode', 'dark', {priority: 'event'});
      } else {
        document.body.setAttribute('data-mgcv-theme', 'light');
        Shiny.setInputValue('dark_mode', 'light', {priority: 'event'});
      }

      // Restore last-used purpose
      var lastPurpose = null;
      try { lastPurpose = localStorage.getItem('mgcvUI_last_purpose'); } catch(e) {}
      if (lastPurpose) {
        var radio = $(\"input[name='purpose'][value='\" + lastPurpose + \"']\");
        if (radio.length) {
          radio.prop('checked', true).trigger('change');
        }
      }
    });

    // Save purpose whenever it changes
    $(document).on('change', \"input[name='purpose']\", function() {
      try { localStorage.setItem('mgcvUI_last_purpose', $(this).val()); } catch(e) {}
    });

    // Initialize Bootstrap 5 popovers for param help icons
    $(document).on('shiny:value', function() {
      setTimeout(function() {
        var els = document.querySelectorAll('[data-bs-toggle=popover]');
        els.forEach(function(el) {
          if (!bootstrap.Popover.getInstance(el)) {
            new bootstrap.Popover(el, { html: false });
          }
        });
      }, 300);
    });
    // Also init on page load
    $(function() {
      setTimeout(function() {
        var els = document.querySelectorAll('[data-bs-toggle=popover]');
        els.forEach(function(el) {
          new bootstrap.Popover(el, { html: false });
        });
      }, 500);
    });
  ")),

  sidebarLayout(
    sidebarPanel(
      width = 4,

      # --- Purpose Mode ---
      tags$div(
        style = "font-weight:bold;",
        radioButtons("purpose", "Purpose:",
                     choices = c("General" = "general",
                                 "For Appraisal" = "appraisal",
                                 "Market Area Analysis" = "market"),
                     selected = "general", inline = TRUE)
      ),
      hr(),

      # --- 1. Import Data ---
      tags$details(class = "mgcv-section", open = NA,
        tags$summary(
          h4(style = "display:inline;",
            "1. Import Data",
            tags$span(
              style = "display:inline-block; width:18px; height:18px; border-radius:50%; background:#88c0d0; color:#fff; font-size:11px; font-weight:bold; text-align:center; line-height:18px; cursor:pointer; margin-left:8px; vertical-align:middle;",
              `data-bs-toggle` = "popover",
              `data-bs-trigger` = "hover focus",
              `data-bs-placement` = "right",
              `data-bs-content` = "Import the CSV or Excel file containing your sales data. Each row is an observation (comparable sale) and each column is a variable (sale price, living area, age, etc.). The data will be used to fit a GAM model via mgcv.",
              "?")
          )
        ),
        mod_data_ui("data")
      ),
      hr(),

      # --- 2. Import from earthUI (optional) ---
      tags$details(class = "mgcv-section", open = NA,
        tags$summary(
          h4(style = "display:inline;",
            "2. Import from earthUI (optional)",
            tags$span(
              style = "display:inline-block; width:18px; height:18px; border-radius:50%; background:#88c0d0; color:#fff; font-size:11px; font-weight:bold; text-align:center; line-height:18px; cursor:pointer; margin-left:8px; vertical-align:middle;",
              `data-bs-toggle` = "popover",
              `data-bs-trigger` = "hover focus",
              `data-bs-placement` = "right",
              `data-bs-content` = "Optionally import an earthUI result (.rds file) to seed the GAM with knot positions discovered by MARS. Earth finds data-driven change points (hinges) that become anchor knots for the GAM splines, giving the model a head start. Also imports variable selections, interaction structure, and linear/factor designations from the earth model.",
              "?")
          )
        ),
        div(style = "padding-top: 6px;",
          mod_earth_import_ui("earth")
        )
      ),
      hr(),

      conditionalPanel(
        condition = "output.data_imported",

        # --- 3. Project Output Folder ---
        tags$details(class = "mgcv-section",
          tags$summary(h4(style = "display:inline;",
            "3. Project Output Folder",
            tags$span(
              style = "display:inline-block; width:18px; height:18px; border-radius:50%; background:#88c0d0; color:#fff; font-size:11px; font-weight:bold; text-align:center; line-height:18px; cursor:pointer; margin-left:8px; vertical-align:middle;",
              `data-bs-toggle` = "popover",
              `data-bs-trigger` = "hover focus",
              `data-bs-placement` = "right",
              `data-bs-content` = "Set the folder where all output files are saved: Excel exports (estimated prices, adjustments), sales grids, and reports. Defaults to ~/Downloads.",
              "?")
          )),
          textInput("output_folder", NULL,
                    value = path.expand("~/Downloads"))
        ),
        hr(),

        # --- 4. Variable Configuration ---
        tags$details(class = "mgcv-section",
          tags$summary(h4(style = "display:inline;",
            "4. Variable Configuration",
            tags$span(
              style = "display:inline-block; width:18px; height:18px; border-radius:50%; background:#88c0d0; color:#fff; font-size:11px; font-weight:bold; text-align:center; line-height:18px; cursor:pointer; margin-left:8px; vertical-align:middle;",
              `data-bs-toggle` = "popover",
              `data-bs-trigger` = "hover focus",
              `data-bs-placement` = "right",
              `data-bs-content` = "Select the target (response) variable and configure predictors. Inc = include in model. Factor = treat as categorical. Linear = force a straight-line relationship (no smooth). Special = assign a column role (e.g. contract_date, weights, living_area) for appraisal-specific features.",
              "?")
          )),
          conditionalPanel(
            condition = "input.purpose !== 'general'",
            dateInput("effective_date", "Effective Date",
                      value = Sys.Date())
          ),
          mod_variables_ui("vars")
        ),
        hr(),

        # --- 5. Mgcv Call Parameters ---
        tags$details(class = "mgcv-section", open = NA,
          tags$summary(h4(style = "display:inline;",
            "5. Mgcv Call Parameters",
            tags$span(
              style = "display:inline-block; width:18px; height:18px; border-radius:50%; background:#88c0d0; color:#fff; font-size:11px; font-weight:bold; text-align:center; line-height:18px; cursor:pointer; margin-left:8px; vertical-align:middle;",
              `data-bs-toggle` = "popover",
              `data-bs-trigger` = "hover focus",
              `data-bs-placement` = "right",
              `data-bs-content` = "Configure the mgcv::gam() call: distribution family, smoothing method, penalty strength (gamma), basis type for splines, basis dimension (k), variable selection, and interaction structure. These control how flexible and regularized the fitted model will be.",
              "?")
          )),
          mod_variables_params_ui("vars")
        ),
        hr(),

        # --- 6. Fit Mgcv GAM Model ---
        tags$details(class = "mgcv-section", open = NA,
          tags$summary(h4(style = "display:inline;",
            "6. Fit Mgcv GAM Model",
            tags$span(
              style = "display:inline-block; width:18px; height:18px; border-radius:50%; background:#88c0d0; color:#fff; font-size:11px; font-weight:bold; text-align:center; line-height:18px; cursor:pointer; margin-left:8px; vertical-align:middle;",
              `data-bs-toggle` = "popover",
              `data-bs-trigger` = "hover focus",
              `data-bs-placement` = "right",
              `data-bs-content` = "Fit the Generalized Additive Model using mgcv. The model estimates smooth nonlinear relationships between each predictor and the response. Results appear in the tabs on the right: summary statistics, contribution plots, diagnostics, and more.",
              "?")
          )),
          mod_model_fit_ui("model")
        ),

        # --- 7. Download Estimated Sale Prices & Residuals ---
        conditionalPanel(
          condition = "output.model_fitted",
          hr(),
          tags$details(class = "mgcv-section",
            tags$summary(uiOutput("download_heading", inline = TRUE)),
            actionButton("export_data", "Download Output (Excel)",
                         class = "btn-primary",
                         style = "width: 100%;")
          )
        ),

        # --- 8. Calculate RCA Adjustments (Appraisal/Market only) ---
        conditionalPanel(
          condition = "output.model_fitted && (input.purpose === 'appraisal' || input.purpose === 'market')",
          hr(),
          tags$details(class = "mgcv-section",
            tags$summary(h4("8. Calculate RCA Adjustments & Download",
                            style = "display:inline;")),
            actionButton("rca_output_btn",
                         "Calculate RCA Adjustments & Download",
                         class = "btn-primary",
                         style = "width: 100%;")
          )
        ),

        # --- 9. Generate Sales Grid & Download (Appraisal only) ---
        conditionalPanel(
          condition = "output.model_fitted && input.purpose === 'appraisal'",
          hr(),
          tags$details(class = "mgcv-section",
            tags$summary(h4("9. Generate Sales Grid & Download",
                            style = "display:inline;")),
            tags$p("Recommends comps with gross adjustment < 25%, ",
                   "sorted by sale age. You can add or remove comps.",
                   style = "font-size: 0.85em; color: var(--bs-secondary-color);"),
            actionButton("sales_grid_btn",
                         "Generate Sales Grid & Download",
                         class = "btn-primary",
                         style = "width: 100%;")
          )
        ),

        # --- 10. Download Report ---
        conditionalPanel(
          condition = "output.model_fitted",
          hr(),
          tags$details(class = "mgcv-section",
            tags$summary(uiOutput("report_heading", inline = TRUE)),
            selectInput("export_format", "Format",
                        choices = c("HTML" = "html",
                                    "Word" = "docx",
                                    "PDF" = "pdf")),
            actionButton("export_report_btn", "Download Report",
                         class = "btn-primary",
                         style = "width: 100%;")
          )
        )
      )
    ),

    mainPanel(
      width = 8,
      conditionalPanel(
        condition = "!output.data_imported",
        div(
          class = "text-muted",
          style = "text-align: center; padding: 80px 20px;",
          h3("Welcome to mgcvUI"),
          p("Upload a CSV or Excel file, configure variables, ",
            "and fit a GAM model."),
          p("Designed for real estate appraisers who need transparent, ",
            "defensible nonlinear models.")
        )
      ),
      mod_model_results_ui("model")
    )
  ),

  # Footer
  tags$hr(style = "margin-top: 30px; margin-bottom: 10px;"),
  tags$footer(
    style = paste("text-align: center; padding: 10px 15px 15px;",
                  "font-size: 0.8em; color: var(--bs-secondary-color);"),
    tags$p(style = "margin: 2px 0;",
      HTML(paste0("mgcvUI v", utils::packageVersion("mgcvUI")))
    ),
    tags$p(style = "margin: 2px 0;",
      "Licensed under the ",
      tags$a(href = "https://www.gnu.org/licenses/agpl-3.0.html",
             target = "_blank",
             "GNU Affero General Public License v3.0"),
      " or later (AGPL-3)."
    )
  )
)

server <- function(input, output, session) {
  # --- Nord theme switching ---
  observe({
    mode <- input$dark_mode
    req(mode)
    tryCatch(
      session$setCurrentTheme(
        if (mode == "dark") nord_dark else nord_light
      ),
      error = function(e) {
        message("Theme switch error (non-fatal): ", conditionMessage(e))
      }
    )
  })

  # --- Settings locale ---
  # Guard flag: suppress DB writes until startup restoration is complete
  locale_ready <- reactiveVal(FALSE)

  # Load user's locale defaults from SQLite on startup
  locale_defaults <- mgcvUI:::settings_db_read_locale_()
  if (!is.null(locale_defaults) && length(locale_defaults) > 0L) {
    ld <- locale_defaults
    if (!is.null(ld$locale_country))
      updateSelectInput(session, "locale_country", selected = ld$locale_country)
    if (!is.null(ld$locale_paper))
      updateSelectInput(session, "locale_paper", selected = ld$locale_paper)
    if (!is.null(ld$locale_import))
      updateSelectInput(session, "data-locale_import", selected = ld$locale_import)
    if (!is.null(ld$output_folder) && nzchar(ld$output_folder))
      updateTextInput(session, "output_folder", value = ld$output_folder)
    if (!is.null(ld$effective_date) && nzchar(ld$effective_date))
      updateDateInput(session, "effective_date",
                      value = as.Date(ld$effective_date))
    if (!is.null(ld$purpose) && nzchar(ld$purpose))
      updateRadioButtons(session, "purpose", selected = ld$purpose)
    message("mgcvUI: restored locale defaults from SQLite")
  }

  # Mark locale as ready after a delay so restoration completes first
  locale_timer_ <- shiny::reactiveTimer(2000, session)
  shiny::observeEvent(locale_timer_(), {
    locale_ready(TRUE)
    message("mgcvUI: locale guard released")
  }, once = TRUE)

  # Save locale as user default (explicit button)
  observeEvent(input$locale_save_default, {
    locale_settings <- list(
      locale_country = input$locale_country,
      locale_paper   = input$locale_paper,
      locale_import  = input[["data-locale_import"]]
    )
    mgcvUI:::settings_db_write_locale_(locale_settings)
    showNotification("Locale saved as default for all new files.",
                     type = "message", duration = 4)
  })

  # When Settings country changes, sync import locale and update env
  observeEvent(input$locale_country, {
    country <- input$locale_country %||% "us"
    presets <- mgcvUI:::locale_country_presets_()
    preset <- presets[[country]] %||% presets[["us"]]
    updateSelectInput(session, "locale_paper", selected = preset$paper)
    # Sync the per-file import locale dropdown in the data module
    updateSelectInput(session, "data-locale_import", selected = country)
    mgcvUI:::set_locale_(country)
    # Only persist if startup restoration is done
    if (isTRUE(isolate(locale_ready()))) {
      mgcvUI:::settings_db_write_locale_(list(
        locale_country = country,
        locale_paper   = preset$paper,
        locale_import  = country
      ))
    }
  })

  # When import locale or paper changes, update locale env
  observe({
    import_country <- input[["data-locale_import"]] %||%
                      input$locale_country %||% "us"
    settings_country <- input$locale_country %||% "us"
    paper <- input$locale_paper %||% "letter"
    presets <- mgcvUI:::locale_country_presets_()
    import_preset <- presets[[import_country]] %||% presets[["us"]]
    settings_preset <- presets[[settings_country]] %||% presets[["us"]]
    mgcvUI:::set_locale_(settings_country,
                         csv_sep = import_preset$csv_sep,
                         csv_dec = import_preset$csv_dec,
                         big_mark = settings_preset$big_mark,
                         dec_mark = settings_preset$dec_mark,
                         date_fmt = import_preset$date_fmt,
                         paper = paper)
    # Only persist if startup restoration is done
    if (isTRUE(isolate(locale_ready()))) {
      mgcvUI:::settings_db_write_locale_(list(
        locale_country = settings_country,
        locale_paper   = paper,
        locale_import  = import_country
      ))
    }
  })

  # Persist output folder and effective date to global defaults
  observeEvent(input$output_folder, {
    if (!isTRUE(locale_ready())) return()
    folder <- input$output_folder
    if (is.null(folder) || !nzchar(folder)) return()
    ld <- mgcvUI:::settings_db_read_locale_() %||% list()
    ld$output_folder <- folder
    mgcvUI:::settings_db_write_locale_(ld)
  })

  observeEvent(input$effective_date, {
    if (!isTRUE(locale_ready())) return()
    ed <- input$effective_date
    if (is.null(ed)) return()
    ld <- mgcvUI:::settings_db_read_locale_() %||% list()
    ld$effective_date <- as.character(ed)
    mgcvUI:::settings_db_write_locale_(ld)
  })

  # Data import - returns list(data, filename)
  data_mod <- mod_data_server("data")

  # --- Data imported flag ---
  output$data_imported <- reactive(!is.null(data_mod$data()))
  outputOptions(output, "data_imported", suspendWhenHidden = FALSE)

  # --- Restore app-level settings when data file changes ---
  observeEvent(data_mod$filename(), {
    fname <- data_mod$filename()
    if (is.null(fname)) return()
    saved <- mgcvUI:::settings_db_read_(fname)
    if (is.null(saved)) return()
    if (!is.null(saved$purpose)) {
      updateRadioButtons(session, "purpose", selected = saved$purpose)
    }
    if (!is.null(saved$output_folder)) {
      updateTextInput(session, "output_folder", value = saved$output_folder)
    }
    if (!is.null(saved$effective_date)) {
      updateDateInput(session, "effective_date",
                      value = as.Date(saved$effective_date))
    }
  })

  # --- Save app-level settings when they change ---
  observe({
    fname <- data_mod$filename()
    req(fname)
    input$purpose
    input$output_folder
    input$effective_date
    isolate({
      saved <- mgcvUI:::settings_db_read_(fname)
      config <- if (!is.null(saved)) saved else list()
      config$output_folder  <- input$output_folder %||% ""
      config$effective_date <- as.character(input$effective_date %||% "")
      config$purpose        <- input$purpose %||% "general"
      mgcvUI:::settings_db_write_(fname, config)
    })
  })

  output$data_preview_table <- DT::renderDT({
    req(data_mod$data())
    DT::datatable(data_mod$data(),
                  options = list(scrollX = TRUE, pageLength = 15),
                  rownames = FALSE)
  })

  # Optional earth import
  earth_mod <- mod_earth_import_server("earth")
  earth_knots_r <- earth_mod$knots

  # Reset data and earth imports when purpose changes
  observeEvent(input$purpose, {
    data_mod$reset()
    earth_mod$reset()
  }, ignoreInit = TRUE)

  # Variable selection - returns config list
  var_config_r <- mod_variables_server("vars",
                                       data_r        = data_mod$data,
                                       filename_r    = data_mod$filename,
                                       earth_knots_r = earth_knots_r,
                                       purpose_r     = reactive(input$purpose))

  # Derived data reactive: recompute sale_age when effective_date changes
  app_data_r <- reactive({
    df <- data_mod$data()
    req(df)
    cfg <- var_config_r()
    eff_date <- input$effective_date

    specials <- cfg$specials
    if (is.null(specials) || is.null(eff_date)) return(df)

    contract_col <- names(which(specials == "contract_date"))
    sale_age_col <- names(which(specials == "sale_age"))

    if (length(contract_col) == 1L && length(sale_age_col) == 1L &&
        contract_col %in% names(df) && sale_age_col %in% names(df)) {
      # Parse contract dates
      contract_dates <- df[[contract_col]]
      if (!inherits(contract_dates, "Date")) {
        fmts <- mgcvUI:::locale_date_formats_()
        for (fmt in fmts) {
          parsed <- as.Date(contract_dates, format = fmt)
          # Require >50% non-NA AND dates in a sane range (1900-2100)
          # to avoid %Y parsing 2-digit years as year 0025
          ok <- !is.na(parsed) & parsed >= as.Date("1900-01-01") &
                parsed <= as.Date("2100-12-31")
          if (sum(ok) > length(contract_dates) * 0.5) {
            contract_dates <- parsed
            break
          }
        }
      }
      if (inherits(contract_dates, "Date")) {
        df[[sale_age_col]] <- as.numeric(
          difftime(as.Date(eff_date), contract_dates, units = "days")
        )
      }
    }
    df
  })

  # Model fitting + results display
  gam_result_r <- mod_model_server("model",
                                   data_r        = app_data_r,
                                   var_config_r  = var_config_r,
                                   earth_knots_r = earth_knots_r,
                                   dark_mode_r   = reactive(identical(input$dark_mode, "dark")))

  # Report export (existing module — kept for function export features)
  mod_report_server("report",
                    gam_result_r = gam_result_r,
                    data_r       = app_data_r)

  # --- Model fitted flag for conditionalPanel ---
  output$model_fitted <- reactive(!is.null(gam_result_r()))
  outputOptions(output, "model_fitted", suspendWhenHidden = FALSE)

  # --- RCA percentage data (stored after RCA export) ---
  rv_rca <- reactiveValues(pct_data = NULL, rca_df = NULL)

  output$rca_computed <- reactive(!is.null(rv_rca$pct_data))
  outputOptions(output, "rca_computed", suspendWhenHidden = FALSE)

  # --- Dynamic step headings ---
  output$download_heading <- renderUI({
    label <- if (identical(input$purpose, "general")) {
      "7. Download Estimated Target Variable(s) & Residuals"
    } else {
      "7. Download Estimated Sale Prices & Residuals"
    }
    h4(label, style = "display:inline;")
  })

  output$report_heading <- renderUI({
    n <- if (identical(input$purpose, "appraisal")) "10"
         else if (identical(input$purpose, "market")) "9"
         else "8"
    h4(paste0(n, ". Download Report"), style = "display:inline;")
  })

  # ---- Helper: find living_area column from variable specials ----
  find_living_area_ <- function() {
    cfg <- var_config_r()
    if (is.null(cfg) || is.null(cfg$specials)) return(NULL)
    sp <- cfg$specials
    la_idx <- which(sp == "living_area")
    if (length(la_idx) == 0) return(NULL)
    names(sp)[la_idx[1L]]
  }

  # ---- Helper: compute per-smooth-term contributions ----
  # Uses predict(model, type = "terms") which returns a matrix of
  # per-term contributions. For GAMs this is native and simple.
  compute_gam_contributions_ <- function(model, newdata) {
    # type = "terms" returns a matrix with one column per model term
    preds <- predict(model, newdata = newdata, type = "terms")
    # Convert to named list of numeric vectors
    contribs <- list()
    for (col in colnames(preds)) {
      contribs[[col]] <- as.numeric(preds[, col])
    }
    contribs
  }

  # --- 6. Download Output (Excel) ---
  observeEvent(input$export_data, {
    req(gam_result_r(), data_mod$data())
    if (!requireNamespace("writexl", quietly = TRUE)) {
      showNotification(
        "Package 'writexl' required. Install with: install.packages('writexl')",
        type = "error", duration = 10)
      return()
    }

    folder <- input$output_folder
    if (is.null(folder) || !nzchar(folder)) folder <- path.expand("~/Downloads")
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)

    tryCatch({
      export_df <- data_mod$data()
      res       <- gam_result_r()
      model     <- res$model
      response  <- res$response
      xform     <- res$response_transform %||% "none"
      n         <- nrow(export_df)

      # Predictions (model predicts on transformed scale, back-transform)
      est_col   <- rep(NA_real_, n)
      resid_col <- rep(NA_real_, n)

      complete <- tryCatch({
        pv <- predict(model, newdata = export_df, type = "response")
        est_col <- mgcvUI:::back_transform_(as.numeric(pv), xform)
        resid_col <- export_df[[response]] - est_col
        rep(TRUE, n)
      }, error = function(e) {
        message("Prediction error on some rows: ", e$message)
        ok <- logical(n)
        for (i in seq_len(n)) {
          tryCatch({
            pv <- predict(model, newdata = export_df[i, , drop = FALSE],
                          type = "response")
            est_col[i] <<- mgcvUI:::back_transform_(as.numeric(pv), xform)
            resid_col[i] <<- export_df[[response]][i] - est_col[i]
            ok[i] <- TRUE
          }, error = function(e2) NULL)
        }
        ok
      })

      # Per-term contributions (remain on transformed scale)
      if (any(complete)) {
        contribs <- compute_gam_contributions_(model,
                                                export_df[complete, , drop = FALSE])
        intercept <- stats::coef(model)[["(Intercept)"]]

        # Label basis/contributions with scale info
        contrib_suffix <- if (xform != "none") paste0(" (", xform, " scale)") else ""
        export_df[["basis"]] <- NA_real_
        export_df[["basis"]][complete] <- round(intercept, 4)

        for (tl in names(contribs)) {
          col_name <- gsub("^s\\((.+)\\)$", "\\1", tl)
          col_name <- gsub("^te\\((.+)\\)$", "\\1", col_name)
          col_name <- gsub("^ti\\((.+)\\)$", "\\1", col_name)
          col_name <- paste0(col_name, "_contribution")
          export_df[[col_name]] <- NA_real_
          export_df[[col_name]][complete] <- round(contribs[[tl]], 4)
        }

        # Verification: back-transform sum of contributions vs est
        contrib_total <- intercept + Reduce(`+`, contribs)
        est_from_contribs <- mgcvUI:::back_transform_(contrib_total, xform)
        export_df[["calc_residual"]] <- NA_real_
        export_df[["calc_residual"]][complete] <-
          round(export_df[[response]][complete] - est_from_contribs, 1)
      }

      export_df[[paste0("est_", response)]] <- round(est_col, 1)
      export_df[["residual"]] <- round(resid_col, 1)

      # --- CQA scores ---
      la_col <- find_living_area_()
      comp_rows <- if (identical(input$purpose, "appraisal")) -1L else seq_len(n)
      comp_resid <- resid_col[comp_rows]
      comp_resid <- comp_resid[!is.na(comp_resid)]
      n_comps <- length(comp_resid)
      if (n_comps > 0) {
        cqa <- vapply(resid_col, function(r) {
          if (is.na(r)) return(NA_real_)
          sum(comp_resid < r, na.rm = TRUE) / n_comps * 10
        }, numeric(1))
        export_df[["cqa"]] <- round(cqa, 2)

        if (!is.null(la_col) && la_col %in% names(export_df)) {
          resid_sf <- resid_col / export_df[[la_col]]
          export_df[["residual_sf"]] <- round(resid_sf, 4)
          comp_resid_sf <- resid_sf[comp_rows]
          comp_resid_sf <- comp_resid_sf[!is.na(comp_resid_sf)]
          n_sf <- length(comp_resid_sf)
          if (n_sf > 0) {
            cqa_sf <- vapply(resid_sf, function(r) {
              if (is.na(r)) return(NA_real_)
              sum(comp_resid_sf < r, na.rm = TRUE) / n_sf * 10
            }, numeric(1))
            export_df[["cqa_sf"]] <- round(cqa_sf, 2)
          }
        }
      }

      # In appraisal mode, set subject row (row 1) actual/residual to NA
      if (identical(input$purpose, "appraisal")) {
        export_df[["residual"]][1L] <- NA_real_
        if ("cqa" %in% names(export_df)) export_df[["cqa"]][1L] <- NA_real_
        if ("cqa_sf" %in% names(export_df)) export_df[["cqa_sf"]][1L] <- NA_real_
      }

      # Sort by residual_sf descending for appraisal/market
      if (input$purpose %in% c("appraisal", "market")) {
        has_subject <- identical(input$purpose, "appraisal")
        sort_col <- if ("residual_sf" %in% names(export_df)) "residual_sf" else "residual"
        if (sort_col %in% names(export_df)) {
          if (has_subject && nrow(export_df) >= 2L) {
            comps <- export_df[2:nrow(export_df), , drop = FALSE]
            comps <- comps[order(comps[[sort_col]], decreasing = TRUE,
                                 na.last = TRUE), , drop = FALSE]
            export_df <- rbind(export_df[1L, , drop = FALSE], comps)
          } else {
            export_df <- export_df[order(export_df[[sort_col]],
                                         decreasing = TRUE,
                                         na.last = TRUE), , drop = FALSE]
          }
        }
      }

      # Move ranking columns to the left
      rank_cols <- c("residual_sf", "cqa_sf", "residual", "cqa")
      rank_cols <- rank_cols[rank_cols %in% names(export_df)]
      if (length(rank_cols) > 0L) {
        other_cols <- setdiff(names(export_df), rank_cols)
        export_df <- export_df[, c(rank_cols, other_cols), drop = FALSE]
      }

      base <- tools::file_path_sans_ext(data_mod$filename() %||% "mgcvui")
      out_path <- file.path(folder, paste0(base, "_output_",
                            format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))
      writexl::write_xlsx(export_df, out_path)
      session$sendCustomMessage("download_check", list(id = "export_data"))
      showNotification(paste0("Output saved to: ", out_path),
                       type = "message", duration = 8)
    }, error = function(e) {
      showNotification(paste("Export error:", e$message),
                       type = "error", duration = 10)
    })
  })

  # --- 7. Calculate RCA Adjustments & Download ---
  observeEvent(input$rca_output_btn, {
    req(gam_result_r(), data_mod$data())

    la_col <- find_living_area_()
    cqa_choices <- c("CQA" = "cqa")
    if (!is.null(la_col)) {
      cqa_choices <- c(cqa_choices, "CQA per SF" = "cqa_sf")
    }

    showModal(modalDialog(
      title = "RCA Raw Output \u2014 Subject CQA Score",
      radioButtons("rca_cqa_type", "Score type:",
                   choices = cqa_choices, inline = TRUE),
      numericInput("rca_cqa_value", "Subject CQA Score:",
                   value = 5.00, min = 0, max = 9.99, step = 0.01),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("export_rca", "Generate", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$export_rca, {
    removeModal()
    req(gam_result_r(), data_mod$data())
    if (!requireNamespace("writexl", quietly = TRUE)) {
      showNotification(
        "Package 'writexl' required. Install with: install.packages('writexl')",
        type = "error", duration = 10)
      return()
    }

    folder <- input$output_folder
    if (is.null(folder) || !nzchar(folder)) folder <- path.expand("~/Downloads")
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)

    tryCatch({
      export_df <- data_mod$data()
      res       <- gam_result_r()
      model     <- res$model
      response  <- res$response
      xform     <- res$response_transform %||% "none"
      user_cqa  <- input$rca_cqa_value
      n         <- nrow(export_df)

      if (n < 2L) {
        showNotification("Need at least 2 rows (subject + 1 comp).",
                         type = "error")
        return()
      }

      # Predictions (back-transform to original scale)
      predicted_log <- as.numeric(predict(model, newdata = export_df,
                                          type = "response"))
      predicted <- mgcvUI:::back_transform_(predicted_log, xform)
      actual    <- export_df[[response]]
      residuals_val <- actual - predicted

      # Subject row (row 1): sale price treated as NA
      actual[1L] <- NA_real_
      residuals_val[1L] <- NA_real_

      export_df[[paste0("est_", response)]] <- round(predicted, 1)

      # --- CQA scores on comps (rows 2+) ---
      la_col <- find_living_area_()
      comp_resid <- residuals_val[-1L]
      comp_resid_valid <- comp_resid[!is.na(comp_resid)]
      n_comps <- length(comp_resid_valid)

      cqa_col <- rep(NA_real_, n)
      if (n_comps > 0) {
        cqa_col <- vapply(residuals_val, function(r) {
          if (is.na(r)) return(NA_real_)
          sum(comp_resid_valid < r, na.rm = TRUE) / n_comps * 10
        }, numeric(1))
      }
      export_df[["residual"]] <- round(residuals_val, 1)
      export_df[["cqa"]] <- round(cqa_col, 2)

      # CQA_SF if living_area designated
      resid_sf <- NULL
      cqa_sf_col <- NULL
      if (!is.null(la_col) && la_col %in% names(export_df)) {
        resid_sf <- residuals_val / export_df[[la_col]]
        export_df[["residual_sf"]] <- round(resid_sf, 4)
        comp_resid_sf <- resid_sf[-1L]
        comp_resid_sf_valid <- comp_resid_sf[!is.na(comp_resid_sf)]
        n_sf <- length(comp_resid_sf_valid)
        if (n_sf > 0) {
          cqa_sf_col <- vapply(resid_sf, function(r) {
            if (is.na(r)) return(NA_real_)
            sum(comp_resid_sf_valid < r, na.rm = TRUE) / n_sf * 10
          }, numeric(1))
          export_df[["cqa_sf"]] <- round(cqa_sf_col, 2)
        }
      }

      # --- Interpolate subject residual from CQA ---
      use_sf <- (input$rca_cqa_type == "cqa_sf" && !is.null(la_col))
      if (use_sf) {
        comp_cqa_vals <- cqa_sf_col[-1L]
        comp_resid_for_interp <- resid_sf[-1L]
      } else {
        comp_cqa_vals <- cqa_col[-1L]
        comp_resid_for_interp <- residuals_val[-1L]
      }

      valid <- !is.na(comp_cqa_vals) & !is.na(comp_resid_for_interp)
      cqa_sorted   <- comp_cqa_vals[valid]
      resid_sorted <- comp_resid_for_interp[valid]
      ord <- order(cqa_sorted)
      cqa_sorted   <- cqa_sorted[ord]
      resid_sorted <- resid_sorted[ord]

      subject_resid <- stats::approx(cqa_sorted, resid_sorted,
                                     xout = user_cqa, rule = 2)$y

      # Convert per-SF back to total if needed
      if (use_sf) {
        subject_la <- export_df[[la_col]][1L]
        subject_resid_total <- subject_resid * subject_la
      } else {
        subject_resid_total <- subject_resid
      }

      # Subject value = model estimate + interpolated residual
      subject_est <- predicted[1L] + subject_resid_total
      residuals_val[1L] <- subject_resid_total
      export_df[["residual"]][1L] <- round(subject_resid_total, 1)
      export_df[["subject_value"]] <- NA_real_
      export_df[["subject_value"]][1L] <- round(subject_est, 1)
      export_df[["subject_cqa"]] <- NA_real_
      export_df[["subject_cqa"]][1L] <- user_cqa

      if (use_sf && !is.null(la_col)) {
        export_df[["residual_sf"]][1L] <- round(subject_resid, 1)
      }

      # Handle weight-0 rows: use subject_value so RCA columns can be computed
      wt_col_name <- var_config_r()$weights_col
      zero_wt <- integer(0)
      if (!is.null(wt_col_name) && wt_col_name %in% names(export_df)) {
        wvals <- export_df[[wt_col_name]]
        zero_wt <- which(wvals == 0)
      }
      if (length(zero_wt) > 0L) {
        sv <- predicted[zero_wt] + subject_resid_total
        export_df[["subject_value"]][zero_wt] <- round(sv, 1)
        actual[zero_wt] <- sv
        residuals_val <- actual - predicted
        export_df[["residual"]][zero_wt] <- round(residuals_val[zero_wt], 1)
        if (!is.null(la_col) && la_col %in% names(export_df)) {
          la <- export_df[[la_col]]
          export_df[["residual_sf"]][zero_wt] <-
            round(residuals_val[zero_wt] / la[zero_wt], 1)
        }
      }

      # --- Per-term contributions ---
      contribs <- compute_gam_contributions_(model, export_df)
      intercept <- stats::coef(model)[["(Intercept)"]]

      # Back-transform basis to dollar scale for display
      export_df[["basis"]] <- round(
        mgcvUI:::back_transform_(intercept, xform), 1)

      clean_term_name_ <- function(tl) {
        nm <- gsub("^s\\((.+)\\)$", "\\1", tl)
        nm <- gsub("^te\\((.+)\\)$", "\\1", nm)
        nm <- gsub("^ti\\((.+)\\)$", "\\1", nm)
        nm
      }

      # For log-transformed models: convert log-scale contributions
      # to dollar-scale value contributions using the linearized approach.
      # Each comp's dollar contribution for a term =
      #   predicted_dollars * (10^contrib_i - 1) / (10^sum_contribs - 1)
      #   ... simplified: we use back_transform on cumulative sums.
      # Simpler approach: contribution_dollars = predicted * contrib_log / sum_log
      # Best approach: for each row, dollar VC = back_transform(intercept + contribs)
      #   allocated proportionally.

      if (xform != "none") {
        # Log-scale total per row
        log_total <- intercept + Reduce(`+`, contribs)
        dollar_pred <- mgcvUI:::back_transform_(log_total, xform)
        # Baseline: predicted value with zero contribution from all smooths
        dollar_base <- mgcvUI:::back_transform_(intercept, xform)
        # Total dollar effect of all terms
        dollar_effect <- dollar_pred - dollar_base

        # Proportional allocation: each term's dollar contribution is its
        # share of the log-scale sum × the total dollar effect.
        # This avoids the multicollinearity blow-up from leave-one-out.
        log_sum <- Reduce(`+`, contribs)
        for (tl in names(contribs)) {
          col_name <- paste0(clean_term_name_(tl), "_contribution")
          # Proportion of log-scale total attributable to this term
          share <- ifelse(abs(log_sum) > 1e-10,
                          contribs[[tl]] / log_sum, 0)
          export_df[[col_name]] <- round(dollar_effect * share, 1)
        }
      } else {
        for (tl in names(contribs)) {
          col_name <- paste0(clean_term_name_(tl), "_contribution")
          export_df[[col_name]] <- round(contribs[[tl]], 1)
        }
      }

      # --- RCA Adjustments (always in dollars) ---
      adj_sum   <- rep(0, n)
      gross_sum <- rep(0, n)

      if (xform != "none") {
        # Adjustments: subject's contribution minus comp's contribution
        # Use proportional allocation (same method as contributions above)
        log_total <- intercept + Reduce(`+`, contribs)
        dollar_pred <- mgcvUI:::back_transform_(log_total, xform)
        dollar_base <- mgcvUI:::back_transform_(intercept, xform)
        dollar_effect <- dollar_pred - dollar_base
        log_sum <- Reduce(`+`, contribs)

        for (tl in names(contribs)) {
          adj_col_name <- paste0(clean_term_name_(tl), "_adjustment")
          share <- ifelse(abs(log_sum) > 1e-10,
                          contribs[[tl]] / log_sum, 0)
          dollar_contrib <- dollar_effect * share
          # Adjustment = subject's dollar contribution - comp's
          adjustment <- dollar_contrib[1L] - dollar_contrib
          export_df[[adj_col_name]] <- round(adjustment, 1)
          adj_sum   <- adj_sum + ifelse(is.na(adjustment), 0, adjustment)
          gross_sum <- gross_sum + ifelse(is.na(adjustment), 0, abs(adjustment))
        }
      } else {
        for (tl in names(contribs)) {
          adj_col_name <- paste0(clean_term_name_(tl), "_adjustment")
          subject_contrib <- contribs[[tl]][1L]
          adjustment <- subject_contrib - contribs[[tl]]
          export_df[[adj_col_name]] <- round(adjustment, 1)
          adj_sum   <- adj_sum + ifelse(is.na(adjustment), 0, adjustment)
          gross_sum <- gross_sum + ifelse(is.na(adjustment), 0, abs(adjustment))
        }
      }

      # Residual adjustment (always in dollars)
      resid_adj <- subject_resid_total - residuals_val
      export_df[["residual_adjustment"]] <- round(resid_adj, 1)
      adj_sum   <- adj_sum + ifelse(is.na(resid_adj), 0, resid_adj)
      gross_sum <- gross_sum + ifelse(is.na(resid_adj), 0, abs(resid_adj))

      export_df[["net_adjustments"]]   <- round(adj_sum, 1)
      export_df[["gross_adjustments"]] <- round(gross_sum, 1)

      sale_price <- export_df[[response]]
      export_df[["residual_adj_pct"]] <- round(resid_adj / sale_price * 100, 2)
      export_df[["net_adj_pct"]]      <- round(adj_sum / sale_price * 100, 2)
      export_df[["gross_adj_pct"]]    <- round(gross_sum / sale_price * 100, 2)

      export_df[["adjusted_sale_price"]] <- round(actual + adj_sum, 1)

      # Subject row: adjustments are zero (subject vs self)
      adj_cols <- grep(
        "_adjustment$|net_adjustments|gross_adjustments|adjusted_sale_price|_adj_pct$",
        names(export_df), value = TRUE)
      for (ac in adj_cols) {
        export_df[[ac]][1L] <- NA_real_
      }
      export_df[["adjusted_sale_price"]][1L] <- round(subject_est, 1)

      # Store pct data for RCA Analysis plots (comps only, exclude subject)
      rv_rca$pct_data <- data.frame(
        residual_adj_pct = export_df[["residual_adj_pct"]][-1L],
        net_adj_pct      = export_df[["net_adj_pct"]][-1L],
        gross_adj_pct    = export_df[["gross_adj_pct"]][-1L],
        stringsAsFactors = FALSE
      )
      rv_rca$rca_df <- export_df

      # Move ranking columns to the left
      rank_cols <- c("residual_sf", "cqa_sf", "residual", "cqa")
      rank_cols <- rank_cols[rank_cols %in% names(export_df)]
      if (length(rank_cols) > 0L) {
        other_cols <- setdiff(names(export_df), rank_cols)
        export_df <- export_df[, c(rank_cols, other_cols), drop = FALSE]
      }

      base <- tools::file_path_sans_ext(data_mod$filename() %||% "mgcvui")
      out_path <- file.path(folder, paste0(base, "_adjusted_",
                            format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))
      writexl::write_xlsx(export_df, out_path)
      session$sendCustomMessage("download_check",
                                list(id = "rca_output_btn"))
      showNotification(paste0("RCA output saved to: ", out_path),
                       type = "message", duration = 8)
    }, error = function(e) {
      showNotification(paste("RCA error:", e$message),
                       type = "error", duration = 15)
    })
  })

  # --- RCA Analysis Plots ---
  rca_pct_histogram_ <- function(vals, title, xlab, fill_color) {
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) return(NULL)

    avg_val    <- mean(vals)
    median_val <- stats::median(vals)
    sd_val     <- stats::sd(vals)
    font_fam   <- mgcv_font_family_()

    # Bins in 20% increments
    lo <- floor(min(vals) / 20) * 20
    hi <- ceiling(max(vals) / 20) * 20
    breaks <- seq(lo, hi, by = 20)
    if (length(breaks) < 2) breaks <- c(lo, lo + 20)

    df <- data.frame(x = vals)
    subtitle <- sprintf("Mean: %.2f%%    Median: %.2f%%    Std Dev: %.2f%%",
                        avg_val, median_val, sd_val)

    ggplot2::ggplot(df, ggplot2::aes(x = .data$x)) +
      ggplot2::geom_histogram(breaks = breaks, fill = fill_color,
                              color = "white", alpha = 0.85) +
      ggplot2::geom_vline(xintercept = avg_val, linetype = "dashed",
                          color = "#2e3440", linewidth = 0.8) +
      ggplot2::geom_vline(xintercept = median_val, linetype = "dotted",
                          color = "#5e81ac", linewidth = 0.8) +
      ggplot2::annotate("text", x = avg_val, y = Inf, label = "Mean",
                        vjust = 2, hjust = -0.15, size = 3.5,
                        color = "#2e3440", family = font_fam) +
      ggplot2::annotate("text", x = median_val, y = Inf, label = "Median",
                        vjust = 3.5, hjust = -0.15, size = 3.5,
                        color = "#5e81ac", family = font_fam) +
      ggplot2::scale_x_continuous(breaks = breaks,
                                  labels = paste0(breaks, "%")) +
      ggplot2::labs(title = title, subtitle = subtitle,
                    x = xlab, y = "Frequency") +
      ggplot2::theme_minimal(base_family = font_fam) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 11, color = "#4c566a"),
        axis.text.x = ggplot2::element_text(angle = 0)
      )
  }

  output$rca_resid_pct_plot <- renderPlot({
    req(rv_rca$pct_data)
    rca_pct_histogram_(rv_rca$pct_data$residual_adj_pct,
                       "Residual Adjustment %",
                       "Residual Adj. %", "#88c0d0")
  })

  output$rca_net_pct_plot <- renderPlot({
    req(rv_rca$pct_data)
    rca_pct_histogram_(rv_rca$pct_data$net_adj_pct,
                       "Net Adjustment %",
                       "Net Adj. %", "#5e81ac")
  })

  output$rca_gross_pct_plot <- renderPlot({
    req(rv_rca$pct_data)
    rca_pct_histogram_(rv_rca$pct_data$gross_adj_pct,
                       "Gross Adjustment %",
                       "Gross Adj. %", "#a3be8c")
  })

  # --- 9. Generate Sales Grid & Download ---
  # Step 1: Button click shows modal with recommended comps
  observeEvent(input$sales_grid_btn, {
    req(rv_rca$rca_df)
    rca <- rv_rca$rca_df
    n_total <- nrow(rca)
    if (n_total < 2) {
      showNotification("Need at least 2 rows (subject + 1 comp).",
                       type = "error", duration = 8)
      return()
    }

    has_gross_pct <- "gross_adjustments" %in% colnames(rca) &&
                     "sale_price" %in% colnames(rca)

    # Find sale_age column from specials
    cfg <- var_config_r()
    sg_specials <- cfg$specials

    # Warn about missing special types needed for the sales grid
    required_specials <- c("contract_date", "living_area")
    recommended_specials <- c("latitude", "longitude", "lot_size")
    designated <- if (!is.null(sg_specials)) unique(sg_specials) else character(0)
    missing_req <- setdiff(required_specials, designated)
    missing_rec <- setdiff(recommended_specials, designated)
    if (length(missing_req) > 0L) {
      showNotification(
        paste0("Sales Grid: required special types not designated: ",
               paste(missing_req, collapse = ", "),
               ". Set these in Variable Configuration > Special column."),
        type = "warning", duration = 15)
    }
    if (length(missing_rec) > 0L) {
      showNotification(
        paste0("Sales Grid: recommended special types not designated: ",
               paste(missing_rec, collapse = ", "),
               ". Grid will work but some fields will be blank."),
        type = "message", duration = 10)
    }
    sa_col_name <- "sale_age"
    if (!is.null(sg_specials)) {
      sa_idx <- which(sg_specials == "sale_age")
      if (length(sa_idx) > 0L) sa_col_name <- names(sg_specials)[sa_idx[1L]]
    }

    wt_col <- if (!is.null(cfg$weights_col) && cfg$weights_col %in% colnames(rca))
      rca[[cfg$weights_col]] else rep(1, n_total)

    # Build comp info table (rows 2..n_total with weight > 0)
    response <- cfg$response
    comp_info <- data.frame(
      row       = 2:n_total,
      address   = if ("street_address" %in% colnames(rca))
                    rca[["street_address"]][2:n_total] else rep("", n_total - 1),
      sale_price = if (response %in% colnames(rca))
                     rca[[response]][2:n_total] else rep(NA, n_total - 1),
      sale_age  = if (sa_col_name %in% colnames(rca))
                    rca[[sa_col_name]][2:n_total] else rep(NA, n_total - 1),
      weight    = wt_col[2:n_total],
      gross_adj = if ("gross_adjustments" %in% colnames(rca))
                    rca[["gross_adjustments"]][2:n_total] else rep(0, n_total - 1),
      stringsAsFactors = FALSE
    )

    comp_info$gross_adj_pct <- ifelse(
      !is.na(comp_info$sale_price) & comp_info$sale_price != 0,
      abs(comp_info$gross_adj / comp_info$sale_price),
      NA
    )

    eligible <- comp_info[!is.na(comp_info$weight) & comp_info$weight > 0, ]
    eligible <- eligible[order(eligible$gross_adj_pct, na.last = TRUE), ]

    recommended <- eligible[!is.na(eligible$gross_adj_pct) &
                            eligible$gross_adj_pct < 0.25, ]
    recommended <- recommended[order(recommended$sale_age, na.last = TRUE), ]
    if (nrow(recommended) > 30) recommended <- recommended[1:30, ]

    others <- eligible[is.na(eligible$gross_adj_pct) |
                       eligible$gross_adj_pct >= 0.25, ]
    others <- others[order(others$gross_adj_pct, na.last = TRUE), ]

    rv_rca$sg_recommended <- recommended
    rv_rca$sg_others <- others

    rec_checks <- if (nrow(recommended) > 0) {
      lapply(seq_len(nrow(recommended)), function(i) {
        r <- recommended[i, ]
        lbl <- sprintf("Row %d | %s | SP: $%s | Age: %s | Gross: %.1f%%",
                       r$row,
                       substr(as.character(r$address), 1, 30),
                       formatC(r$sale_price, format = "f", digits = 0,
                               big.mark = ","),
                       as.character(r$sale_age),
                       r$gross_adj_pct * 100)
        tags$div(
          checkboxInput(paste0("sg_rec_", r$row), lbl, value = TRUE),
          style = "margin-bottom: 0px;"
        )
      })
    } else {
      tags$p("No comps with gross adjustment < 25% found.",
             style = "color: var(--bs-secondary-color);")
    }

    other_checks <- if (nrow(others) > 0) {
      lapply(seq_len(min(nrow(others), 50)), function(i) {
        r <- others[i, ]
        pct_str <- if (!is.na(r$gross_adj_pct)) {
          sprintf("%.1f%%", r$gross_adj_pct * 100)
        } else "N/A"
        lbl <- sprintf("Row %d | %s | SP: $%s | Age: %s | Gross: %s",
                       r$row,
                       substr(as.character(r$address), 1, 30),
                       formatC(r$sale_price, format = "f", digits = 0,
                               big.mark = ","),
                       as.character(r$sale_age),
                       pct_str)
        tags$div(
          checkboxInput(paste0("sg_rec_", r$row), lbl, value = FALSE),
          style = "margin-bottom: 0px;"
        )
      })
    } else NULL

    showModal(modalDialog(
      title = "Sales Grid \u2014 Select Comparables (max 30)",
      size = "l",
      tags$div(
        style = "max-height: 500px; overflow-y: auto;",
        tags$h5(paste0("Recommended Comps (gross adj < 25%, ",
                       "sorted by sale age) \u2014 ",
                       nrow(recommended), " found")),
        rec_checks,
        if (!is.null(other_checks)) {
          tagList(
            hr(),
            tags$h5("Additional Comps (gross adj >= 25%)"),
            other_checks
          )
        }
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("sg_confirm", "Generate Sales Grid",
                     class = "btn-primary")
      )
    ))
  })

  # Step 2: Confirm button in modal — generate the grid
  observeEvent(input$sg_confirm, {
    req(rv_rca$rca_df)
    removeModal()

    all_candidate_rows <- c(
      if (!is.null(rv_rca$sg_recommended) && nrow(rv_rca$sg_recommended) > 0)
        rv_rca$sg_recommended$row else integer(0),
      if (!is.null(rv_rca$sg_others) && nrow(rv_rca$sg_others) > 0)
        rv_rca$sg_others$row[seq_len(min(nrow(rv_rca$sg_others), 50))]
        else integer(0)
    )
    comp_rows <- integer(0)
    for (r in all_candidate_rows) {
      cb_val <- input[[paste0("sg_rec_", r)]]
      if (!is.null(cb_val) && isTRUE(cb_val)) {
        comp_rows <- c(comp_rows, r)
      }
    }

    if (length(comp_rows) == 0) {
      showNotification("No comps selected.", type = "warning", duration = 8)
      return()
    }
    if (length(comp_rows) > 30) {
      comp_rows <- comp_rows[1:30]
      showNotification("Capped at 30 comps.", type = "warning", duration = 5)
    }

    # Sort selected comps by gross_adj_pct ascending
    rca <- rv_rca$rca_df
    response <- var_config_r()$response
    sp <- if (response %in% colnames(rca)) rca[[response]][comp_rows]
          else rep(NA, length(comp_rows))
    gross <- if ("gross_adjustments" %in% colnames(rca))
               rca[["gross_adjustments"]][comp_rows] else rep(0, length(comp_rows))
    gap <- ifelse(!is.na(sp) & sp != 0, abs(gross / sp), NA)
    comp_rows <- comp_rows[order(gap, na.last = TRUE)]

    folder <- input$output_folder
    if (is.null(folder) || !nzchar(folder)) folder <- path.expand("~/Downloads")
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)

    out_path <- file.path(folder, paste0("SalesGrid_",
                          format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))

    message("mgcvUI: Sales grid with ", length(comp_rows),
            " comps (rows: ", paste(comp_rows, collapse = ","), ")")

    tryCatch({
      tmp_adj <- tempfile(fileext = ".xlsx")
      # Ensure all columns are atomic (no list columns)
      rca_export <- rv_rca$rca_df
      for (cn in names(rca_export)) {
        if (is.list(rca_export[[cn]])) {
          rca_export[[cn]] <- vapply(rca_export[[cn]], paste, character(1),
                                     collapse = ",")
        }
      }
      writexl::write_xlsx(rca_export, tmp_adj)
      message("mgcvUI: wrote RCA temp file: ", tmp_adj,
              " (", nrow(rca_export), " rows, ", ncol(rca_export), " cols)")

      grid_script <- system.file("app", "sales_grid.R", package = "mgcvUI")
      if (!nzchar(grid_script)) {
        showNotification("Sales grid script not found in package.",
                         type = "error", duration = 10)
        return()
      }
      source(grid_script, local = TRUE)

      # Build specials named list from designations
      cfg <- var_config_r()
      sg_specials_map <- list()
      if (!is.null(cfg$specials)) {
        for (nm in names(cfg$specials)) {
          sp_type <- cfg$specials[[nm]]
          if (sp_type != "no") sg_specials_map[[sp_type]] <- nm
        }
      }

      n_comp <- length(comp_rows)
      withProgress(
        message = "Generating Sales Grid",
        detail = sprintf("0 of %d comps processed", n_comp),
        value = 0, {
        generate_sales_grid(
          adjusted_file     = tmp_adj,
          comp_rows         = comp_rows,
          output_file       = out_path,
          specials          = sg_specials_map,
          progress_fn       = function(sheet, total_sheets,
                                       comps_done, total_comps) {
            setProgress(
              value = comps_done / total_comps,
              detail = sprintf("Sheet %d of %d \u2014 %d of %d comps processed",
                               sheet, total_sheets, comps_done, total_comps))
          }
        )
      })
      unlink(tmp_adj)

      showNotification(paste0("Sales grid saved to: ", out_path,
                              " (", length(comp_rows), " comps, ",
                              ceiling(length(comp_rows) / 3), " sheets)"),
                       type = "message", duration = 10)
      session$sendCustomMessage("download_check",
        list(id = "sales_grid_btn"))
    }, error = function(e) {
      showNotification(paste("Sales grid error:", e$message),
                       type = "error", duration = 10)
    })
  })

  # --- 10. Download Report (to output folder) ---
  observeEvent(input$export_report_btn, {
    req(gam_result_r())

    folder <- input$output_folder
    if (is.null(folder) || !nzchar(folder)) folder <- path.expand("~/Downloads")
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)

    fmt <- input$export_format
    ext <- paste0(".", fmt)
    base <- tools::file_path_sans_ext(data_mod$filename() %||% "mgcvui")
    out_path <- file.path(folder, paste0(base, "_report_",
                          format(Sys.time(), "%Y%m%d_%H%M%S"), ext))

    showNotification("Generating report...", type = "message",
                     duration = 3, id = "report_progress")
    tryCatch({
      res   <- gam_result_r()
      model <- res$model
      message("mgcvUI: generating ", fmt, " report to ", out_path)

      if (fmt == "docx") {
        # Word document via officer
        font_fam <- mgcv_font_family_()
        tmpdir <- tempdir()

        # Smooth plots (only univariate s() terms)
        plotted <- character(0)
        for (spec in res$smooth_specs) {
          if (spec$type != "s" || length(spec$vars) != 1L) next
          var <- spec$vars[1]
          if (var %in% plotted) next
          p <- tryCatch(
            plot_smooth_single(res, var, earth_knots = res$earth_knots),
            error = function(e) {
              message("  report plot error for '", var, "': ", e$message)
              NULL
            }
          )
          if (!is.null(p)) {
            ggplot2::ggsave(file.path(tmpdir, paste0("smooth_", var, ".png")),
                            p, width = 7, height = 4, dpi = 150)
            plotted <- c(plotted, var)
          }
        }

        # Diagnostics
        p_diag <- tryCatch(plot_diagnostics(res), error = function(e) NULL)
        if (!is.null(p_diag)) {
          ggplot2::ggsave(file.path(tmpdir, "diagnostics.png"), p_diag,
                          width = 8, height = 6, dpi = 150)
        }

        # Actual vs predicted
        p_avp <- tryCatch(plot_actual_vs_predicted(res), error = function(e) NULL)
        if (!is.null(p_avp)) {
          ggplot2::ggsave(file.path(tmpdir, "actual_vs_predicted.png"), p_avp,
                          width = 7, height = 5, dpi = 150)
        }

        # Build Word document
        doc <- officer::read_docx()
        doc <- officer::body_add_par(doc, "mgcvUI GAM Report",
                                     style = "heading 1")
        doc <- officer::body_add_par(doc, paste("Date:",
                                     format(Sys.time(), "%Y-%m-%d %H:%M")))
        doc <- officer::body_add_par(doc, "")

        # Model summary
        doc <- officer::body_add_par(doc, "Model Summary", style = "heading 2")
        summ <- format_gam_summary(res)
        doc <- officer::body_add_par(doc, paste("R-squared:",
                                     round(summ$r_squared, 4)))
        doc <- officer::body_add_par(doc, paste("Deviance explained:",
                                     round(summ$dev_explained * 100, 1), "%"))
        doc <- officer::body_add_par(doc, paste("AIC:", round(summ$aic, 1)))
        doc <- officer::body_add_par(doc, paste("n:", summ$n_obs))
        if (!is.null(summ$cv_rsq)) {
          doc <- officer::body_add_par(doc, paste("CV R-squared:",
                                       round(summ$cv_rsq, 4)))
        }
        doc <- officer::body_add_par(doc, paste("Family:", summ$family))
        doc <- officer::body_add_par(doc, paste("Method:", summ$method))
        doc <- officer::body_add_par(doc, paste("Formula:",
                                     deparse(res$formula, width.cutoff = 500)))

        # Smooth terms table
        if (nrow(summ$smooth_table) > 0) {
          doc <- officer::body_add_par(doc, "Smooth Terms", style = "heading 2")
          doc <- officer::body_add_table(doc, value = summ$smooth_table,
                                         style = "table_template")
        }

        # Parametric terms table
        if (nrow(summ$parametric_table) > 0) {
          doc <- officer::body_add_par(doc, "Parametric Terms",
                                       style = "heading 2")
          doc <- officer::body_add_table(doc, value = summ$parametric_table,
                                         style = "table_template")
        }

        # Plots
        doc <- officer::body_add_par(doc, "Smooth Plots", style = "heading 2")
        for (var in plotted) {
          fp <- file.path(tmpdir, paste0("smooth_", var, ".png"))
          if (file.exists(fp)) {
            doc <- officer::body_add_img(doc, src = fp, width = 6, height = 3.5)
            doc <- officer::body_add_par(doc, "")
          }
        }

        doc <- officer::body_add_par(doc, "Diagnostics", style = "heading 2")
        fp <- file.path(tmpdir, "diagnostics.png")
        if (file.exists(fp)) {
          doc <- officer::body_add_img(doc, src = fp, width = 7, height = 5.25)
        }

        doc <- officer::body_add_par(doc, "Actual vs Predicted",
                                     style = "heading 2")
        fp <- file.path(tmpdir, "actual_vs_predicted.png")
        if (file.exists(fp)) {
          doc <- officer::body_add_img(doc, src = fp, width = 6, height = 4.3)
        }

        print(doc, target = out_path)

      } else {
        # HTML or PDF via rmarkdown
        rmd_template <- system.file("rmd", "gam_report.Rmd",
                                     package = "mgcvUI")
        if (nzchar(rmd_template)) {
          tmpdir <- tempdir()
          rmd_copy <- file.path(tmpdir, "gam_report.Rmd")
          file.copy(rmd_template, rmd_copy, overwrite = TRUE)

          # Save result to temp RDS for template
          rds_path <- file.path(tmpdir, "gam_result.rds")
          saveRDS(res, rds_path)

          out_fmt <- if (fmt == "html") {
            rmarkdown::html_document()
          } else {
            rmarkdown::pdf_document()
          }

          tmp_out <- file.path(tmpdir, paste0("gam_report", ext))
          rmarkdown::render(
            rmd_copy,
            output_format = out_fmt,
            output_file = tmp_out,
            params = list(result_path = rds_path,
                          title = "mgcvUI GAM Report"),
            envir = new.env(parent = globalenv()),
            quiet = TRUE
          )
          file.copy(tmp_out, out_path, overwrite = TRUE)
        } else {
          stop("Report template not found.")
        }
      }

      session$sendCustomMessage("download_check",
                                list(id = "export_report_btn"))
      showNotification(paste0("Report saved to: ", out_path),
                       type = "message", duration = 8)
    }, error = function(e) {
      showNotification(paste("Report error:", e$message),
                       type = "error", duration = 10)
    })
  })
}

shinyApp(ui, server)
