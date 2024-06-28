rm(list=ls())

require(xml2)
require(rvest)
require(dplyr)
require(purrr)
require(here)
require(glue)
require(stringr)
require(lubridate)
require(readxl)
require(stringr)
require(lubridate)
require(tidyr)

setwd("S:/Finance/Shared Area/BNSSG - BI/8 Modelling and Analytics/projects/corona/rtt/ode model (2022)/england application/")
dir <- getwd()

year_range <- 2019:(year(now()))


# NOTE: this changes each year and I'm not 100% on the logic of generating the
# right URL
url_stem <- glue::glue("https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-20{(year_range[-length(year_range)] - 2000)}-{(year_range[-1] - 2000)}/")

pathways <- c("Incomplete", "NonAdmitted", "Admitted", "New-Periods") %>%
  purrr::set_names(., nm = .)

urls <- purrr::map(
  url_stem,
  .f = function(stem) purrr::imap(
    pathways,
    ~stringr::str_subset(html_attr(html_nodes(read_html(stem), "a"), "href"), glue::glue("/{.x}-Provider"))
  )
)




read_rtt_trust_data <- function(url,
                                pathway = "Incomplete",
                                year_range = year_range
) {
  per_pat <- glue::glue("-({glue::glue_collapse(month.abb, sep = '|')})-?({glue::glue_collapse((year_range - 2000), sep = '|')})-")
  period <- stringr::str_match(url, per_pat)
  period <- lubridate::ymd(glue::glue("20{period[3]}-{period[2]}-01"))
  period <- lubridate::rollforward(period)
  cols <- c(
    "Region Code",
    "CCG Code",
    "CCG Name",
    "Provider Name",
    "Provider Code",
    "Treatment Function Code",
    "Treatment Function",
    "Total within 18 weeks"
  )
  wait_cols <- c(">0-1", ">1-2", ">2-3", ">3-4",
                 ">4-5", ">5-6", ">6-7",
                 ">7-8", ">8-9", ">9-10",
                 ">10-11", ">11-12", ">12-13",
                 ">13-14", ">14-15", ">15-16", 
                 ">16-17", ">17-18", ">18-19",
                 ">19-20", ">20-21", ">21-22",
                 ">22-23", ">23-24", ">24-25",
                 ">25-26", ">26-27", ">27-28",
                 ">28-29", ">29-30", ">30-31",
                 ">31-32", ">32-33", ">33-34",
                 ">34-35", ">35-36", ">36-37",
                 ">37-38", ">38-39", ">39-40",
                 ">40-41", ">41-42", ">42-43",
                 ">43-44", ">44-45", ">45-46",
                 ">46-47", ">47-48", ">48-49",
                 ">49-50", ">50-51", ">51-52",
                 "52 plus", ">52-53", ">53-54",
                 ">54-55", ">55-56", ">56-57",
                 ">57-58", ">58-59", ">59-60",
                 ">60-61", ">61-62", ">62-63",
                 ">63-64", ">64-65", ">65-66",
                 ">66-67", ">67-68", ">68-69",
                 ">69-70", ">70-71", ">71-72",
                 ">72-73", ">73-74", ">74-75",
                 ">75-76", ">76-77", ">77-78",
                 ">78-79", ">79-80", ">80-81",
                 ">81-82", ">82-83", ">83-84",
                 ">84-85", ">85-86", ">86-87",
                 ">87-88", ">88-89", ">89-90",
                 ">90-91", ">91-92", ">92-93", 
                 ">93-94", ">94-95", ">95-96",
                 ">96-97", ">97-98", ">98-99",
                 ">99-100", ">100-101", ">101-102",
                 ">102-103", ">103-104", "104 plus")
  w_i <- seq(from = 0.5, by = 1, length.out = 105)
  cols_out <- switch(pathway,
                     "Incomplete" = c("Treatment Function",
                                      "Treatment Function Code",
                                      "% within 18 weeks",
                                      "Total number of incomplete pathways",
                                      "Total 52 plus weeks",
                                      wait_cols,
                                      "Average (median) waiting time (in weeks)"),
                     "NonAdmitted" = c("Treatment Function",
                                       "Treatment Function Code",
                                       wait_cols,
                                       ">100-101", ">101-102", ">102-103", ">103-104", "104 plus",
                                       "Total number of completed pathways (all)"),
                     "Admitted"    = c("Treatment Function",
                                       "Treatment Function Code",
                                       wait_cols,
                                       "Total number of completed pathways (all)"),
                     "New-Periods" = c("Treatment Function",
                                       "Treatment Function Code",
                                       "Number of new RTT clock starts during the month")
  )
  
  cols <- switch(pathway,
                 "Incomplete" = c(cols, cols_out),
                 "NonAdmitted" = c(cols, cols_out),
                 "Admitted" = c(cols, cols_out),
                 "New-Periods" = c(cols, cols_out)
  )
  # set the clockstop varname later (as the columns are equally named in the
  # excel files for non-admitted and admitted and we want to keep them separate)
  clckstp_var <- switch(pathway,
                        "Incomplete" = "",
                        "NonAdmitted" = "nonad",
                        "Admitted" = "ad",
                        "New-Periods" = ""
  )
  
  incpl_pthwy_cols_gt18 <- c(">18-19", ">19-20", ">20-21", ">21-22", ">22-23", 
                             ">23-24", ">24-25", ">25-26", ">26-27", ">27-28", ">28-29", ">29-30", 
                             ">30-31", ">31-32", ">32-33", ">33-34", ">34-35", ">35-36", ">36-37", 
                             ">37-38", ">38-39", ">39-40", ">40-41", ">41-42", ">42-43", ">43-44", 
                             ">44-45", ">45-46", ">46-47", ">47-48", ">48-49", ">49-50", ">50-51", 
                             ">51-52", ">52-53", ">53-54", ">54-55", ">55-56", ">56-57", ">57-58", 
                             ">58-59", ">59-60", ">60-61", ">61-62", ">62-63", ">63-64", ">64-65", 
                             ">65-66", ">66-67", ">67-68", ">68-69", ">69-70", ">70-71", ">71-72", 
                             ">72-73", ">73-74", ">74-75", ">75-76", ">76-77", ">77-78", ">78-79", 
                             ">79-80", ">80-81", ">81-82", ">82-83", ">83-84", ">84-85", ">85-86", 
                             ">86-87", ">87-88", ">88-89", ">89-90", ">90-91", ">91-92", ">92-93", 
                             ">93-94", ">94-95", ">95-96", ">96-97", ">97-98", ">98-99", ">99-100", 
                             ">100-101", ">101-102", ">102-103", ">103-104", "104 plus", "52 plus")
  
  tfc_codes <- c("General Surgery" = "(:?C_|[INA]P)?100",
                 "Urology" = "(:?C_|[INA]P)?101",
                 "Trauma and Orthopaedic"= "(:?C_|[INA]P)?110",
                 "Ear Nose and Throat" = "(:?C_|[INA]P)?120",
                 "Ophthalmology" = "(:?C_|[INA]P)?130",
                 "Oral Surgery" = "(:?C_|[INA]P)?140",
                 "Neurosurgical" = "(:?C_|[INA]P)?150",
                 "Plastic Surgery" = "(:?C_|[INA]P)?160",
                 "Cardiothoracic Surgery" = "(:?C_|[INA]P)?170",
                 "General Internal Medicine" ="300",
                 "Gastroenterology" = "(:?C_|[INA]P)?301",
                 "Cardiology" = "(:?C_|[INA]P)?320",
                 "Dermatology" = "(:?C_|[INA]P)?330",
                 "Respiratory Medicine" = "(:?C_|[INA]P)?340",
                 "Neurology" = "(:?C_|[INA]P)?400",
                 "Rheumatology" = "(:?C_|[INA]P)?410",
                 "Elderly Medicine" = "(:?C_|[INA]P)?430",
                 "Gynaecology" = "(:?C_|[INA]P)?502",
                 "Other" = "X0[1-6]",
                 "Total" = "999")
  
  tmp <- tempfile(fileext = ".xls")
  on.exit(unlink(tmp))
  download.file(url,
                destfile = tmp,
                quiet = TRUE,
                mode = "wb")
  
  
  
  read_xls_safe <- purrr::safely(readxl::read_xls)
  dat <- read_xls_safe(path.expand(tmp), sheet = "Provider", skip = 13)
  if(is.null(dat$error)){
    dat <- dat$result
  } else {
    dat <- readxl::read_xlsx(path.expand(tmp), sheet = "Provider", skip = 13)
  }
  
  
  # dat <- dat %>%
  #   dplyr::select(dplyr::any_of(cols)) %>%
  #   dplyr::mutate(dplyr::across(-c("Region Code", 
  #                                  "Provider Name",
  #                                  "Provider Code",
  #                                  "Treatment Function Code",
  #                                  "Treatment Function"),
  #                               .fns = as.numeric)) %>%
  #   dplyr::rowwise() %>%
  #   dplyr::mutate(specialty = names(tfc_codes[stringr::str_detect(`Treatment Function Code`, pattern = tfc_codes)]),
  #                 `Treatment Function Code` = stringr::str_remove_all(`Treatment Function Code`, pattern = "[^0-9]")
  #   ) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::select(tidyselect::contains(cols_out), `Provider Name`, `Treatment Function`, specialty) %>%
  #   dplyr::rename_with(.cols = dplyr::matches("% within 18 weeks"), .fn = ~"rtt_18") %>%
  #   dplyr::rename_with(.cols = dplyr::matches("Total number of incomplete pathways"), .fn = ~"n_wl") %>%
  #   dplyr::rename_with(.cols = dplyr::matches("Number of new RTT clock starts during the month"), .fn = ~"n_clckstrt") %>%
  #   dplyr::rename_with(.cols = dplyr::matches("Average \\(median\\) waiting time \\(in weeks\\)"), .fn = ~"med_wait") %>%
  #   dplyr::rename_with(.cols = dplyr::matches("Total number of completed pathways \\(all\\)"), .fn = ~glue::glue("n_clckstp_{clckstp_var}")) %>%
  #   dplyr::mutate(period = period) 
  
  
  dat <- dat %>%
    dplyr::select(dplyr::any_of(cols)) %>%
    dplyr::mutate(dplyr::across(-c("Region Code", 
                                   "Provider Name",
                                   "Provider Code",
                                   "Treatment Function Code",
                                   "Treatment Function"),
                                .fns = as.numeric)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(specialty = names(tfc_codes[stringr::str_detect(`Treatment Function Code`, pattern = tfc_codes)]),
                  `Treatment Function Code` = stringr::str_remove_all(`Treatment Function Code`, pattern = "[^0-9]")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(tidyselect::contains(cols_out), `Provider Name`, `Treatment Function`, specialty) %>%
    dplyr::mutate(period = period)
  
  if(pathway == "Incomplete") {
    dat <- dat %>%
      dplyr::rename_with(.cols = dplyr::matches("% within 18 weeks"), .fn = ~"rtt_18") %>%
      dplyr::rename_with(.cols = dplyr::matches("Total number of incomplete pathways"), .fn = ~"n_wl") %>%
      dplyr::rename_with(.cols = dplyr::matches("Average \\(median\\) waiting time \\(in weeks\\)"), .fn = ~"med_wait") 
  }
  if(pathway == "New-Periods") {
    dat <- dat %>%
      dplyr::rename_with(.cols = dplyr::matches("Number of new RTT clock starts during the month"), .fn = ~"n_clckstrt")  
  }
  
  if(pathway %in% c("Admitted", "NonAdmitted")) {
    dat <- dat %>%  
      dplyr::rename_with(.cols = dplyr::matches("Total number of completed pathways \\(all\\)"), .fn = ~glue::glue("n_clckstp_{clckstp_var}"))
  }  
  
  
  if(pathway %in% c("NonAdmitted", "Admitted", "Incomplete")){
    dat <- dat %>%
      select(period, `Provider Name`, `Treatment Function`, dplyr::any_of(wait_cols)) %>%
      pivot_longer(cols = dplyr::any_of(wait_cols),
                   names_to = "week_band",
                   values_to = "value") %>%
      # slice(1:105) %>%
      mutate(
        row_id = 1:n(),
        week_val = replace_na(as.numeric(str_extract(week_band, "(?<=-)([0-9]{1,3})")), 105),
        month_break = month(period - lubridate::dweeks(week_val - 1)) != month(period - lubridate::dweeks(week_val)),
        balance_1 = if_else(month_break, as.numeric(day(period - lubridate::dweeks(week_val - 1))), 7),
        balance_2 = if_else(month_break, (rollforward(period - lubridate::dweeks(week_val)) -(period - lubridate::dweeks(week_val)))/ddays(1), 0),
        month_1 = interval(rollforward(period - lubridate::dweeks(week_val - 1)), period) %/% months(1),
        month_2 = interval(rollforward(period - lubridate::dweeks(week_val)), period) %/% months(1),
        value_1 = value * balance_1/7,
        value_2 = value * balance_2/7,
        months.waited_1 = case_when(
          month_1 == 0 ~ "<1",
          month_1 == 23 & month_2 == 24 ~ "24+",
          TRUE ~ unclass(glue::glue("{month_1}-{month_1 + 1}")) # need to drop glue class
        ),
        months.waited_2 = case_when(
          month_2 == 0 ~ "<1",
          month_1 == 23 & month_2 == 24 ~ "24+",
          TRUE ~ unclass(glue::glue("{month_2}-{month_2 + 1}")) # need to drop glue class
        )
      ) %>%
      select(-week_band, -value, -week_val, -month_break, -balance_1, -balance_2, -month_1, -month_2) %>%
      pivot_longer(cols = c(months.waited_1, months.waited_2, value_1, value_2),
                   names_to = c(".value", "var"),
                   names_sep = "_") %>%
      rename(months_waited = months.waited) %>%
      mutate(months_waited = factor(months_waited, levels = c(
        "<1",
        "0-1",
        "1-2",
        "2-3",
        "3-4",
        "4-5",
        "5-6",
        "6-7",
        "7-8",
        "8-9",
        "9-10",
        "10-11",
        "11-12",
        "12-13",
        "13-14",
        "14-15",
        "15-16",
        "16-17",
        "17-18",
        "18-19",
        "19-20",
        "20-21",
        "21-22",
        "22-23",
        "23-24",
        "24+"
      ))) %>%
      group_by(`Provider Name`, `Treatment Function`, period, months_waited) %>%
      summarise(value = sum(value))
  }
  
  if(pathway == "New-Periods"){
   dat <- dat %>%
     mutate(months_waited = NA,
            value = n_clckstrt)
  }
  
  dat <- dat %>%
    dplyr::mutate(type = switch(pathway,
                                "Incomplete" = "Incomplete",
                                "NonAdmitted" = "Complete",
                                "Admitted" = "Complete",
                                "New-Periods" = "Referrals")
                  ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      trust = `Provider Name`,
      specialty = `Treatment Function`,
      period,
      type,
      months_waited,
      value
      )
  dat
}

# future::plan(future::multisession, workers = parallel::detectCores() - 8)

rtt_dat <-
  purrr::map(urls,
             .f = function(year)
               purrr::imap(
                 year,
                 .f = function(pth, name = .y)
                   # furrr::future_map_dfr(pth,
                   purrr::map_dfr(pth,
                                  ~ read_rtt_trust_data(.x,
                                                        pathway = name,
                                                        year_range = year_range)
                   )
               )
  )  %>%
  purrr::map_df(purrr::reduce, dplyr::full_join) 



readr::write_csv(rtt_dat, here::here(dir, "calc-dat-monthly.csv"))
