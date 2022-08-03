library(shiny)
library(formattable)
library(lubridate)
library(bslib)
library(readr)
library(DT)
library(rb3)
library(oplib)
library(bizdays)
library(tidyverse)
library(plotly)

.theme <- bs_theme(
  fg = "#fff",
  bg = "#000"
)

PARAMS <- list(
  refdate = getdate("last bizday", Sys.Date(), "Brazil/B3")
)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  theme = .theme,
  titlePanel("Volatilidade Implícita de Opções de Ações"),
  fluidRow(
    div(span("1", style = "color: black;")),
    column(2, dateInput("refdate", "Data de referência", PARAMS$refdate)),
    column(2, selectInput("underlyingSymbol", "Ações", "")),
    column(
      2,
      checkboxGroupInput(
        "optionType", "Tipo", c("Call", "Put"), c("Call", "Put"), TRUE
      )
    ),
    column(
      6, checkboxGroupInput("optionMaturity", "Vencimento", "", NULL, TRUE)
    )
  ),
  br(),
  tabsetPanel(
    tabPanel(
      "Opções",
      div(span("1", style = "color: black;")),
      fluidRow(
        dataTableOutput("tableOptions")
      )
    ),
    tabPanel(
      "Volatilidade Implícita",
      div(span("1", style = "color: black;")),
      fluidRow(
        column(5,
          radioButtons(
            "strikeOrDelta", "Eixo x (Strike ou Delta)",
            c("Strike", "Delta"), "Strike",
            inline = TRUE
          ),
          offset = 1
        ),
        column(
          5,
          radioButtons(
            "volOrVar", "Eixo y (Volatilidade ou Variância Total)",
            c("Volatilidade" = "vol", "Variância Total" = "var"), "vol",
            inline = TRUE
          )
        )
      ),
      fluidRow(
        column(10, plotlyOutput("plotImpvol"), offset = 1)
      )
    ),
  ),
)


server <- function(input, output, session) {
  optionsSuperset <- reactive({
    refdate <- input$refdate
    ch <- cotahist_get(refdate, "daily")
    yc <- yc_get(refdate)
    cotahist_equity_options_superset(ch, yc)
  })

  refdate <- reactive({
    op <- optionsSuperset()
    op$refdate[1]
  })

  optionsData <- reactive({
    validate(
      need(input$underlyingSymbol != "", "Símbolo não definido"),
      need(any(input$optionType != ""), "Tipo não definido"),
      need(input$optionMaturity != "", "Vencimento não definido")
    )
    op <- optionsSuperset()
    op1 <- op |>
      filter(
        symbol.underlying == input$underlyingSymbol
      )

    close_underlying <- op1$close.underlying[1]
    op1 <- op1 |>
      filter(
        maturity_date %in% as.Date(input$optionMaturity),
        type %in% input$optionType
      )
    validate(
      need(nrow(op1) > 0, "Nenhuma opção selecionada")
    )
    op1 |>
      mutate(
        biz_days = bizdays(
          refdate, following(maturity_date, "Brazil/ANBIMA"), "Brazil/ANBIMA"
        ),
        time_to_maturity = biz_days / 252,
        rate = log(1 + r_252),
        bsm_impvol = bsmimpvol(
          close, type, close.underlying, strike, time_to_maturity, rate, 0
        ),
        delta = bsmdelta(
          type, close.underlying, strike, time_to_maturity, rate, 0, bsm_impvol
        ),
        vega = bsmvega(
          type, close.underlying, strike, time_to_maturity, rate, 0, bsm_impvol
        ),
        theta = bsmtheta(
          type, close.underlying, strike, time_to_maturity, rate, 0, bsm_impvol
        ),
        rho = bsmrho(
          type, close.underlying, strike, time_to_maturity, rate, 0, bsm_impvol
        )
      )
  })

  output$tableOptions <- renderDataTable(
    {
      n_format <- scales::number_format(accuracy = 0.01)
      optionsData() |>
        select(
          symbol, volume, biz_days, type, strike, close.underlying,
          close, bsm_impvol, delta, vega, theta, rho
        ) |>
        mutate(
          delta = n_format(delta),
          vega = n_format(vega),
          rho = n_format(rho),
          theta = n_format(theta),
          bsm_impvol = n_format(bsm_impvol),
          volume = scales::comma(volume)
        ) |>
        arrange(biz_days, type, strike)
    },
    options = list(pageLength = 1000, info = FALSE)
  )

  output$plotImpvol <- renderPlotly({
    df <- if (input$strikeOrDelta == "Strike") {
      x_lab <- "Strike"
      optionsData() |>
        mutate(x = strike) |>
        filter(!is.na(bsm_impvol))
    } else {
      x_lab <- "Delta"
      optionsData() |>
        mutate(
          x = ifelse(type == "Call", delta, 1 + delta)
        ) |>
        filter(!is.na(bsm_impvol))
    }

    df <- if (input$volOrVar == "vol") {
      y_lab <- "Volatilidade"
      df |>
        mutate(y = bsm_impvol)
    } else {
      y_lab <- "Variância Total"
      df |>
        mutate(y = bsm_impvol * bsm_impvol * biz_days / 252)
    }

    df |>
      ggplot(aes(x = x, y = y, color = factor(biz_days))) +
      geom_point() +
      labs(x = x_lab, y = y_lab, color = "Dias Úteis") +
      facet_wrap(type ~ .)
  })

  observeEvent(input$refdate, {
    op <- optionsSuperset()
    symbols_counts <- op |>
      group_by(symbol.underlying) |>
      count()
    symbols <- symbols_counts$symbol.underlying
    names(symbols) <- paste(
      symbols_counts$symbol.underlying, "-", symbols_counts$n
    )
    updateSelectInput(session, "underlyingSymbol",
      choices = symbols
    )
  })

  observeEvent(input$underlyingSymbol, {
    op <- optionsSuperset()
    op1 <- op |>
      filter(symbol.underlying == input$underlyingSymbol)
    maturities <- unique(op1$maturity_date) |> sort()
    updateCheckboxGroupInput(session, "optionMaturity",
      inline = TRUE,
      choices = maturities,
      selected = maturities[1]
    )
  })
}

shinyApp(
  ui = ui, server = server,
  options = list(launch.browser = TRUE)
)
