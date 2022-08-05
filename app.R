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
    tabPanel(
      "Fit Modelo Corrado-Su",
      div(span("1", style = "color: black;")),
      fluidRow(
        column(1, actionButton("fitCorradoSuModel", "Fit"), offset = 1),
        column(
          2,
          radioButtons(
            "strikeOrDelta_CS", "Eixo x (Strike ou Delta)",
            c("Strike", "Delta"), "Strike",
            inline = TRUE
          )
        ),
        column(3, tableOutput("csmParams"))
      ),
      fluidRow(
        column(10, plotlyOutput("plotImpvol_CS"), offset = 1)
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

  output$plotImpvol_CS <- renderPlotly({
    validate(
      need(input$optionMaturity != "", "Vencimento não definido"),
      need(length(input$optionMaturity) == 1, "Selecionar apenas 1 vencimento"),
      need(csmParams(), "Rodar parâmetros")
    )
    op <- csmOptionsData()
    params <- csmParams()
    strike_rng <- range(op$strike)
    gen_data <- tibble(
      type = "Call",
      close.underlying = op$close.underlying[1],
      strike = seq(strike_rng[1], strike_rng[2], length.out = 500),
      time_to_maturity = op$time_to_maturity[1],
      rate = op$rate[1]
    )
    gen_data <- gen_data |>
      mutate(
        theo_price = csmprice(
          type, close.underlying, strike, time_to_maturity, rate, 0,
          as.numeric(params[1]), as.numeric(params[2]), as.numeric(params[3])
        ),
        csm_impvol = bsmimpvol(
          theo_price, type, close.underlying, strike, time_to_maturity, rate, 0
        ),
        delta = csmdelta(
          type, close.underlying, strike, time_to_maturity, rate, 0,
          as.numeric(params[1]), as.numeric(params[2]), as.numeric(params[3])
        )
      )

    df <- if (input$strikeOrDelta_CS == "Strike") {
      x_lab <- "Strike"
      op |>
        mutate(x = strike) |>
        filter(!is.na(bsm_impvol))
    } else {
      x_lab <- "Delta"
      op |>
        mutate(
          x = ifelse(type == "Call", delta, 1 + delta)
        ) |>
        filter(!is.na(bsm_impvol))
    }

    df_gen_data <- if (input$strikeOrDelta_CS == "Strike") {
      gen_data |>
        mutate(x = strike)
    } else {
      gen_data |>
        mutate(x = delta)
    }

    df |>
      ggplot(aes(x = x, y = bsm_impvol, colour = type)) +
      geom_point() +
      labs(x = x_lab, y = "Volatilidade") +
      geom_line(aes(x = x, y = csm_impvol), data = df_gen_data, colour = "black")
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

  csmParams <- reactiveVal()

  output$csmParams <- renderUI({
    validate(
      need(csmParams(), "Rodar parâmetros")
    )
    params <- csmParams()
    p(
      strong("sigma = "), format(params["sigma"], digits = 4),
      strong("mu3 = "),   format(params["mu3"], digits = 4),
      strong("mu4 = "),   format(params["mu4"], digits = 4)
    )
  })

  csmOptionsData <- reactive({
    op <- optionsData()
    params <- csmParams()
    op |>
      mutate(
        theo_price = csmprice(
          type, close.underlying, strike, time_to_maturity, rate, 0,
          params[1], params[2], params[3]
        ),
        csm_impvol = bsmimpvol(
          theo_price, type, close.underlying, strike, time_to_maturity, rate, 0
        )
      )
  })

  observeEvent(input$fitCorradoSuModel, {
    if (input$optionMaturity == "" || length(input$optionMaturity) > 1) {
      return(NULL)
    }
    op <- optionsData()
    params <- with(op, {
      csm_fit_min_price(
        par = c(0.1, 0, 3),
        type, close.underlying, strike, rate, 0, time_to_maturity, close, 1,
        control = list(trace = 3)
      )
    })

    csmParams(params)
  })
}

shinyApp(
  ui = ui, server = server,
  options = list(launch.browser = TRUE)
)
