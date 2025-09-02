# app.R — Correos (un solo Excel maestro + subcapítulos por capítulo)

# ---- Paquetes ----
library(shiny)
library(bslib)
library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(stringr)
library(glue)
library(DT)
library(here)
library(blastula)

`%||%` <- function(a,b) if (is.null(a) || (is.atomic(a)&&length(a)==1&&is.na(a))) b else a

# ---- Utilidades ----
normalize_subs <- function(x){
  req(nrow(x) > 0)
  x <- janitor::clean_names(x)

  # Asegurar columnas
  if (!"capitulo_num"       %in% names(x)) x$capitulo_num       <- NA_integer_
  if (!"subcapitulo"        %in% names(x)) x$subcapitulo        <- NA_character_
  if (!"titulo_subcapitulo" %in% names(x)) x$titulo_subcapitulo <- NA_character_
  if (!"fecha_inicio"       %in% names(x)) x$fecha_inicio       <- NA
  if (!"fecha_fin"          %in% names(x)) x$fecha_fin          <- NA
  if (!"avance"             %in% names(x)) x$avance             <- NA_real_
  if (!"comentarios"        %in% names(x)) x$comentarios        <- NA_character_

  cap_from_capitulo <- if ("capitulo" %in% names(x)) {
    suppressWarnings(as.integer(stringr::str_extract(as.character(x$capitulo), "[0-9]+")))
  } else NA_integer_

  x %>%
    mutate(
      capitulo_num = dplyr::coalesce(
        suppressWarnings(as.integer(.data$capitulo_num)),
        cap_from_capitulo,
        suppressWarnings(as.integer(stringr::str_extract(as.character(.data$subcapitulo), "^[0-9]+")))
      ),
      subcapitulo  = as.character(.data$subcapitulo),
      fecha_inicio = suppressWarnings(as.Date(.data$fecha_inicio)),
      fecha_fin    = suppressWarnings(as.Date(.data$fecha_fin)),
      avance       = suppressWarnings(as.numeric(.data$avance))
    ) %>%
    select(capitulo_num, subcapitulo, titulo_subcapitulo,
           fecha_inicio, fecha_fin, avance, comentarios)
}

fmt_date   <- function(x){ y <- suppressWarnings(as.Date(x)); ifelse(is.na(y), "", format(y, "%Y-%m-%d")) }
fmt_avance <- function(x){ x <- suppressWarnings(as.numeric(x)); ifelse(is.na(x), "", ifelse(x<=1, paste0(round(x*100), "%"), paste0(round(x), "%"))) }
fmt_fase   <- function(x) x %||% ""

compose_body_md <- function(cap_txt, titulo, autor, coaut, sub_tbl, fases){
  subs_md <- if (nrow(sub_tbl) == 0) {
    "—"
  } else {
    paste(apply(sub_tbl, 1, function(rr){
      parts <- c(rr[["titulo_subcapitulo"]] %||% "")
      fi <- fmt_date(rr[["fecha_inicio"]]); ff <- fmt_date(rr[["fecha_fin"]])
      if (nzchar(fi) || nzchar(ff)) parts <- c(parts, sprintf("(%s — %s)", ifelse(nzchar(fi), fi, "?"), ifelse(nzchar(ff), ff, "?")))
      av <- fmt_avance(rr[["avance"]]); if (nzchar(av)) parts <- c(parts, paste0("[avance: ", av, "]"))
      com <- rr[["comentarios"]] %||% ""; if (nzchar(trimws(com))) parts <- c(parts, paste0(" — ", com))
      paste0("- ", paste(parts, collapse = " "))
    }), collapse = "\n")
  }

  glue("
**Asunto:** Libro de Estadística – Encargo de {cap_txt}: {titulo}

Estimado/a **{autor}**{ifelse(nchar(coaut)>0, glue(' (con {coaut})'), '')},

- **{cap_txt} – {titulo}**
- **Subcapítulos sugeridos:**
{subs_md}

**Hitos por fases (referenciales):**
- Fase 1: {fases[1]}
- Fase 2: {fases[2]}
- Fase 3: {fases[3]}
- Fase 4: {fases[4]}
- Fase 5: {fases[5]}

> **Importante:** En esta fase **no se requieren ejercicios**.
> Solo necesitamos el **desarrollo completo del capítulo**.
")
}

# ---- UI ----
theme <- bs_theme(bootswatch = "flatly", base_font = font_google("Inter"))

ui <- page_navbar(
  theme = theme,
  title = "Envío de correos — Libro de Estadística",

  nav_panel(
    "Cargar & revisar",
    layout_columns(
      col_widths = c(4,8),
      card(
        card_header("1) Fuente de datos"),
        HTML("
<p><b>Archivo único requerido:</b> <code>data/Cronograma_Libro_Estadistica_CON_INDICE.xlsx</code></p>
<p><b>Hojas esperadas:</b> <code>Capitulos</code> (índice) y <code>Subcapitulos</code> (detalle)</p>
        "),
        actionButton("reload", "🔄 Recargar", class = "btn btn-primary"),
        br(), br(),
        strong("Estado de lectura:"),
        verbatimTextOutput("origen_txt", placeholder = TRUE)
      ),
      card(
        card_header("2) Índice (vista previa)"),
        DTOutput("tbl_idx")
      )
    )
  ),

  nav_panel(
    "Redactar & enviar",
    layout_columns(
      col_widths = c(4,8),
      card(
        card_header("3) Selección y opciones"),
        uiOutput("pick_row"),
        checkboxInput("cc_coautor", "CC al coautor", value = TRUE),
        checkboxInput("adj_rubrica", "Adjuntar rúbrica (docs/Rubrica_Capitulo.pdf)", value = TRUE),
        checkboxInput("modo_prueba", "Modo prueba (guardar HTML en outbox/ y no enviar)", value = TRUE),
        textInput("from_name", "Remitente (nombre para el correo)", value = "Coordinación Libro de Estadística"),
        actionButton("send_btn", "✉️ Enviar / Generar", class = "btn btn-success")
      ),
      card(
        card_header("4) Vista previa del correo"),
        htmlOutput("preview_html")
      ),
      card(
        card_header("Registro / Log"),
        verbatimTextOutput("send_log", placeholder = TRUE)
      ),
      card(
        card_header("Subcapítulos (capítulo seleccionado)"),
        DTOutput("tbl_subs_cap")
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session){

  idx <- reactiveVal(NULL)
  subs_all <- reactiveVal(NULL)
  estado <- reactiveVal("—")
  sendlog <- reactiveVal("—")

  load_data <- function(){
    cron_path <- here::here("data","Cronograma_Libro_Estadistica_CON_INDICE.xlsx")
    validate(need(file.exists(cron_path),
                  "Falta: data/Cronograma_Libro_Estadistica_CON_INDICE.xlsx"))

    sh <- excel_sheets(cron_path)
    validate(need("Capitulos"    %in% sh, "No se encontró la hoja 'Capitulos'"))
    validate(need("Subcapitulos" %in% sh, "No se encontró la hoja 'Subcapitulos'"))

    # Índice
    df <- read_excel(cron_path, sheet = "Capitulos") %>% clean_names()

    # Normalizar nombres habituales
    if (!"capitulo_num" %in% names(df)) {
      df$capitulo_num <- suppressWarnings(as.integer(str_extract(df$capitulo, "[0-9]+")))
    }
    if ("principal"        %in% names(df)) df$autor_principal  <- df$principal
    if ("principal_correo" %in% names(df)) df$correo_principal <- df$principal_correo
    if ("coautor_correo"   %in% names(df)) df$correo_coautor   <- df$coautor_correo

    # Subcapítulos
    subs_raw <- read_excel(cron_path, sheet = "Subcapitulos")
    subs <- normalize_subs(subs_raw)

    validate(need(!any(is.na(subs$capitulo_num)),
      "No pude inferir capitulo_num en la hoja 'Subcapitulos'.
       Incluye capitulo_num, o capitulo con número, o subcapitulo con prefijo N.N."))

    subs_all(subs)
    estado(glue("OK: {nrow(subs)} subcapítulos leídos"))

    resumen <- subs %>%
      group_by(capitulo_num) %>%
      summarise(subcapitulos = paste(titulo_subcapitulo, collapse = "; "), .groups = "drop")

    df <- df %>% left_join(resumen, by = "capitulo_num")
    idx(df)
  }

  observeEvent(TRUE, load_data(), once = TRUE)
  observeEvent(input$reload, load_data())
  output$origen_txt <- renderText(estado())

  output$tbl_idx <- renderDT({
    req(idx()); datatable(idx(), options = list(pageLength = 10, scrollX = TRUE), rownames = TRUE)
  })

  # >>> Selector de capítulos (sin updateSelectInput, se crea con choices aquí)
  output$pick_row <- renderUI({
    req(idx())
    df <- idx()
    labs <- ifelse(!is.na(df$capitulo) & nzchar(df$capitulo),
                   paste0(df$capitulo, " — ", df$titulo_capitulo),
                   paste0("Cap ", df$capitulo_num, " — ", df$titulo_capitulo))
    selectInput(
      "row_pick", "Capítulo",
      choices = setNames(seq_len(nrow(df)), labs),
      width = "100%"
    )
  })

  # Vista previa
  output$preview_html <- renderUI({
    req(idx(), subs_all(), input$row_pick)
    r <- as.integer(input$row_pick)
    row <- idx()[r, , drop = FALSE]

    cap_txt <- row$capitulo %||% paste("Cap", row$capitulo_num)
    cap_num <- suppressWarnings(as.integer(str_extract(cap_txt, "[0-9]+")))
    if (is.na(cap_num)) cap_num <- suppressWarnings(as.integer(row$capitulo_num))

    titulo <- row$titulo_capitulo %||% ""
    autor  <- row$autor_principal %||% ""
    coaut  <- row$coautor %||% ""

    fases <- c(fmt_fase(row$fase_1_fin), fmt_fase(row$fase_2_fin), fmt_fase(row$fase_3_fin),
               fmt_fase(row$fase_4_fin), fmt_fase(row$fase_5_fin))

    sub_tbl <- subs_all() %>% filter(capitulo_num == cap_num) %>% arrange(subcapitulo)

    subject <- glue("Libro de Estadística – {cap_txt}: {titulo}")
    body_md <- compose_body_md(cap_txt, titulo, autor, coaut, sub_tbl, fases)

    HTML(glue("<h4>Asunto</h4><p>{subject}</p><h4>Cuerpo</h4><pre style='white-space:pre-wrap'>{body_md}</pre>"))
  })

  output$tbl_subs_cap <- renderDT({
    req(idx(), subs_all(), input$row_pick)
    r <- as.integer(input$row_pick)
    row <- idx()[r, , drop = FALSE]
    cap_num <- suppressWarnings(as.integer(str_extract(row$capitulo, "[0-9]+")))
    if (is.na(cap_num)) cap_num <- suppressWarnings(as.integer(row$capitulo_num))
    sub_tbl <- subs_all() %>% filter(capitulo_num == cap_num) %>% arrange(subcapitulo)
    datatable(sub_tbl, options = list(pageLength = 10, scrollX = TRUE))
  })

  # Envío / Modo prueba
  observeEvent(input$send_btn, {
    req(idx(), subs_all(), input$row_pick)
    r <- as.integer(input$row_pick)
    row <- idx()[r, , drop = FALSE]

    cap_txt <- row$capitulo %||% paste("Cap", row$capitulo_num)
    cap_num <- suppressWarnings(as.integer(str_extract(cap_txt, "[0-9]+")))
    if (is.na(cap_num)) cap_num <- suppressWarnings(as.integer(row$capitulo_num))

    titulo   <- row$titulo_capitulo %||% ""
    autor    <- row$autor_principal %||% ""
    coaut    <- row$coautor %||% ""
    to_email <- row$correo_principal %||% ""
    cc_email <- if (isTRUE(input$cc_coautor)) (row$correo_coautor %||% "") else ""

    validate(need(nzchar(to_email), "Este capítulo no tiene correo principal (correo_principal)."))

    fases <- c(fmt_fase(row$fase_1_fin), fmt_fase(row$fase_2_fin), fmt_fase(row$fase_3_fin),
               fmt_fase(row$fase_4_fin), fmt_fase(row$fase_5_fin))

    sub_tbl <- subs_all() %>% filter(capitulo_num == cap_num) %>% arrange(subcapitulo)

    subject <- glue("Libro de Estadística – {cap_txt}: {titulo}")
    body_md <- compose_body_md(cap_txt, titulo, autor, coaut, sub_tbl, fases)

    email <- blastula::compose_email(
      body = md(body_md),
      footer = md(glue("_Enviado por la coordinación · {format(Sys.time(), '%Y-%m-%d %H:%M')}._"))
    )

    # Adjuntar rúbrica si existe y se pide
    attach_path <- if (isTRUE(input$adj_rubrica)) here::here("docs","Rubrica_Capitulo.pdf") else ""
    if (isTRUE(input$adj_rubrica) && file.exists(attach_path)) {
      email <- email %>% add_attachment(file = attach_path)
    }

    # Modo prueba: guardar HTML
    if (isTRUE(input$modo_prueba)) {
      outdir <- here::here("Shiny","Correos","outbox")
      if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
      fhtml <- file.path(outdir, glue("PREVIEW_{cap_num}_{format(Sys.time(), '%Y%m%d_%H%M%S')}.html"))
      tryCatch(
        {
          email %>% blastula::render_email() %>% writeLines(con = fhtml)
          sendlog(glue("📝 Modo prueba: guardado {basename(fhtml)}\nPara: {to_email}  CC: {cc_email}"))
        },
        error = function(e){
          sendlog(glue("❌ Error al guardar HTML: {e$message}"))
        }
      )
      return(invisible())
    }

    # Envío real con credencial guardada (keyring)
    tryCatch(
      {
        blastula::smtp_send(
          email        = email,
          from         = tolower(row$correo_coordinacion %||% row$correo_principal %||% to_email),
          to           = to_email,
          subject      = subject,
          cc           = if (nzchar(cc_email)) cc_email else NULL,
          credentials  = creds_key("office365"),
          from_name    = input$from_name
        )
        sendlog(glue("✅ Enviado a {to_email}{ifelse(nzchar(cc_email), paste0(' (CC: ', cc_email, ')'), '')}"))
      },
      error = function(e){
        sendlog(glue("❌ Error al enviar: {e$message}"))
      }
    )
  })

  output$send_log <- renderText(sendlog())
}

# ---- Run ----
shinyApp(ui, server)
