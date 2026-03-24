# ============================================================
# AÇÕES POR 15 MINUTOS
# ============================================================

#' Distribuição de Ações por Intervalos de 15 Minutos
#'
#' Gera um grid de 6 mini-mapas mostrando a distribuição de ações
#' por terço do campo em cada período de 15 minutos.
#'
#' @param partida Data frame da partida.
#' @param team_id ID do time a ser analisado.
#' @param cor_destaque Cor de destaque (terço do meio). Padrão: `"#E57373"`.
#' @param titulo Título do grid final.
#' @param salvar Caminho para salvar o PNG.
#'
#' @return Um objeto `patchwork`.
#' @export
ca_acoes_15min <- function(partida,
                            team_id,
                            cor_destaque = "#E57373",
                            titulo       = "Distribuição de Ações por Intervalo de 15 Minutos",
                            salvar       = NULL) {
  
  .carregar_fonte()
  team_id <- .validar_team(partida, team_id)
  bg      <- "#F7F6F2"
  
  if (!requireNamespace("showtext", quietly = TRUE)) stop("Pacote 'showtext' necessário.")
  
  .plot_periodo <- function(start_min, end_min) {
    
    df_periodo <- partida |>
      dplyr::filter(.data$teamId == team_id,
                    .data$minute >= start_min,
                    .data$minute < end_min)
    
    if (nrow(df_periodo) < 1) return(NULL)
    
    df_periodo <- df_periodo |>
      dplyr::mutate(Third = dplyr::case_when(
        .data$x <= 33.3              ~ "Defensive",
        .data$x > 33.3 & .data$x <= 66.6 ~ "Middle",
        .data$x > 66.6              ~ "Attacking"
      ))
    
    actions_pct <- df_periodo |>
      dplyr::group_by(.data$Third) |>
      dplyr::summarise(Actions = dplyr::n(), .groups = "drop") |>
      dplyr::mutate(Percent = round(.data$Actions / sum(.data$Actions) * 100, 0))
    
    pct_def <- actions_pct$Percent[actions_pct$Third == "Defensive"]
    pct_mid <- actions_pct$Percent[actions_pct$Third == "Middle"]
    pct_att <- actions_pct$Percent[actions_pct$Third == "Attacking"]
    
    if (length(pct_def) == 0) pct_def <- 0
    if (length(pct_mid) == 0) pct_mid <- 0
    if (length(pct_att) == 0) pct_att <- 0
    
    ggplot2::ggplot() +
      ggplot2::annotate("rect", xmin = 0,    xmax = 33.3, ymin = 0, ymax = 100, fill = "#F9F6EF") +
      ggplot2::annotate("rect", xmin = 33.3, xmax = 66.6, ymin = 0, ymax = 100, fill = cor_destaque) +
      ggplot2::annotate("rect", xmin = 66.6, xmax = 100,  ymin = 0, ymax = 100, fill = "#F9F6EF") +
      ggsoccer::annotate_pitch(colour = "#7f7f7f", fill = NA) +
      ggsoccer::theme_pitch() +
      ggplot2::annotate("text", x = 16.6, y = 50, label = paste0(pct_def, "%"),
                        size = 6.5, fontface = "bold", color = "#3d3d3d", family = "roboto_slab") +
      ggplot2::annotate("text", x = 50,   y = 50, label = paste0(pct_mid, "%"),
                        size = 6.5, fontface = "bold", color = "white", family = "roboto_slab") +
      ggplot2::annotate("text", x = 83.3, y = 50, label = paste0(pct_att, "%"),
                        size = 6.5, fontface = "bold", color = "#3d3d3d", family = "roboto_slab") +
      ggplot2::labs(title = paste0(start_min, "–", end_min, " min")) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_reverse() +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.text  = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(family = "roboto_slab", face = "bold", size = 14, hjust = 0.5),
        plot.background  = ggplot2::element_rect(fill = bg, colour = NA),
        panel.background = ggplot2::element_rect(fill = bg, colour = NA)
      )
  }
  
  intervalos <- seq(0, 75, by = 15)
  plots      <- Filter(Negate(is.null), lapply(intervalos, function(s) .plot_periodo(s, s + 15)))
  
  if (length(plots) == 0) stop("Nenhum evento encontrado para teamId: ", team_id)
  
  final_grid <- patchwork::wrap_plots(plots, ncol = 3, nrow = 2) +
    patchwork::plot_annotation(
      title    = titulo,
      subtitle = paste0("Team ID: ", team_id, " | % por terço do campo"),
      caption  = "Campo Analítico | campoanalytics pkg",
      theme    = ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = bg, colour = NA),
        plot.title      = ggplot2::element_text(family = "roboto_slab", face = "bold", size = 16, hjust = 0.5),
        plot.subtitle   = ggplot2::element_text(family = "roboto_slab", size = 12, hjust = 0.5),
        plot.caption    = ggplot2::element_text(family = "roboto_slab", size = 10, hjust = 0.5)
      )
    )
  
  if (!is.null(salvar)) {
    ggplot2::ggsave(salvar, plot = final_grid, width = 30, height = 20, dpi = 150, units = "cm", bg = bg)
    message("Salvo em: ", salvar)
  }
  
  final_grid
}


# ============================================================
# TAXA DE SUCESSO POR EVENTO
# ============================================================

#' Taxa de Sucesso por Tipo de Evento (com IC 95%)
#'
#' @param partida Data frame da partida.
#' @param nome_jogador Nome exato do jogador conforme a coluna `jogador`.
#' @param cor_ponto Cor dos pontos e barras de erro. Padrão: `"#f0627d"`.
#' @param salvar Caminho para salvar o PNG.
#'
#' @return Um objeto `ggplot`.
#' @export
ca_taxa_sucesso <- function(partida,
                             nome_jogador,
                             cor_ponto = "#f0627d",
                             salvar    = NULL) {
  
  .carregar_fonte()
  
  jogadores_disponiveis <- unique(partida$jogador)
  if (!nome_jogador %in% jogadores_disponiveis) {
    stop("Jogador '", nome_jogador, "' nao encontrado.\nJogadores disponíveis (primeiros 10): ",
         paste(head(jogadores_disponiveis, 10), collapse = ", "), call. = FALSE)
  }
  
  df_plot <- partida |>
    dplyr::filter(.data$jogador == nome_jogador) |>
    dplyr::group_by(.data$type) |>
    dplyr::summarise(
      total    = dplyr::n(),
      sucessos = sum(.data$outcome == "Successful"),
      .groups  = "drop"
    ) |>
    dplyr::mutate(
      estimate = .data$sucessos / .data$total,
      se       = sqrt((.data$estimate * (1 - .data$estimate)) / .data$total),
      low      = pmax(0, .data$estimate - 1.96 * .data$se),
      high     = pmin(1, .data$estimate + 1.96 * .data$se),
      type     = stats::reorder(.data$type, .data$estimate)
    )
  
  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = .data$estimate, y = .data$type)) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$low, xmax = .data$high),
                            height = 0.2, color = cor_ponto, linewidth = 0.8) +
    ggplot2::geom_point(color = cor_ponto, size = 4) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, 1.05),
      breaks = seq(0, 1, 0.25)
    ) +
    ggplot2::labs(
      title    = paste0("Taxa de Sucesso por Evento: ", nome_jogador),
      subtitle = "Intervalo de confiança de 95% para cada tipo de ação",
      x        = "% de Sucesso",
      y        = NULL,
      caption  = paste0("Campo Analítico | campoanalytics pkg | Jogador: ", nome_jogador)
    ) +
    ggplot2::theme_minimal(base_family = "roboto_slab") +
    ggplot2::theme(
      text              = ggplot2::element_text(color = "black"),
      plot.background   = ggplot2::element_rect(fill = "white", color = NA),
      panel.background  = ggplot2::element_rect(fill = "white", color = NA),
      panel.grid.major.x = ggplot2::element_line(color = "grey90"),
      panel.grid.major.y = ggplot2::element_line(color = "grey95"),
      panel.grid.minor  = ggplot2::element_blank(),
      axis.text         = ggplot2::element_text(color = "black", size = 11),
      plot.title        = ggplot2::element_text(size = 18),
      plot.subtitle     = ggplot2::element_text(size = 12, color = "grey30"),
      plot.caption      = ggplot2::element_text(hjust = 1, color = "grey40")
    )
  
  if (!is.null(salvar)) {
    ggplot2::ggsave(salvar, plot = p, width = 22, height = 16, dpi = 150, units = "cm", bg = "white")
    message("Salvo em: ", salvar)
  }
  
  p
}


# ============================================================
# TABELA DE PASSES POR BLOCO
# ============================================================

#' Tabela de Passes por Bloco do Campo
#'
#' Gera uma tabela estilizada com os melhores passadores em cada
#' terço do campo (Bloco Baixo, Médio e Alto).
#'
#' @param partida Data frame da partida.
#' @param team_id ID do time a ser analisado. Se `NULL`, usa todos.
#' @param top_n Número de jogadores por bloco. Padrão: `5`.
#' @param min_passes Volume mínimo de passes para inclusão. Padrão: `3`.
#' @param titulo Título da tabela.
#' @param salvar Caminho para salvar a imagem PNG. Se `NULL`, retorna objeto `gt`.
#'
#' @return Um objeto `gt`.
#' @export
ca_tabela_passes <- function(partida,
                              team_id    = NULL,
                              top_n      = 5,
                              min_passes = 3,
                              titulo     = "Distribuição de Passes por Bloco",
                              salvar     = NULL) {
  
  if (!requireNamespace("gt", quietly = TRUE)) stop("Pacote 'gt' necessário. Instale com: install.packages('gt')")
  
  df <- partida
  if (!is.null(team_id)) {
    team_id <- .validar_team(partida, team_id)
    df <- dplyr::filter(df, .data$teamId == team_id)
  }
  
  passes_blocos <- df |>
    dplyr::filter(.data$type == "Pass") |>
    dplyr::mutate(
      bloco = dplyr::case_when(
        .data$x <= 33.3              ~ "Bloco Baixo",
        .data$x > 33.3 & .data$x <= 66.6 ~ "Bloco Médio",
        .data$x > 66.6              ~ "Bloco Alto"
      ),
      bloco = factor(.data$bloco, levels = c("Bloco Baixo", "Bloco Médio", "Bloco Alto"))
    ) |>
    dplyr::group_by(.data$jogador, .data$bloco) |>
    dplyr::summarise(
      Total        = dplyr::n(),
      Certos       = sum(.data$outcome == "Successful", na.rm = TRUE),
      Aproveitamento = .data$Certos / .data$Total,
      .groups      = "drop"
    ) |>
    dplyr::filter(.data$Total > min_passes)
  
  if (nrow(passes_blocos) == 0) stop("Dados insuficientes. Reduza 'min_passes'.")
  
  tabela <- passes_blocos |>
    dplyr::arrange(.data$bloco, dplyr::desc(.data$Aproveitamento)) |>
    dplyr::group_by(.data$bloco) |>
    dplyr::slice_max(order_by = .data$Aproveitamento, n = top_n) |>
    gt::gt() |>
    gt::tab_header(
      title    = gt::md(paste0("**", titulo, "**")),
      subtitle = gt::md(paste0("Top ", top_n, " jogadores por bloco | Mínimo ", min_passes, " passes"))
    ) |>
    gt::fmt_percent(columns = "Aproveitamento", decimals = 1) |>
    gt::cols_label(
      jogador        = "Jogador",
      Total          = "Tentados",
      Certos         = "Acertos",
      Aproveitamento = "% Acerto"
    ) |>
    gt::tab_options(
      table.width                   = gt::px(550),
      row_group.background.color    = "#f9f9f9",
      column_labels.font.weight     = "bold",
      table.font.size               = gt::px(14)
    ) |>
    gt::opt_table_font(font = list(gt::google_font("Inter"), "sans-serif")) |>
    gt::data_color(
      columns = "Aproveitamento",
      fn = scales::col_numeric(
        palette = c("#ffcccc", "#ffffff", "#ccffcc"),
        domain  = c(0.5, 1)
      )
    )
  
  if (!is.null(salvar)) {
    gt::gtsave(tabela, salvar)
    message("Tabela salva em: ", salvar)
  }
  
  tabela
}
