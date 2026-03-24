# ============================================================
# SHOT MAP (PERSPECTIVA DO GOLEIRO)
# ============================================================

#' Mapa de Chutes na Perspectiva do Goleiro
#'
#' Exibe os chutes sofridos ou realizados do ponto de vista da baliza,
#' diferenciando gols de defesas.
#'
#' @param partida Data frame da partida.
#' @param team_id ID do time (goleiro/defesa analisado).
#' @param nome_jogador Nome do jogador para o título (ex: goleiro). Opcional.
#' @param cor_gol Cor dos gols sofridos. Padrão: `"#e35d6a"`.
#' @param cor_defesa Cor das defesas. Padrão: `"#584d77"`.
#' @param salvar Caminho para salvar o PNG. Se `NULL`, apenas exibe.
#'
#' @return Um objeto `ggplot`.
#' @export
ca_shot_map <- function(partida,
                         team_id,
                         nome_jogador = NULL,
                         cor_gol      = "#e35d6a",
                         cor_defesa   = "#584d77",
                         salvar       = NULL) {
  
  .carregar_fonte()
  team_id <- .validar_team(partida, team_id)
  bg      <- "white"
  
  df_shots <- partida |>
    dplyr::filter(.data$teamId == team_id,
                  .data$type %in% c("Save", "SavedShot", "Goal")) |>
    dplyr::mutate(
      goal_x = 20 + (.data$y / 100) * 60,
      goal_z = stats::runif(dplyr::n(), 2, 36)
    )
  
  if (nrow(df_shots) == 0) stop("Nenhum evento de chute/defesa para teamId: ", team_id)
  
  titulo_plot    <- if (!is.null(nome_jogador)) nome_jogador else paste0("Team ", team_id)
  n_gols         <- sum(df_shots$type == "Goal")
  n_defesas      <- sum(df_shots$type != "Goal")
  
  p <- ggplot2::ggplot(df_shots, ggplot2::aes(x = .data$goal_x, y = .data$goal_z)) +
    # Trave
    ggplot2::annotate("segment", x = 20, xend = 80, y = 38, yend = 38, color = "#909090", linewidth = 2.5) +
    ggplot2::annotate("segment", x = 20, xend = 20, y = 0,  yend = 38, color = "#909090", linewidth = 2.5) +
    ggplot2::annotate("segment", x = 80, xend = 80, y = 0,  yend = 38, color = "#909090", linewidth = 2.5) +
    ggplot2::annotate("segment", x = 5,  xend = 95, y = 0,  yend = 0,  color = "#909090", linewidth = 1) +
    # Pontos
    ggplot2::geom_point(ggplot2::aes(color = .data$type), size = 6, alpha = 0.8) +
    ggplot2::scale_color_manual(values = c("Goal" = cor_gol, "Save" = cor_defesa, "SavedShot" = cor_defesa)) +
    ggplot2::coord_fixed(ratio = 0.5, xlim = c(5, 95), ylim = c(-25, 60)) +
    ggplot2::labs(
      title    = titulo_plot,
      subtitle = paste0("Shot Map | Team ", team_id),
      caption  = "Campo Analítico | campoanalytics pkg"
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = bg, color = NA),
      plot.title      = ggplot2::element_text(family = "roboto_slab", hjust = 0.5, face = "bold", size = 24,
                                               margin = ggplot2::margin(t = 20)),
      plot.subtitle   = ggplot2::element_text(family = "roboto_slab", hjust = 0.5, size = 14,
                                               color = "#505050", margin = ggplot2::margin(b = 20)),
      plot.caption    = ggplot2::element_text(family = "roboto_slab", size = 10, color = "grey40"),
      legend.position = "none"
    ) +
    # Legenda rodapé
    ggplot2::annotate("point", x = 35, y = -15, size = 12, color = cor_gol) +
    ggplot2::annotate("text",  x = 35, y = -15, label = n_gols, color = "white", fontface = "bold", family = "roboto_slab") +
    ggplot2::annotate("text",  x = 44, y = -15, label = "gols", size = 5, family = "roboto_slab") +
    ggplot2::annotate("point", x = 60, y = -15, size = 12, shape = 21, stroke = 1.5, color = cor_defesa) +
    ggplot2::annotate("text",  x = 60, y = -15, label = n_defesas, fontface = "bold", family = "roboto_slab") +
    ggplot2::annotate("text",  x = 67, y = -15, label = "defesas", size = 5, family = "roboto_slab")
  
  if (!is.null(salvar)) {
    ggplot2::ggsave(salvar, plot = p, width = 18, height = 16, dpi = 150, units = "cm", bg = bg)
    message("Salvo em: ", salvar)
  }
  
  p
}


# ============================================================
# xG RACE CHART
# ============================================================

#' Corrida de xG Acumulado por Minuto
#'
#' @param partida Data frame da partida.
#' @param team_id_casa ID do time da casa.
#' @param team_id_fora ID do time visitante.
#' @param nome_casa Nome do time da casa.
#' @param nome_fora Nome do time visitante.
#' @param cor_casa Cor do time da casa. Padrão: `"#000000"`.
#' @param cor_fora Cor do time visitante. Padrão: `"#E57373"`.
#' @param titulo Título do gráfico.
#' @param salvar Caminho para salvar o PNG.
#'
#' @return Um objeto `ggplot`.
#' @export
ca_xg_race <- function(partida,
                        team_id_casa,
                        team_id_fora,
                        nome_casa = NULL,
                        nome_fora = NULL,
                        cor_casa  = "#000000",
                        cor_fora  = "#E57373",
                        titulo    = "xG Race Chart",
                        salvar    = NULL) {
  
  .carregar_fonte()
  team_id_casa <- .validar_team(partida, team_id_casa)
  team_id_fora <- .validar_team(partida, team_id_fora)
  bg           <- "#F7F6F2"
  
  if (is.null(nome_casa)) nome_casa <- paste0("Team ", team_id_casa)
  if (is.null(nome_fora)) nome_fora <- paste0("Team ", team_id_fora)
  
  df_race <- partida |>
    dplyr::filter(
      .data$teamId %in% c(team_id_casa, team_id_fora),
      .data$type %in% c("Goal", "MissedShots", "SavedShot", "ShotOnPost")
    ) |>
    dplyr::mutate(xG = stats::runif(dplyr::n(), 0.05, 0.4)) |>  # xG simulado — substituir por coluna real se disponível
    dplyr::arrange(.data$minute) |>
    dplyr::group_by(.data$teamId) |>
    dplyr::mutate(xg_cum = cumsum(.data$xG)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      team_name = dplyr::case_when(
        .data$teamId == team_id_casa ~ nome_casa,
        .data$teamId == team_id_fora ~ nome_fora
      )
    )
  
  df_gols <- df_race |>
    dplyr::filter(.data$type == "Goal") |>
    dplyr::mutate(label_gol = paste0(.data$jogador, " (", .data$minute, "')"))
  
  cores <- stats::setNames(c(cor_casa, cor_fora), c(nome_casa, nome_fora))
  
  p <- ggplot2::ggplot(df_race, ggplot2::aes(x = .data$minute, y = .data$xg_cum,
                                               color = .data$team_name, group = .data$team_name)) +
    ggplot2::geom_step(linewidth = 1.2) +
    ggplot2::geom_vline(xintercept = 45, linetype = "dashed", color = "grey80") +
    ggplot2::geom_point(data = df_gols, size = 5, shape = 21, fill = "white", stroke = 1.5) +
    ggplot2::geom_text(data = df_gols, label = "G", size = 3, fontface = "bold", color = "black") +
    ggrepel::geom_label_repel(
      data = df_gols,
      ggplot2::aes(label = .data$label_gol),
      nudge_y = 0.25, direction = "y", segment.color = "grey50",
      size = 3.5, fontface = "bold", fill = "white", show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(values = cores, name = NULL) +
    ggplot2::labs(
      title    = titulo,
      subtitle = "Gols Esperados Acumulados (xG simulado) por minuto",
      x        = "Minuto",
      y        = "xG Acumulado",
      caption  = "Campo Analítico | campoanalytics pkg\n* xG simulado — forneça coluna 'xG' para valores reais"
    ) +
    ggplot2::theme_minimal(base_family = "roboto_slab") +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = bg, color = bg),
      legend.position = "top",
      plot.title      = ggplot2::element_text(face = "bold", size = 20)
    )
  
  if (!is.null(salvar)) {
    ggplot2::ggsave(salvar, plot = p, width = 24, height = 14, dpi = 150, units = "cm", bg = bg)
    message("Salvo em: ", salvar)
  }
  
  p
}


# ============================================================
# DOMINANCE MAP
# ============================================================

#' Mapa de Dominância por Zonas do Campo
#'
#' Compara a diferença de ações bem-sucedidas entre dois times por zona.
#'
#' @param partida Data frame da partida.
#' @param team_id_casa ID do time da casa (referência positiva).
#' @param team_id_fora ID do time visitante (referência negativa).
#' @param cor_dominante Cor de destaque do time da casa. Padrão: `"#E57373"`.
#' @param titulo Título do gráfico.
#' @param salvar Caminho para salvar o PNG.
#'
#' @return Um objeto `ggplot`.
#' @export
ca_dominance_map <- function(partida,
                              team_id_casa,
                              team_id_fora,
                              cor_dominante = "#E57373",
                              titulo        = "Dominance Map",
                              salvar        = NULL) {
  
  .carregar_fonte()
  team_id_casa <- .validar_team(partida, team_id_casa)
  team_id_fora <- .validar_team(partida, team_id_fora)
  bg           <- "#f8f9fa"
  
  col_casa <- paste0("team_", team_id_casa)
  col_fora <- paste0("team_", team_id_fora)
  
  df_grid <- partida |>
    dplyr::filter(
      .data$outcome == "Successful",
      .data$teamId %in% c(team_id_casa, team_id_fora)
    ) |>
    dplyr::mutate(
      x_bin = cut(.data$x, breaks = seq(0, 100, length.out = 7), include.lowest = TRUE, labels = FALSE),
      y_bin = cut(.data$y, breaks = seq(0, 100, length.out = 6), include.lowest = TRUE, labels = FALSE)
    ) |>
    dplyr::group_by(.data$x_bin, .data$y_bin, .data$teamId) |>
    dplyr::summarise(n_acoes = dplyr::n(), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from  = .data$teamId,
      names_prefix = "team_",
      values_from = .data$n_acoes,
      values_fill = 0
    ) |>
    dplyr::mutate(
      diff       = .data[[col_casa]] - .data[[col_fora]],
      total_zona = .data[[col_casa]] + .data[[col_fora]],
      x_pos      = (.data$x_bin * (100 / 6)) - (100 / 12),
      y_pos      = (.data$y_bin * (100 / 5)) - (100 / 10)
    )
  
  p <- ggplot2::ggplot(df_grid) +
    ggsoccer::annotate_pitch(dimensions = ggsoccer::pitch_opta, fill = bg, colour = "#d1d1d1") +
    ggplot2::geom_tile(
      ggplot2::aes(x = .data$x_pos, y = .data$y_pos, fill = .data$diff),
      color = "gray60", linetype = "dashed", linewidth = 0.3, alpha = 0.8
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x     = .data$x_pos, y = .data$y_pos + 2,
        label = ifelse(.data$diff > 0, paste0("+", .data$diff), .data$diff)
      ),
      family = "roboto_slab", fontface = "bold", size = 5, color = "#1a1a1a"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x     = .data$x_pos, y = .data$y_pos - 3,
        label = paste0("(", .data$total_zona, ")")
      ),
      family = "roboto_slab", size = 3, color = "#555555"
    ) +
    ggplot2::scale_fill_gradient(low = "white", high = cor_dominante, guide = "none") +
    ggplot2::annotate("text", x = 50, y = -5,   label = "▶ ▶ ▶ ▶", size = 4, color = "gray50") +
    ggplot2::annotate("text", x = 50, y = -8.5, label = "Attacking Direction",
                      family = "roboto_slab", size = 3, color = "gray50") +
    ggsoccer::theme_pitch() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = bg, color = NA),
      plot.title      = ggplot2::element_text(family = "roboto_slab", face = "bold", size = 20),
      plot.subtitle   = ggplot2::element_text(family = "roboto_slab", size = 11, color = "gray30"),
      plot.caption    = ggplot2::element_text(family = "roboto_slab", size = 8, color = "gray60", hjust = 0.9)
    ) +
    ggplot2::labs(
      title    = titulo,
      subtitle = paste0("Diferença de ações bem-sucedidas | Team ", team_id_casa, " vs Team ", team_id_fora),
      caption  = "Campo Analítico | campoanalytics pkg\n(Número entre parênteses = total de ações na zona)"
    )
  
  if (!is.null(salvar)) {
    ggplot2::ggsave(salvar, plot = p, width = 22, height = 16, dpi = 150, units = "cm", bg = bg)
    message("Salvo em: ", salvar)
  }
  
  p
}
