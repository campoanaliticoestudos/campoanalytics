# ============================================================
# PASSING NETWORK
# ============================================================

#' Rede de Passes entre Jogadores (Passing Network)
#'
#' Exibe o campo com posicionamento médio dos jogadores e setas
#' representando as conexões de passe mais frequentes.
#'
#' @param partida Data frame da partida.
#' @param team_id ID do time a ser analisado.
#' @param cor_principal Cor de destaque do time em hex. Padrão: `"#E57373"`.
#' @param nome_time Nome do time para exibição. Se `NULL`, usa o teamId.
#' @param n_titulares Número de jogadores a incluir. Padrão: `11`.
#' @param n_conexoes Número máximo de conexões exibidas. Padrão: `20`.
#' @param salvar Caminho para salvar o PNG. Se `NULL`, apenas exibe.
#'
#' @return Um objeto `ggplot`.
#' @export
#'
#' @examples
#' \dontrun{
#' partida <- ca_carregar_partida("partida.csv")
#' ca_passing_network(partida, team_id = "63", cor_principal = "#E57373",
#'                    nome_time = "Atlético de Madrid")
#' }
ca_passing_network <- function(partida,
                                team_id,
                                cor_principal = "#E57373",
                                nome_time     = NULL,
                                n_titulares   = 11,
                                n_conexoes    = 20,
                                salvar        = NULL) {
  
  .carregar_fonte()
  team_id   <- .validar_team(partida, team_id)
  bg        <- "#F7F6F2"
  if (is.null(nome_time)) nome_time <- paste0("Team ", team_id)
  
  df_time <- partida |> dplyr::filter(.data$teamId == team_id)
  nomes_titulares <- .titulares(df_time, n_titulares)
  
  # --- Estatísticas de passes ---
  stats_passes <- df_time |>
    dplyr::filter(.data$jogador %in% nomes_titulares, .data$type == "Pass") |>
    dplyr::group_by(.data$jogador) |>
    dplyr::summarise(total_passes = dplyr::n(), .groups = "drop")
  
  # --- Posições médias ---
  posicoes <- df_time |>
    dplyr::filter(.data$jogador %in% nomes_titulares, .data$type == "Pass", .data$outcome == "Successful") |>
    dplyr::group_by(.data$jogador) |>
    dplyr::summarise(x = mean(.data$x, na.rm = TRUE), y = mean(.data$y, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(.data$x) |>
    dplyr::mutate(idx = dplyr::row_number())
  
  # --- Atribuição tática simplificada ---
  n_disp <- nrow(posicoes)
  posicoes <- posicoes |>
    dplyr::mutate(pos = dplyr::case_when(
      .data$idx == 1                        ~ "GK",
      .data$idx %in% 2:5  & n_disp >= 5    ~ c("LB","LCB","RCB","RB")[.data$idx - 1],
      .data$idx %in% 6:8  & n_disp >= 8    ~ c("LCM","DM","RCM")[.data$idx - 5],
      .data$idx %in% 9:11 & n_disp >= 11   ~ c("LW","FW","RW")[.data$idx - 8],
      TRUE ~ paste0("P", .data$idx)
    ))
  
  info_tatica <- posicoes |>
    dplyr::left_join(stats_passes, by = "jogador")
  
  # --- Conexões ---
  df_passes_seq <- df_time |>
    dplyr::filter(.data$type == "Pass", .data$outcome == "Successful",
                  .data$jogador %in% nomes_titulares) |>
    dplyr::mutate(jogador_d = dplyr::lead(.data$jogador)) |>
    dplyr::filter(!is.na(.data$jogador_d), .data$jogador != .data$jogador_d,
                  .data$jogador_d %in% nomes_titulares) |>
    dplyr::count(.data$jogador, .data$jogador_d, name = "freq") |>
    dplyr::left_join(info_tatica |> dplyr::select(.data$jogador, x_s = .data$x, y_s = .data$y), by = "jogador") |>
    dplyr::left_join(info_tatica |> dplyr::select(.data$jogador, x_e = .data$x, y_e = .data$y),
                     by = c("jogador_d" = "jogador")) |>
    dplyr::filter(!is.na(.data$x_s), !is.na(.data$x_e)) |>
    dplyr::arrange(dplyr::desc(.data$freq)) |>
    dplyr::slice(1:n_conexoes) |>
    dplyr::mutate(
      dx    = .data$x_e - .data$x_s,
      dy    = .data$y_e - .data$y_s,
      dist  = sqrt(.data$dx^2 + .data$dy^2),
      x_s2  = .data$x_s + (6 / .data$dist) * .data$dx,
      y_s2  = .data$y_s + (6 / .data$dist) * .data$dy,
      x_e2  = .data$x_e - (11 / .data$dist) * .data$dx,
      y_e2  = .data$y_e - (11 / .data$dist) * .data$dy
    )
  
  total_passes_str <- sum(stats_passes$total_passes, na.rm = TRUE)
  
  p <- ggplot2::ggplot() +
    ggsoccer::annotate_pitch(colour = "#D0D0D0", fill = bg, limits = FALSE) +
    ggplot2::geom_curve(
      data = df_passes_seq,
      ggplot2::aes(x = .data$x_s2, y = .data$y_s2,
                   xend = .data$x_e2, yend = .data$y_e2,
                   linewidth = .data$freq),
      curvature = 0.1, color = "black", alpha = 0.4,
      arrow = ggplot2::arrow(length = ggplot2::unit(0.12, "inches"), type = "closed"),
      show.legend = FALSE
    ) +
    ggplot2::geom_point(
      data = info_tatica,
      ggplot2::aes(x = .data$x, y = .data$y, size = .data$total_passes),
      fill = cor_principal, color = "black", shape = 21, stroke = 0.7
    ) +
    ggplot2::geom_text(
      data = info_tatica,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$pos),
      color = "white", fontface = "bold", size = 3, family = "roboto_slab"
    ) +
    ggplot2::scale_linewidth(range = c(0.5, 3.0)) +
    ggplot2::scale_size_continuous(limits = c(0, 120), range = c(6, 12)) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_reverse() +
    ggplot2::labs(
      title    = "PASSING NETWORK",
      subtitle = paste0(nome_time, "\nPasses totais: ", total_passes_str),
      caption  = "Campo Analítico | campoanalytics pkg"
    ) +
    ggsoccer::theme_pitch() +
    ggplot2::theme(
      aspect.ratio  = 1.6,
      plot.background = ggplot2::element_rect(fill = bg, colour = NA),
      plot.title    = ggplot2::element_text(hjust = 0.5, face = "bold", size = 18, family = "roboto_slab"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, face = "bold", size = 12, family = "roboto_slab"),
      plot.caption  = ggplot2::element_text(hjust = 0.5, size = 8, family = "roboto_slab"),
      legend.position = "none"
    )
  
  if (!is.null(salvar)) {
    ggplot2::ggsave(salvar, plot = p, width = 22, height = 20, dpi = 150, units = "cm", bg = bg)
    message("Salvo em: ", salvar)
  }
  
  p
}
