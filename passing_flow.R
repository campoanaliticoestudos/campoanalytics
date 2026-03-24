# ============================================================
# PASSING FLOW + VOLUME
# ============================================================

#' Mapa de Direção e Volume de Passes (Passing Flow)
#'
#' Gera dois mapas lado a lado: setas mostrando a direção média dos passes
#' e zonas coloridas com percentual de volume por área do campo.
#'
#' @param partida Data frame da partida (saída de \code{\link{ca_carregar_partida}}).
#' @param team_id ID do time a ser analisado (string ou número).
#' @param cor_principal Cor de destaque em hex. Padrão: `"#E57373"`.
#' @param titulo Título do gráfico. Se `NULL`, gerado automaticamente.
#' @param subtitulo Subtítulo do gráfico. Se `NULL`, usa nome do time.
#' @param res_x Resolução horizontal da grade de flow. Padrão: `20`.
#' @param res_y Resolução vertical da grade de flow. Padrão: `15`.
#' @param salvar Caminho para salvar o PNG. Se `NULL`, apenas exibe.
#'
#' @return Um objeto `patchwork` com os dois gráficos combinados.
#' @export
#'
#' @examples
#' \dontrun{
#' partida <- ca_carregar_partida("partida.csv")
#' ca_passing_flow(partida, team_id = "63", cor_principal = "#E57373")
#' }
ca_passing_flow <- function(partida,
                             team_id,
                             cor_principal = "#E57373",
                             titulo        = NULL,
                             subtitulo     = NULL,
                             res_x         = 20,
                             res_y         = 15,
                             salvar        = NULL) {
  
  .carregar_fonte()
  team_id <- .validar_team(partida, team_id)
  bg      <- "#F7F6F2"
  paleta  <- .paleta(cor_principal, bg)
  
  if (is.null(titulo))    titulo    <- paste0("Passing Analysis - Team ", team_id)
  if (is.null(subtitulo)) subtitulo <- paste0("Team ID: ", team_id)
  
  df_passes <- partida |>
    dplyr::filter(.data$teamId == team_id, .data$type == "Pass", .data$outcome == "Successful")
  
  if (nrow(df_passes) == 0) stop("Nenhum passe bem-sucedido encontrado para teamId: ", team_id)
  
  # --- FLOW (Setas) ---
  df_flow <- df_passes |>
    dplyr::mutate(
      gx = cut(.data$x, breaks = seq(0, 100, length.out = res_x + 1), labels = FALSE, include.lowest = TRUE),
      gy = cut(.data$y, breaks = seq(0, 100, length.out = res_y + 1), labels = FALSE, include.lowest = TRUE)
    ) |>
    dplyr::group_by(.data$gx, .data$gy) |>
    dplyr::summarise(
      x = mean(.data$x), y = mean(.data$y),
      dx = mean(.data$endX - .data$x), dy = mean(.data$endY - .data$y),
      count = dplyr::n(), .groups = "drop"
    )
  
  # --- VOLUME (Zonas) ---
  total_passes <- nrow(df_passes)
  
  zones_df <- expand.grid(zx = 1:6, zy = 1:3) |>
    dplyr::mutate(
      id   = (.data$zx - 1) * 3 + .data$zy,
      xmin = (.data$zx - 1) * (100 / 6), xmax = .data$zx * (100 / 6),
      ymin = (.data$zy - 1) * (100 / 3), ymax = .data$zy * (100 / 3)
    )
  
  df_volume <- df_passes |>
    dplyr::mutate(
      zx      = pmin(pmax(floor(.data$x / (100 / 6)) + 1, 1), 6),
      zy      = pmin(pmax(floor(.data$y / (100 / 3)) + 1, 1), 3),
      zone_id = (.data$zx - 1) * 3 + .data$zy
    ) |>
    dplyr::group_by(.data$zone_id) |>
    dplyr::summarise(pct = dplyr::n() / total_passes, .groups = "drop") |>
    dplyr::right_join(data.frame(zone_id = 1:18), by = "zone_id") |>
    dplyr::mutate(pct = tidyr::replace_na(.data$pct, 0)) |>
    dplyr::left_join(zones_df |> dplyr::select(.data$id, .data$xmin, .data$xmax, .data$ymin, .data$ymax),
                     by = c("zone_id" = "id"))
  
  tema <- .tema_campo(bg)
  
  # --- GRÁFICO 1: FLOW ---
  p1 <- ggplot2::ggplot() +
    ggplot2::geom_bin2d(
      data = dplyr::filter(partida, .data$teamId == team_id, .data$type == "Pass"),
      ggplot2::aes(x = .data$x, y = .data$y), binwidth = c(5, 6.6), alpha = 0.4
    ) +
    ggplot2::scale_fill_gradientn(colors = paleta) +
    ggsoccer::annotate_pitch(colour = "#444444", fill = NA, linewidth = 0.3) +
    ggplot2::geom_segment(
      data = df_flow,
      ggplot2::aes(
        x    = .data$x, y = .data$y,
        xend = .data$x + (.data$dx / sqrt(.data$dx^2 + .data$dy^2)) * 3,
        yend = .data$y + (.data$dy / sqrt(.data$dx^2 + .data$dy^2)) * 3
      ),
      arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "cm"), type = "open"),
      color = "black", linewidth = 0.7
    ) +
    ggplot2::coord_flip(xlim = c(0, 100), ylim = c(0, 100), expand = FALSE) +
    ggplot2::labs(subtitle = "DIREÇÃO E SENTIDO (PASSING FLOW)") +
    tema
  
  # --- GRÁFICO 2: VOLUME ---
  p2 <- ggplot2::ggplot(df_volume) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                   ymin = .data$ymin, ymax = .data$ymax, fill = .data$pct),
      colour = "#444444", linewidth = 0.2
    ) +
    ggsoccer::annotate_pitch(colour = "#444444", fill = NA, linewidth = 0.3) +
    ggplot2::geom_text(
      ggplot2::aes(
        x     = (.data$xmin + .data$xmax) / 2,
        y     = (.data$ymin + .data$ymax) / 2,
        label = ifelse(.data$pct > 0.01, scales::percent(.data$pct, accuracy = 1), "")
      ),
      family = "roboto_slab", fontface = "bold", size = 8.5, color = "#111111"
    ) +
    ggplot2::scale_fill_gradientn(colors = paleta) +
    ggplot2::coord_flip(xlim = c(0, 100), ylim = c(0, 100), expand = FALSE) +
    ggplot2::labs(subtitle = "DISTRIBUIÇÃO PERCENTUAL DE AÇÕES") +
    tema
  
  # --- UNIÃO ---
  final_plot <- (p1 | p2) +
    patchwork::plot_annotation(
      title    = titulo,
      subtitle = subtitulo,
      caption  = "Campo Analítico | campoanalytics pkg",
      theme    = ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = bg, color = bg),
        text            = ggplot2::element_text(family = "roboto_slab", face = "bold"),
        plot.title      = ggplot2::element_text(size = 22, hjust = 0, margin = ggplot2::margin(l = 20, t = 10)),
        plot.subtitle   = ggplot2::element_text(size = 14, hjust = 0, margin = ggplot2::margin(l = 20, b = 10))
      )
    )
  
  if (!is.null(salvar)) {
    ggplot2::ggsave(salvar, plot = final_plot, width = 28, height = 16, dpi = 150, units = "cm", bg = bg)
    message("Salvo em: ", salvar)
  }
  
  final_plot
}
