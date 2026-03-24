# ============================================================
# FUNÇÕES UTILITÁRIAS INTERNAS DO CAMPOANALYTICS
# ============================================================

# --- COLUNAS OBRIGATÓRIAS ---
.colunas_obrigatorias <- c("teamId", "type", "outcome", "x", "y", "endX", "endY", "minute", "jogador")

# ============================================================
#' Carrega e valida o CSV de uma partida
#'
#' @param caminho Caminho para o arquivo CSV da partida.
#' @param separador Separador do CSV. Padrão: `","`.
#'
#' @return Um `data.frame` validado com os dados da partida.
#' @export
#'
#' @examples
#' \dontrun{
#' partida <- ca_carregar_partida("Real Madrid 3-2 Atletico Madrid.csv")
#' }
ca_carregar_partida <- function(caminho, separador = ",") {
  
  if (!file.exists(caminho)) {
    stop("Arquivo nao encontrado: ", caminho, call. = FALSE)
  }
  
  df <- utils::read.csv(caminho, sep = separador, stringsAsFactors = FALSE)
  
  faltando <- setdiff(.colunas_obrigatorias, names(df))
  if (length(faltando) > 0) {
    stop(
      "Colunas obrigatorias ausentes no CSV: ", paste(faltando, collapse = ", "),
      "\nColunas encontradas: ", paste(names(df), collapse = ", "),
      call. = FALSE
    )
  }
  
  # Conversões de tipo seguras
  df$x      <- suppressWarnings(as.numeric(df$x))
  df$y      <- suppressWarnings(as.numeric(df$y))
  df$endX   <- suppressWarnings(as.numeric(df$endX))
  df$endY   <- suppressWarnings(as.numeric(df$endY))
  df$minute <- suppressWarnings(as.numeric(df$minute))
  df$teamId <- as.character(df$teamId)
  
  message("Partida carregada: ", nrow(df), " eventos | ",
          length(unique(df$teamId)), " times | ",
          length(unique(df$jogador)), " jogadores")
  
  df
}


# ============================================================
#' Lista os times disponíveis em um dataset de partida
#'
#' @param partida Data frame da partida carregado por \code{\link{ca_carregar_partida}}.
#' @return Um data.frame com `teamId` e contagem de eventos.
#' @export
ca_times <- function(partida) {
  partida |>
    dplyr::group_by(.data$teamId) |>
    dplyr::summarise(eventos = dplyr::n(), jogadores = dplyr::n_distinct(.data$jogador), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$eventos))
}


# ============================================================
# TEMA PADRÃO INTERNO
# ============================================================

#' Cria o tema visual padrão do campoanalytics
#'
#' @param cor_fundo Cor de fundo. Padrão: `"#F7F6F2"`.
#' @param familia_fonte Família de fonte. Padrão: `"roboto_slab"`.
#' @keywords internal
.tema_campo <- function(cor_fundo = "#F7F6F2", familia_fonte = "roboto_slab") {
  ggplot2::theme_void() +
    ggplot2::theme(
      panel.background  = ggplot2::element_rect(fill = cor_fundo, color = "#444444", linewidth = 0.3),
      plot.background   = ggplot2::element_rect(fill = cor_fundo, color = cor_fundo),
      text              = ggplot2::element_text(family = familia_fonte, face = "bold"),
      plot.subtitle     = ggplot2::element_text(hjust = 0.5, size = 10,
                                                 margin = ggplot2::margin(b = 15)),
      plot.margin       = ggplot2::margin(20, 30, 20, 30),
      legend.position   = "none"
    )
}


# ============================================================
# PALETA DE CORES PADRÃO
# ============================================================

#' Retorna paleta de cores padrão para um time
#'
#' @param cor_principal Cor principal do time (hex). Padrão: `"#E57373"`.
#' @param cor_fundo Cor de fundo. Padrão: `"#F7F6F2"`.
#' @keywords internal
.paleta <- function(cor_principal = "#E57373", cor_fundo = "#F7F6F2") {
  c(cor_fundo, cor_principal)
}


# ============================================================
# CARREGAMENTO DE FONTE
# ============================================================

#' Carrega a fonte Roboto Slab (Google Fonts)
#' @keywords internal
.carregar_fonte <- function() {
  if (!requireNamespace("showtext", quietly = TRUE)) return(invisible(NULL))
  sysfonts::font_add_google("Roboto Slab", "roboto_slab")
  showtext::showtext_auto()
}


# ============================================================
# VALIDAÇÃO DE TEAM ID
# ============================================================

#' Valida se um teamId existe no dataset
#' @keywords internal
.validar_team <- function(partida, team_id) {
  ids <- unique(partida$teamId)
  if (!as.character(team_id) %in% ids) {
    stop("teamId '", team_id, "' nao encontrado. IDs disponiveis: ",
         paste(ids, collapse = ", "), call. = FALSE)
  }
  as.character(team_id)
}


# ============================================================
# IDENTIFICAÇÃO DE TITULARES
# ============================================================

#' Identifica os 11 titulares de um time
#' @keywords internal
.titulares <- function(df_time, n = 11) {
  reservas_on   <- df_time |> dplyr::filter(.data$type == "SubstitutionOn") |> dplyr::pull(.data$jogador) |> unique()
  presentes     <- unique(df_time$jogador)
  titulares_out <- df_time |> dplyr::filter(.data$type == "SubstitutionOff") |> dplyr::pull(.data$jogador) |> unique()
  candidatos    <- union(setdiff(presentes, reservas_on), titulares_out)
  
  # Ordena por volume de ações para pegar os mais presentes
  volume <- df_time |>
    dplyr::filter(.data$jogador %in% candidatos) |>
    dplyr::group_by(.data$jogador) |>
    dplyr::summarise(total = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$total))
  
  head(volume$jogador, n)
}
