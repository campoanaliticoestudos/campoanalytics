#' campoanalytics: Visualizações Táticas de Futebol
#'
#' Gera visualizações táticas profissionais a partir de dados de partida
#' no formato padrão WhoScored/Opta (CSV).
#'
#' @section Funções Principais:
#' - \code{\link{ca_carregar_partida}}: Carrega e valida o CSV da partida
#' - \code{\link{ca_passing_flow}}: Mapa de direção e volume de passes
#' - \code{\link{ca_passing_network}}: Rede de passes entre jogadores
#' - \code{\link{ca_shot_map}}: Mapa de chutes (perspectiva do goleiro)
#' - \code{\link{ca_xg_race}}: Corrida de xG acumulado por minuto
#' - \code{\link{ca_dominance_map}}: Mapa de dominância por zonas
#' - \code{\link{ca_acoes_15min}}: Distribuição de ações por 15 minutos
#' - \code{\link{ca_taxa_sucesso}}: Taxa de sucesso por tipo de evento
#' - \code{\link{ca_tabela_passes}}: Tabela de passes por bloco do campo
#'
#' @docType package
#' @name campoanalytics
"_PACKAGE"

# Importações globais necessárias
#' @importFrom rlang .data
#' @importFrom dplyr filter mutate group_by summarise ungroup left_join arrange
#' @importFrom dplyr n pull slice_max bind_rows case_when lead
#' @importFrom ggplot2 ggplot aes geom_point geom_segment geom_text geom_rect
#' @importFrom ggplot2 annotate theme_void theme element_text element_rect
#' @importFrom ggplot2 scale_fill_gradientn coord_flip labs arrow unit
#' @importFrom ggplot2 scale_color_manual scale_size_continuous ggsave
#' @importFrom ggsoccer annotate_pitch theme_pitch pitch_opta
NULL
