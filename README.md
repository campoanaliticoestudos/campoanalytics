# campoanalytics <img src="man/figures/logo.png" align="right" height="139" />

> Visualizações táticas profissionais de futebol direto do seu CSV.

**campoanalytics** é um pacote R que transforma dados de partida no formato WhoScored/Opta em visualizações táticas completas — com uma linha de código por gráfico.

---

## 📦 Instalação

```r
# Via devtools (GitHub)
# install.packages("devtools")
devtools::install_github("campoanaliticoestudos/campoanalytics")

# Ou instalação local (pasta do pacote)
devtools::install("campoanalytics/")
```

### Dependências

```r
install.packages(c(
  "tidyverse", "ggplot2", "ggsoccer", "patchwork",
  "showtext", "sysfonts", "ggrepel", "gt", "scales", "rlang"
))
```

---

## 🚀 Uso Rápido

### 1. Carregar a partida

```r
library(campoanalytics)

partida <- ca_carregar_partida("Real Madrid 3-2 Atletico Madrid.csv")

# Ver os times disponíveis
ca_times(partida)
#> # A tibble: 2 × 3
#>   teamId eventos jogadores
#>   <chr>    <int>     <int>
#> 1 52        1243        18
#> 2 63        1105        17
```

---

### 2. Passing Flow + Volume

```r
ca_passing_flow(
  partida,
  team_id       = "63",
  cor_principal = "#E57373",
  titulo        = "Atletico de Madrid — Análise de Passes",
  subtitulo     = "Real Madrid 3-2 Atletico Madrid | LaLiga 2025/26",
  salvar        = "passing_flow_atletico.png"  # opcional
)
```

---

### 3. Passing Network

```r
ca_passing_network(
  partida,
  team_id       = "63",
  cor_principal = "#E57373",
  nome_time     = "Atlético de Madrid",
  n_conexoes    = 20,
  salvar        = "passing_network_atletico.png"
)
```

---

### 4. Shot Map (Goleiro)

```r
ca_shot_map(
  partida,
  team_id      = "52",
  nome_jogador = "Thibaut Courtois",
  cor_gol      = "#e35d6a",
  cor_defesa   = "#584d77",
  salvar       = "shot_map_courtois.png"
)
```

---

### 5. xG Race Chart

```r
ca_xg_race(
  partida,
  team_id_casa = "52",
  team_id_fora = "63",
  nome_casa    = "Real Madrid",
  nome_fora    = "Atlético de Madrid",
  cor_casa     = "#000000",
  cor_fora     = "#E57373",
  titulo       = "Real Madrid 3–2 Atlético de Madrid",
  salvar       = "xg_race.png"
)
```

> ⚠️ Se o seu CSV não tiver coluna `xG`, o pacote usa valores simulados automaticamente e indica no caption.

---

### 6. Dominance Map

```r
ca_dominance_map(
  partida,
  team_id_casa  = "52",
  team_id_fora  = "63",
  cor_dominante = "#E57373",
  titulo        = "Real Madrid vs Atlético — Dominância por Zona",
  salvar        = "dominance_map.png"
)
```

---

### 7. Ações por 15 Minutos

```r
ca_acoes_15min(
  partida,
  team_id      = "63",
  cor_destaque = "#E57373",
  titulo       = "Atlético de Madrid — Ações por 15 Minutos",
  salvar       = "acoes_15min.png"
)
```

---

### 8. Taxa de Sucesso por Evento

```r
ca_taxa_sucesso(
  partida,
  nome_jogador = "Antoine Griezmann",
  cor_ponto    = "#E57373",
  salvar       = "taxa_griezmann.png"
)
```

---

### 9. Tabela de Passes por Bloco

```r
ca_tabela_passes(
  partida,
  team_id    = "63",
  top_n      = 5,
  min_passes = 3,
  titulo     = "Distribuição de Passes | Atlético de Madrid",
  salvar     = "tabela_passes.png"
)
```

---

## 📋 Formato Esperado do CSV

O CSV deve conter **obrigatoriamente** estas colunas:

| Coluna    | Tipo      | Descrição                             |
|-----------|-----------|---------------------------------------|
| `teamId`  | character | ID numérico do time                   |
| `type`    | character | Tipo do evento (Pass, Goal, Save...)  |
| `outcome` | character | Resultado (Successful / Unsuccessful) |
| `x`       | numeric   | Posição X do evento (0–100)           |
| `y`       | numeric   | Posição Y do evento (0–100)           |
| `endX`    | numeric   | Posição X final (para passes)         |
| `endY`    | numeric   | Posição Y final (para passes)         |
| `minute`  | numeric   | Minuto do evento                      |
| `jogador` | character | Nome do jogador                       |

Coluna opcional:
- `xG` — Gols esperados. Se ausente, o `ca_xg_race()` usa valores simulados.

---

## 🎨 Personalização

Todos os gráficos aceitam:
- `cor_principal` / `cor_destaque` — cor do time em hex
- `titulo` / `subtitulo` — textos personalizados
- `salvar` — caminho para exportar PNG automaticamente

---

## 📊 Funções Disponíveis

| Função                  | Visualização                            |
|-------------------------|-----------------------------------------|
| `ca_carregar_partida()` | Carrega e valida o CSV                  |
| `ca_times()`            | Lista os times no dataset               |
| `ca_passing_flow()`     | Passing Flow + Volume por zona          |
| `ca_passing_network()`  | Rede de passes entre jogadores          |
| `ca_shot_map()`         | Mapa de chutes na baliza                |
| `ca_xg_race()`          | Corrida de xG acumulado                 |
| `ca_dominance_map()`    | Mapa de dominância por zonas            |
| `ca_acoes_15min()`      | Ações por intervalo de 15 minutos       |
| `ca_taxa_sucesso()`     | Taxa de sucesso por tipo de evento      |
| `ca_tabela_passes()`    | Tabela de passes por bloco do campo     |

---

## 👤 Autor

**Aldrei Peralta** | Campo Analítico  
[campoanalitico.com.br](https://campoanalitico.com.br/)

---

*"Os dados contam a história. A visualização a revela."*
