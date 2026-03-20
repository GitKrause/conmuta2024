library(arrow)       # leer parquet
library(dplyr)
library(ggplot2)
library(LabourMarketAreas)
library(sf)
library(stplanr)

# ── 1. Descargar microdatos del INE ─────────────────────────────────────────
url <- "https://storage.googleapis.com/bktdescargascenso2024/viv_hog_per_censo2024.zip"
download.file(url, "censo2024.zip", mode = "wb")
archive::archive_extract("censo2024.zip")

personas <- read_parquet("personas_censo2024.parquet")

# ── 2. Limpiar códigos territoriales ────────────────────────────────────────
# Nota: el INE publicó códigos de 4 dígitos; se necesita rellenar con cero
fix_cod <- function(x) stringr::str_pad(as.character(x), 5, pad = "0")

od_raw <- personas |>
  filter(
    !is.na(p44_lug_trab_esp),     # tiene lugar de trabajo declarado
    p44_lug_trab_esp > 0,          # excluye no responde / no aplica
    p15_act_prin == 1              # personas ocupadas
  ) |>
  mutate(
    comuna_residencia = fix_cod(codigo_comuna),
    comuna_trabajo    = fix_cod(p44_lug_trab_esp)
  ) |>
  # excluir trabajo fuera del país o códigos inválidos
  filter(
    nchar(comuna_trabajo) == 5,
    !comuna_trabajo %in% c("-0999", "-0099")
  )

# ── 3. Construir matriz OD comunal ───────────────────────────────────────────
od_matrix <- od_raw |>
  count(comuna_residencia, comuna_trabajo, name = "flujo") |>
  filter(flujo >= 10)  # umbral mínimo para evitar ruido

# ── 4. Análisis con LabourMarketAreas ────────────────────────────────────────
# El paquete espera columnas: community_live, community_work, amount
od_lma <- od_matrix |>
  rename(
    community_live = comuna_residencia,
    community_work = comuna_trabajo,
    amount         = flujo
  ) |>
  as.data.table()

# Detectar áreas de mercado laboral (agrupa comunas con alta cohesión interna)
resultado <- findClusters(
  od_lma,
  tarMinPendingFlow = 0.04,  # umbral de cohesión (4% es estándar OCDE)
  tarMinSZ          = 2000,  # tamaño mínimo del área en trabajadores
  LWClus            = TRUE
)

# ── 5. Visualizar desire lines con stplanr ───────────────────────────────────
# Necesitas centroides comunales (disponibles en la cartografía del INE)
# O bien desde el paquete chilemapas:
# install.packages("chilemapas")
library(chilemapas)

centroides <- mapa_comunas |>
  st_centroid() |>
  transmute(
    codigo_comuna,
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2]
  )

od_geo <- od_matrix |>
  filter(
    comuna_residencia != comuna_trabajo,  # solo conmutación intercomunal
    flujo >= 100
  ) |>
  left_join(centroides, by = c("comuna_residencia" = "codigo_comuna")) |>
  rename(lon_o = lon, lat_o = lat) |>
  left_join(centroides, by = c("comuna_trabajo" = "codigo_comuna")) |>
  rename(lon_d = lon, lat_d = lat) |>
  filter(!is.na(lon_o), !is.na(lon_d))

# Convertir a desire lines
deseos <- od2line(
  flow = od_geo |> select(comuna_residencia, comuna_trabajo, flujo),
  zones = mapa_comunas
)

# ── 6. Plot final ─────────────────────────────────────────────────────────────
ggplot() +
  geom_sf(data = mapa_comunas, fill = "#F1EFE8", color = "#D3D1C7", linewidth = 0.1) +
  geom_sf(
    data  = deseos,
    aes(alpha = flujo, linewidth = flujo),
    color = "#1D9E75"
  ) +
  scale_alpha_continuous(range = c(0.05, 0.6), guide = "none") +
  scale_linewidth_continuous(range = c(0.2, 2), guide = "none") +
  coord_sf(xlim = c(-76, -66), ylim = c(-56, -17)) +
  labs(
    title    = "Flujos de conmutación intercomunal · Chile",
    subtitle = "Fuente: Censo 2024 INE · Personas ocupadas (p15_act_prin == 1)",
    caption  = "Solo flujos ≥ 100 personas · Color = teal 400"
  ) +
  theme_void(base_family = "sans") +
  theme(plot.title = element_text(size = 14, face = "bold"))