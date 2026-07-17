library(sf)
library(ggplot2)
library(ggnewscale)
library(ggspatial)
library(cowplot)
library(grid)
library(magick)

# ------------------------------------------------------------
# 1. Parámetros
# ------------------------------------------------------------
Huepetuhe <- st_read("SHP/Huepetuhe.shp", quiet = TRUE)
escala_mapa <- 390000
intervalo_grilla <- 10000

Huepetuhe_utm <- st_transform(Huepetuhe, 32719)

escala_mapa     <- 390000
intervalo_grilla <- 10000


# ------------------------------------------------------------
# 2. Calcular una extensión proporcionada automáticamente
# ------------------------------------------------------------

mapa_base <- mapview(
  st_transform(Huepetuhe_utm, 4326),
  col.regions = NA,
  color = "red",
  lwd = 3
)

# Se abrirá el visor interactivo.
# Escoge la herramienta rectángulo, dibuja el box y pulsa Done.
seleccion <- mapedit::drawFeatures(
  mapa_base,
  sf = TRUE
)

# Verificar que se haya dibujado una geometría
if (is.null(seleccion) || nrow(seleccion) == 0) {
  stop("No se dibujó ninguna extensión.")
}

# ------------------------------------------------------------
# 3. Convertir el box a UTM y obtener límites
# ------------------------------------------------------------

box_utm <- seleccion |>
  st_make_valid() |>
  st_transform(32719)

bbox_distrito <- st_bbox(box_utm)



ancho_bbox <- bbox_distrito["xmax"] - bbox_distrito["xmin"]
alto_bbox  <- bbox_distrito["ymax"] - bbox_distrito["ymin"]

# Márgenes alrededor del distrito
margen_x <- ancho_bbox * 0.06
margen_y <- alto_bbox  * 0.14

xmin <- unname(bbox_distrito["xmin"] - margen_x)
xmax <- unname(bbox_distrito["xmax"] + margen_x)
ymin <- unname(bbox_distrito["ymin"] - margen_y)
ymax <- unname(bbox_distrito["ymax"] + margen_y)

# ------------------------------------------------------------
# 3. Crear cortes de la cuadrícula
# ------------------------------------------------------------

cortes_x <- seq(
  floor(xmin / intervalo_grilla) * intervalo_grilla,
  ceiling(xmax / intervalo_grilla) * intervalo_grilla,
  by = intervalo_grilla
)

cortes_y <- seq(
  floor(ymin / intervalo_grilla) * intervalo_grilla,
  ceiling(ymax / intervalo_grilla) * intervalo_grilla,
  by = intervalo_grilla
)

formato_utm <- function(x) {
  format(
    round(x),
    scientific = FALSE,
    trim = TRUE,
    big.mark = ""
  )
}






library(sf)
library(terra)
Huepetuhe <- st_read("SHP/Huepetuhe.shp", quiet = TRUE)

library(elevatr)
elev = get_elev_raster(Huepetuhe, z=12)
Poligo_alt    <- crop(elev, Huepetuhe)                           #
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, Huepetuhe)
plot(Poligo_alt)

slopee    = terrain(Poligo_alt  , opt = "slope")
aspecte    = terrain(Poligo_alt, opt = "aspect")

library(raster)
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)
plot(hille )

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)

# ------------------------------------------------------------
# 4. Mapa principal
# ------------------------------------------------------------
library(sf)
library(terra)
library(ggplot2)
library(ggnewscale)
library(ggspatial)
library(dplyr)
library(tibble)
library(grid)

options(scipen = 999)

# ------------------------------------------------------------
# 1. Leer el límite distrital
# ------------------------------------------------------------

Huepetuhe <- st_read(
  "SHP/Huepetuhe.shp",
  quiet = TRUE
)

# Corregir posibles geometrías inválidas
Huepetuhe <- st_make_valid(Huepetuhe)

# Transformar a WGS 84 / UTM zona 19S
Huepetuhe_utm <- st_transform(
  Huepetuhe,
  32719
)

# ------------------------------------------------------------
# 2. Descargar MapBiomas Perú 2024
# ------------------------------------------------------------

url_2024 <- paste0(
  "https://storage.googleapis.com/mapbiomas-public/",
  "initiatives/peru/collection_3/LULC/",
  "peru_collection3_integration_v1-classification_2024.tif"
)

carpeta_mapbiomas <- "tif/mapbiomas_peru_2024"

if (!dir.exists(carpeta_mapbiomas)) {
  dir.create(
    carpeta_mapbiomas,
    recursive = TRUE
  )
}

archivo_2024 <- file.path(
  carpeta_mapbiomas,
  "mapbiomas_peru_2024.tif"
)

# Descargar solamente cuando el archivo no existe
if (!file.exists(archivo_2024)) {
  
  message("Descargando MapBiomas Perú 2024...")
  
  download.file(
    url = url_2024,
    destfile = archivo_2024,
    mode = "wb"
  )
  
} else {
  
  message("El archivo MapBiomas 2024 ya existe.")
}

# ------------------------------------------------------------
# 3. Abrir el ráster de cobertura
# ------------------------------------------------------------

mapbiomas_2024 <- terra::rast(
  archivo_2024
)

names(mapbiomas_2024) <- "codigo"

# Comprobar sistemas de coordenadas
crs(mapbiomas_2024)
st_crs(Huepetuhe)

# ------------------------------------------------------------
# 4. Recortar MapBiomas con Huepetuhe
# ------------------------------------------------------------

# Transformar el polígono al CRS original de MapBiomas
Huepetuhe_mapbiomas <- terra::project(
  terra::vect(Huepetuhe),
  crs(mapbiomas_2024)
)

# Recortar primero por extensión
mapbiomas_huepetuhe <- terra::crop(
  mapbiomas_2024,
  Huepetuhe_mapbiomas
)

# Enmascarar usando el límite exacto
mapbiomas_huepetuhe <- terra::mask(
  mapbiomas_huepetuhe,
  Huepetuhe_mapbiomas
)

# ------------------------------------------------------------
# 5. Reproyectar la cobertura a UTM 19S
# ------------------------------------------------------------

# Para variables categóricas siempre utilizar vecino más cercano
mapbiomas_huepetuhe_utm <- terra::project(
  mapbiomas_huepetuhe,
  "EPSG:32719",
  method = "near"
)

names(mapbiomas_huepetuhe_utm) <- "codigo"

# ------------------------------------------------------------
# 6. Tabla oficial de clases y colores
# ------------------------------------------------------------

leyenda_mapbiomas <- tribble(
  ~codigo, ~clase,                                      ~color,
  3,      "Bosque",                                    "#1F8D49",
  4,      "Bosque seco",                               "#7DC975",
  5,      "Manglar",                                   "#04381D",
  6,      "Bosque inundable",                          "#026975",
  9,      "Plantación forestal",                       "#7A5900",
  11,      "Zona pantanosa o pastizal inundable",       "#519799",
  12,      "Pastizal / herbazal",                       "#D6BC74",
  13,      "Otra formación no boscosa",                 "#D89F5C",
  15,      "Pasto",                                     "#EDDE8E",
  18,      "Agricultura",                               "#E974ED",
  21,      "Purma",                                     "#FFEFC3",
  22,      "Área sin vegetación",                       "#D4271E",
  23,      "Playa",                                     "#FFA07A",
  24,      "Infraestructura urbana",                    "#D4271E",
  25,      "Otra área sin vegetación",                  "#DB4D4F",
  26,      "Cuerpo de agua",                            "#2532E4",
  29,      "Afloramiento rocoso",                       "#FFAA5F",
  30,      "Minería",                                   "#9C0027",
  31,      "Acuicultura",                               "#091077",
  33,      "Río",                                       "#2532E4",
  35,      "Palma aceitera",                            "#9065D0",
  40,      "Arroz",                                     "#C71585",
  66,      "Matorral",                                  "#A89358",
  68,      "Otra área natural sin vegetación",          "#E97A7A",
  72,      "Otros cultivos",                            "#910046"
)

# ------------------------------------------------------------
# 7. Convertir el ráster recortado a data.frame
# ------------------------------------------------------------

cobertura_2024_df <- terra::as.data.frame(
  mapbiomas_huepetuhe_utm,
  xy = TRUE,
  na.rm = TRUE
)

names(cobertura_2024_df) <- c(
  "x",
  "y",
  "codigo"
)

# Convertir el código a número
cobertura_2024_df$codigo <- as.numeric(
  cobertura_2024_df$codigo
)

# Agregar nombre y color de la clase
cobertura_2024_df <- cobertura_2024_df %>%
  left_join(
    leyenda_mapbiomas,
    by = "codigo"
  ) %>%
  filter(!is.na(clase))

# ------------------------------------------------------------
# 8. Seleccionar únicamente las clases presentes
# ------------------------------------------------------------

clases_presentes <- cobertura_2024_df %>%
  distinct(
    codigo,
    clase,
    color
  ) %>%
  arrange(
    match(
      codigo,
      c(
        3, 6, 24, 30, 25,
        15, 23, 21, 33, 11,
        18, 9, 22, 26, 68,
        4, 5, 12, 13, 29,
        31, 35, 40, 66, 72
      )
    )
  )

# Mostrar las clases encontradas
print(clases_presentes)

# Vector de colores para ggplot
colores_cobertura <- setNames(
  clases_presentes$color,
  clases_presentes$clase
)

# Orden de aparición en la leyenda
orden_clases <- clases_presentes$clase

cobertura_2024_df <- cobertura_2024_df %>%
  mutate(
    clase = factor(
      clase,
      levels = orden_clases
    )
  )



















mapa_relieve <- ggplot() +
  
  # Sombreado del relieve
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  
  ggnewscale::new_scale_fill() +
  
  
  # ----------------------------------------------------------
# Cobertura MapBiomas 2024
# ----------------------------------------------------------

geom_raster(
  data = cobertura_2024_df,
  aes(
    x = x,
    y = y,
    fill = clase
  ),
  alpha = 0.72,
  show.legend = TRUE
) +
  
  scale_fill_manual(
    name = "Cobertura y uso del suelo",
    values = colores_cobertura,
    breaks = orden_clases,
    drop = TRUE,
    na.translate = FALSE,
    guide = guide_legend(
      nrow = 4,
      byrow = TRUE,
      title.position = "top",
      label.position = "right",
      keywidth = unit(0.55, "cm"),
      keyheight = unit(0.30, "cm"),
      override.aes = list(
        alpha = 1
      )
    )
  ) +
  
  # ----------------------------------------------------------
# Límite distrital
# ----------------------------------------------------------

  
  # Límite distrital
  geom_sf(
    data = Huepetuhe_utm,
    fill = NA,
    color = "black",
    linewidth = 0.55
  ) +
  
  # Extensión
  coord_sf(
    xlim = c(xmin, xmax),
    ylim = c(ymin, ymax),
    crs = st_crs(32719),
    datum = st_crs(32719),
    expand = FALSE,
    clip = "on"
  ) +
  
  # Coordenadas Este
  scale_x_continuous(
    breaks = cortes_x,
    labels = formato_utm,
    sec.axis = dup_axis(
      name = NULL,
      labels = formato_utm
    ),
    expand = expansion(mult = 0)
  ) +
  
  # Coordenadas Norte
  scale_y_continuous(
    breaks = cortes_y,
    labels = formato_utm,
    sec.axis = dup_axis(
      name = NULL,
      labels = formato_utm
    ),
    expand = expansion(mult = 0)
  ) +
  
  # Norte
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    height = unit(1.35, "cm"),
    width = unit(1.35, "cm"),
    pad_x = unit(0.55, "cm"),
    pad_y = unit(0.45, "cm"),
    style = north_arrow_fancy_orienteering
  ) +
  
  # Escala gráfica
  annotation_scale(
    location = "bl",
    width_hint = 0.20,
    unit_category = "metric",
    text_cex = 0.72,
    line_width = 0.65,
    pad_x = unit(0.55, "cm"),
    pad_y = unit(0.45, "cm")
  ) +
  
  labs(
    x = NULL,
    y = NULL,
    fill= "Cobertura"
  ) +
  
  theme_bw(base_size = 8) +
  
  theme(
    # Cuadrícula
    
    # --------------------------------------------------------
    # Leyenda dentro del mapa, abajo y centrada
    # --------------------------------------------------------
    
    legend.position = c(
      0.50,
      0.035
    ),
    
    legend.justification = c(
      0.50,
      0
    ),
    
    legend.direction = "horizontal",
    
    legend.box = "horizontal",
    
    legend.background = element_rect(
      fill = scales::alpha(
        "white",
        0.90
      ),
      color = "black",
      linewidth = 0.40
    ),
    
    legend.margin = margin(
      t = 4,
      r = 6,
      b = 4,
      l = 6
    ),
    
    legend.key = element_rect(
      fill = NA,
      color = NA
    ),
    
    legend.text = element_text(
      size = 6.6,
      color = "black"
    ),
    
    legend.spacing.x = unit(
      0.13,
      "cm"
    ),
    
    legend.spacing.y = unit(
      0.05,
      "cm"
    ),
    
    legend.title = element_text(
      color = "black",
      face = "bold",
      size = 11
    ),
    
    panel.grid.major = element_line(
      color = "grey88",
      linewidth = 0.30
    ),
    
    panel.grid.minor = element_blank(),
    
    # Marco del mapa
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.9
    ),
    
    # Coordenadas
    axis.text = element_text(
      size = 7.2,
      color = "black"
    ),
    
    axis.text.x.top = element_text(
      angle = 0,
      margin = margin(b = 3)
    ),
    
    axis.text.x.bottom = element_text(
      angle = 0,
      margin = margin(t = 3)
    ),
    
    axis.text.y.left = element_text(
      angle = 90,
      margin = margin(r = 3)
    ),
    
    axis.text.y.right = element_text(
      angle = 90,
      margin = margin(l = 3)
    ),
    
    axis.ticks = element_line(
      color = "black",
      linewidth = 0.45
    ),
    
    axis.ticks.length = unit(0.11, "cm"),
    
    # Reducir espacios externos
    plot.margin = margin(
      t = 4,
      r = 8,
      b = 4,
      l = 8,
      unit = "pt"
    )
  )
  


mapa_relieve 


ggsave(
  filename = "mapa_relieve.png",
  plot = mapa_relieve,
  width = 21,
  height = 24,
  units = "cm",
  dpi = 900,
  bg = "white",
  limitsize = FALSE
)



















