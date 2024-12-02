# Uso---------------------------------------------------------------------------
# DGE es la DGEList (archivo .rds)
DGE <- readRDS("~/DataLab/0_Environ/Web/Binomial/24_12_02/Web_v2_0/DGEList_61_TPM.rds")
# patient es la salida de get_patient, o en todo caso, se genera variable con string
patient <- "Br0258"
result <- proc_others(DGE, patient = "Br0258")

# Definición--------------------------------------------------------------------
proc_others <- function(DGE, patient) {
  
  # Saco el paciente del procesamiento
  filtered_samples <- DGE$samples[rownames(DGE$samples) != patient, ]
  
  # Métricas
  metrics <- data.frame(P_uni_map = filtered_samples$P_uni_map, row.names = rownames(filtered_samples))
  
  # Coordenadas para Umap 3D
  coord_cols <- c("coord_col1", "coord_col2", "coord_col3")
  if (!all(coord_cols %in% colnames(filtered_samples))) {
    stop("DGE$samples must contain columns: coord_col1, coord_col2, coord_col3")
  }
  coord <- as.matrix(filtered_samples[, coord_cols])
  rownames(coord) <- rownames(filtered_samples)
  
  # Score riesgo
  if (!"score" %in% colnames(filtered_samples)) {
    stop("DGE$samples must contain a column: score")
  }
  scores <- data.frame(score = filtered_samples$score, row.names = rownames(filtered_samples))
  
  # Evento real
  if (!"evento" %in% colnames(filtered_samples)) {
    stop("DGE$samples must contain a column: evento")
  }
  evento <- data.frame(evento = filtered_samples$evento, row.names = rownames(filtered_samples))
  
  return(list(metrics = metrics, coord = coord, scores = scores, evento = evento))
}
