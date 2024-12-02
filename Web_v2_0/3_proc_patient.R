# Uso---------------------------------------------------------------------------
# DGE es la DGEList (archivo .rds)
DGE <- readRDS("~/DataLab/0_Environ/Web/Binomial/24_12_02/Web_v2_0/DGEList_61_TPM.rds")
# patient es la salida de get_patient, o en todo caso, se genera variable con string
patient <- "Br0258"
# umap_model es el modelo umap que permite predecir las coordenadas 3D
umap_model_path <- "~/DataLab/0_Environ/Web/Binomial/25_11_24/UmapModel.rds"
# binomial_model es el modelo logistico que permite predecir el score de riesgo
binomial_model_path <- "~/DataLab/0_Environ/Web/Binomial/20_11_24/DGELIST_FINAL/ResultadosFinales/Binomial/ENVIAR/binomial_10_genes.rds"

result_2 <- proc_patient(DGE, patient, umap_model_path, binomial_model_path)

# Definición--------------------------------------------------------------------
proc_patient <- function(DGE, patient, umap_model_path, binomial_model_path) {
  
  # Metricas
  metrics_patient <- DGE$samples[rownames(DGE$samples) == patient, "P_uni_map", drop = FALSE]
  
  # Umap
  library(umap)
  
  tpm_values <- DGE$counts
  tpm_values <- t(tpm_values)
  patient_data <- t(tpm_values[patient, , drop = FALSE])
  patient_data <- t(patient_data)
  
  umap_model <- readRDS(umap_model_path)
  patient_coord <- predict(umap_model, patient_data)
  coord_patient <- as.data.frame(patient_coord)
  
  coord_patient$evento <- DGE$samples[patient, "evento"]
  
  # Procesamiento para modelo binomial
  genes_modelo <- c("CAPN9", "CH25H", "TNNT3", "NAALAD2", "PLET1",
                    "GLI1", "CCDC196", "CEACAM6", "DGKG", "TSPAN7")
  
  tpm_data_patient <- DGE$counts[, patient, drop = FALSE]
  tpm_data_patient_genes <- tpm_data_patient[rownames(tpm_data_patient) %in% genes_modelo, , drop = FALSE]
  
  # Conversión a logTPM
  log2_tpm <- function(tpm) {
    return(log2(tpm + 1))
  }
  
  log2_tpm_data <- as.data.frame(apply(tpm_data_patient_genes, 2, log2_tpm))
  rownames(log2_tpm_data) <- rownames(tpm_data_patient_genes)
  log2_tpm_data <- as.matrix(t(log2_tpm_data))
  
  # Predicción
  library(glmnet)
  binomial_model <- readRDS(binomial_model_path)
  score_patient <- predict(binomial_model, newx = log2_tpm_data, s = binomial_model$lambda, type = "response")
  
  return(list(metrics_patient = metrics_patient, coord_patient = coord_patient, score_patient = score_patient))
}
