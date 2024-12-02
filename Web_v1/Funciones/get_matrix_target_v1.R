# dge_matrix_target()
# Devuelve las matrices de expresión de los genes que utiliza el modelo, tanto
# para la cohorte de entrenamiento como para la de validación. Para ello,
# incluye un paso de conversión de los valroes de expresión de TPM a logTPM,
# ya que el modelo funciona con el último.

# También devuelve los valores del evento real para ambas cohortes

# Uso---------------------------------------------------------------------------
DGE_TPM_path <- "DGEList_61_TPM.rds"

result <- get_matrix_target(DGE_TPM_path)

mx_train <- result$mx_train
mx_val <- result$mx_val
evento_train <- result$evento_train
evento_val <- result$evento_val

# Definición--------------------------------------------------------------------
get_matrix_target <- function(DGE_TPM_path) {
  dge_completa <- readRDS(DGE_TPM_path)
  
  log2_tpm <- function(tpm) {
    return(log2(tpm + 1))
  }
  
  tpm_data <- dge_completa$counts
  log2_tpm_data <- as.data.frame(apply(tpm_data, 2, log2_tpm))
  rownames(log2_tpm_data) <- rownames(tpm_data)
  
  #dge_completa_logtpm <- dge_completa
  #dge_completa_logtpm $counts <- as.matrix(log2_tpm_data)
  
  genes_modelo <- c("CAPN9", "CH25H", "TNNT3", "NAALAD2", "PLET1",
                    "GLI1", "CCDC196", "CEACAM6", "DGKG", "TSPAN7")
  
  genes_filt <- genes_modelo[genes_modelo %in% rownames(log2_tpm_data)]
  log2_tpm_filt <- log2_tpm_data[genes_filt, , drop = FALSE]
  
  metadata <- dge_completa$samples
  
  mx_train <- log2_tpm_filt[, metadata$Split == "TRAIN", drop = FALSE]
  mx_val <- log2_tpm_filt[, metadata$Split == "VAL", drop = FALSE]
  mx_train <- as.data.frame(t(mx_train))
  mx_val <- as.data.frame(t(mx_val))
  
  evento_train <- metadata[metadata$Split == "TRAIN", , drop = FALSE]
  evento_val <- metadata[metadata$Split == "VAL", , drop = FALSE]
  evento_train <- data.frame(evento = evento_train$evento, row.names = rownames(evento_train))
  evento_val <- data.frame(evento = evento_val$evento, row.names = rownames(evento_val))
  
  return(list(mx_train = mx_train, mx_val = mx_val,
              evento_train = evento_train, evento_val = evento_val))
}


