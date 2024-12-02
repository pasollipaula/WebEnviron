# predict_coord
# Devuelve grafico con entrenamiento + paciente
# tiene incorporado UmapModel, con el que se predicen las coordinadas del nuevo ID
# UmapModel es donde estan precalculadas las coordenadas de los pacientes de entrenamiento

# Uso---------------------------------------------------------------------------
DGE_TPM <- readRDS("/media/4tb2/Paula/CoxLasso/Binomial/25_11_24/DGEList_61_TPM.rds")
tpm_values <- DGE_TPM$counts
tpm_values <- t(tpm_values)
new_patient_data <- t(tpm_values[1,])
umap_model_path <- "/media/4tb2/Paula/CoxLasso/Binomial/25_11_24/UmapModel.rds"
train_evento <- DGE_TPM$samples$evento

fig <- predict_coord(new_patient_data, umap_model_path, train_evento)
fig

# Definicion--------------------------------------------------------------------
predict_coord <- function(new_patient_data, umap_model_path, train_evento) {
  library(umap)
  library(plotly)
  
  umap_results <- readRDS(umap_model_path)
  
  umap_3d <- as.data.frame(umap_results$layout)
  colnames(umap_3d) <- c("UMAP1", "UMAP2", "UMAP3")
  
  new_coord <- predict(umap_results, new_patient_data)
  new_patient_df <- as.data.frame(new_coord)
  colnames(new_patient_df) <- c("UMAP1", "UMAP2", "UMAP3")
  
  umap_3d$evento <- train_evento
  new_patient_df$evento <- "0"
  
  umap_combined <- rbind(umap_3d, new_patient_df)
  combined_colors <- c(as.character(umap_3d$evento), "orange")
  
  fig <- plot_ly(
    data = umap_combined,
    x = ~UMAP1,
    y = ~UMAP2,
    z = ~UMAP3,
    color = combined_colors,
    colors = c("blue", "red", "green"),
    type = "scatter3d",
    mode = "markers",
    marker = list(size = 5)
  )
  
  return(fig)
}
