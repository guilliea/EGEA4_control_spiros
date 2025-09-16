# Fonction permettant de changer les valeurs de spiros pour un sujet 
fun_change_curve <- function(data, nodos, nCVF_L, nVEMS_L, nDEP_Ls, nDEM75_Ls, nDEM50_Ls, nDEM25_Ls, nDEM2575_Ls) {
  data$CVF_L[data$nodos == nodos] <- nCVF_L
  data$VEMS_L[data$nodos == nodos] <- nVEMS_L
  data$DEP_Ls[data$nodos == nodos] <- nDEP_Ls
  data$DEM75_Ls[data$nodos == nodos] <- nDEM75_Ls
  data$DEM50_Ls[data$nodos == nodos] <- nDEM50_Ls
  data$DEM25_Ls[data$nodos == nodos] <- nDEM25_Ls
  data$DEM2575_Ls[data$nodos == nodos] <- nDEM2575_Ls
  return(data)
}
  
  
  
  
  
