library(shiny)
library(dplyr)
library(tibble)
library(purrr)
library(readr)
library(stringr)
library(tidyr)
library(gtools)
library(ggplot2)
library(ggrepel)
library(viridis)
library(cowplot)
library(ComplexHeatmap) # bioconductor
library(DT)
library(plotly)
library(crosstalk)
library(shinyjs)
library(shinythemes)
library(shinycustomloader)
library(shinyjqui)
library(shinyWidgets)
library(rintrojs)
library(shinyBS)
library(bsplus)
library(Seurat)

### source R
source("config.R")
s <- readRDS("spatial_all14_harmony_annot_reduc.rds")
autocomplete_list <- c("sample", "name", "nCount_Spatial", "nFeature_Spatial", "percent_mito", "percent_ribo", rownames(s@assays$SCT@data) %>% sort())
cats <- c("sample", "name")

slides <- c("umap", 
            s@images %>% names() %>% str_remove("^[A-D]1_") %>% mixedsort(), 
            s@images %>% names() %>% str_remove("^[A-D]1_") %>% mixedsort() %>% str_c("_H&E"))
# temp <- readxl::excel_sheets("/Users/rf/Downloads/Supplmentary data 3 2.3.22.xlsx") %>%
#   .[-1] %>% as.data.frame() %>%
#   setNames("id") %>%
#   separate(id, " - ", into = c("cluster", "name")) %>%
#   mutate(name = ifelse(is.na(name), "undefined", name)) %>%
#   mutate(name = ifelse(str_detect(name, "^ "), str_remove(name, "^ "), name))
# s$sample <- s$orig.ident %>% str_remove("^[A-D]1_")
# s$cluster_0.1 <- s$SCT_snn_res.0.1
# s$cluster_0.2 <- s$SCT_snn_res.0.2
# s$cluster_0.5 <- s$SCT_snn_res.0.5
# s$cluster_0.8 <- s$SCT_snn_res.0.8
# s$name <- s@meta.data %>% left_join(temp, by = c("seurat_clusters" = "cluster")) %>% pull(name.y)
# s$name <- factor(s$name, levels = gtools::mixedsort(s$name %>% unique()))
# names(s@images) <- slides
# saveRDS(s, "spatial_all14_harmony_annot_reduc.rds")


