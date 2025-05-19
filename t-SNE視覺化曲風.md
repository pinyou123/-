all_genres <- all_data$Genre  


set.seed(123)
tonnetz_data_feature <- tonnetz_data[,3:37]

tonnetz_data_feature_jitter <- tonnetz_data_feature + 
  matrix(rnorm(nrow(tonnetz_data_feature) * ncol(tonnetz_data_feature), mean = 0, sd = 1e-4), 
         nrow = nrow(tonnetz_data_feature), 
         ncol = ncol(tonnetz_data_feature))
tsne_result_tonnetz <- Rtsne(tonnetz_data_feature_jitter,dim = 2,pca = T,perplexity = 50)

tsne_data_tonnetz <- data.frame(tsne_result_tonnetz$Y)
colnames(tsne_data_tonnetz) <- c("V1", "V2")  
tsne_data_tonnetz$Genre <- all_genres 

x1 <- ggplot(tsne_data_tonnetz, aes(x = V1, y = V2, color = Genre)) +
  geom_point(size = 3, alpha = 0.7) +
  theme_minimal() +
  labs(
    x = "t-SNE dim1",
    y = "t-SNE dim2",
    color = "Genre" 
  )+
  scale_color_manual(values = my_colors)+
  theme(
    axis.title = element_text(size = 24),       
    axis.text = element_text(size = 20),       
    legend.title = element_text(size = 24),     
    legend.text = element_text(size = 20)       
  )

x1
############################################################################################
set.seed(123)
tonnetz向量_feature <- tonnetz向量[,2:19]

tonnetz向量_feature_jitter <- tonnetz向量_feature + 
  matrix(rnorm(nrow(tonnetz向量_feature) * ncol(tonnetz向量_feature), mean = 0, sd = 1e-4), 
         nrow = nrow(tonnetz向量_feature), 
         ncol = ncol(tonnetz向量_feature))
tsne_result_tonnetz向量 <- Rtsne(tonnetz向量_feature_jitter,dim = 2,pca = T,perplexity = 50)

tsne向量_tonnetz <- data.frame(tsne_result_tonnetz向量$Y)
colnames(tsne向量_tonnetz) <- c("V1", "V2")  
tsne向量_tonnetz$Genre <- all_genres 

x2 <- ggplot(tsne向量_tonnetz, aes(x = V1, y = V2, color = Genre)) +
  geom_point(size = 3, alpha = 0.7) +
  theme_minimal() +
  labs(
    x = "t-SNE dim1",
    y = "t-SNE dim2",
    color = "Genre" 
  )+
  scale_color_manual(values = my_colors)+
  theme(
    axis.title = element_text(size = 24),       
    axis.text = element_text(size = 20),       
    legend.title = element_text(size = 24),     
    legend.text = element_text(size = 20)       
  )

x2
#############################################################################################

set.seed(123)
tonnetz_data_feature <- all_data[,5:57]

tonnetz_data_feature_jitter <- tonnetz_data_feature + 
  matrix(rnorm(nrow(tonnetz_data_feature) * ncol(tonnetz_data_feature), mean = 0, sd = 1e-4), 
         nrow = nrow(tonnetz_data_feature), 
         ncol = ncol(tonnetz_data_feature))
tsne_result_tonnetz <- Rtsne(tonnetz_data_feature_jitter,dim = 2,pca = T,perplexity = 50)

tsne_data_tonnetz <- data.frame(tsne_result_tonnetz$Y)
colnames(tsne_data_tonnetz) <- c("V1", "V2")  
tsne_data_tonnetz$Genre <- all_genres 

x3 <- ggplot(tsne_data_tonnetz, aes(x = V1, y = V2, color = Genre)) +
  geom_point(size = 3, alpha = 0.7) +
  theme_minimal() +
  labs(
    x = "t-SNE dim1",
    y = "t-SNE dim2",
    color = "Genre" 
  )+
  scale_color_manual(values = my_colors)+
  theme(
    axis.title = element_text(size = 24),       
    axis.text = element_text(size = 20),       
    legend.title = element_text(size = 24),     
    legend.text = element_text(size = 20)       
  )

x3
############################################################################################

set.seed(123)
mc_data_feature <- mc_data[,2:197]
mc_data_feature_jitter <- mc_data_feature+ 
  matrix(rnorm(nrow(mc_data_feature) * ncol(mc_data_feature), mean = 0, sd = 1e-4), 
         nrow = nrow(mc_data_feature), 
         ncol = ncol(mc_data_feature))
tsne_result_mc <- Rtsne(mc_data_feature_jitter,dim = 2,pca =T,perplexity = 50)

tsne_data_mc <- data.frame(tsne_result_mc$Y)
colnames(tsne_data_mc) <- c("V1", "V2")  
tsne_data_mc$Genre <- all_genres
tsne_data_mc$Display.year <- as.numeric(as.character(all_data$Display.year))

t1 <- ggplot(tsne_data_mc, aes(x = V1, y = V2, color = Genre)) +
  geom_point(size = 2.5) +
  theme_minimal() +
  labs(
    x = "t-SNE dim1",
    y = "t-SNE dim2",
    color = "Genre",
  )+
  scale_color_manual(values = my_colors)+
  theme(
    axis.title = element_text(size = 24),       
    axis.text = element_text(size = 20),       
    legend.title = element_text(size = 24),     
    legend.text = element_text(size = 20)       
  )

t1
##############################################################################################

set.seed(123)
lda_data_feature <- lda_data[,2:40]
lda_data_feature_jitter <- lda_data_feature+ 
  matrix(rnorm(nrow(lda_data_feature) * ncol(lda_data_feature), mean = 0, sd = 1e-4), 
         nrow = nrow(lda_data_feature), 
         ncol = ncol(lda_data_feature))

tsne_result_lda <- Rtsne(lda_data_feature_jitter,dim = 2,pca =T,perplexity = 50)

tsne_data_lda <- data.frame(tsne_result_lda$Y)
colnames(tsne_data_lda) <- c("V1", "V2")  
tsne_data_lda$Genre <- all_genres 

ggplot(tsne_data_lda, aes(x = V1, y = V2, color = Genre)) +
  geom_point(size = 3, alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "t-SNE lda topics視覺化",
    x = "t-SNE dim1",
    y = "t-SNE dim2",
    color = "Genre" 
  )+
  scale_color_manual(values = my_colors)
#############################################################################################
set.seed(123)
ld1_feature <- ld1[,3:12]

ld1_feature_jitter <- ld1_feature + 
  matrix(rnorm(nrow(ld1_feature) * ncol(ld1_feature), mean = 0, sd = 1e-4), 
         nrow = nrow(ld1_feature), 
         ncol = ncol(ld1_feature))

tsne_result_ld1 <- Rtsne(ld1_feature_jitter,dim = 2,pca =T,perplexity = 50)

tsne_data_ld1 <- data.frame(tsne_result_ld1$Y)
colnames(tsne_data_ld1) <- c("V1", "V2")  
tsne_data_ld1$genre <- all_genres  

ggplot(tsne_data_ld1, aes(x = V1, y = V2, color = genre)) +
  geom_point(size = 2.5, alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "t-SNE lda單音topics視覺化",
    x = "t-SNE dim1",
    y = "t-SNE dim2",
    color = "Genre" 
  )+
  scale_color_manual(values = my_colors)

###############################################################################################
set.seed(123) 

ld2_feature <- ld2[, 3:31]  

ld2_feature_jitter <- ld2_feature + 
  matrix(rnorm(nrow(ld2_feature) * ncol(ld2_feature), mean = 0, sd = 1e-4), 
         nrow = nrow(ld2_feature), 
         ncol = ncol(ld2_feature))

tsne_result_ld2 <- Rtsne(ld2_feature_jitter, dim = 2, pca = T, perplexity = 50)


tsne_data_ld2 <- data.frame(tsne_result_ld2$Y)
colnames(tsne_data_ld2) <- c("V1", "V2")  
tsne_data_ld2$Genre <- all_genres
tsne_data_ld2$Display.year <- as.numeric(as.character(all_data$Display.year))

t2 <- ggplot(tsne_data_ld2, aes(x = V1, y = V2, color = Genre)) +
  geom_point(size = 2.5) +
  theme_minimal() +
  labs(
    x = "t-SNE dim1",
    y = "t-SNE dim2",
    color = "Genre",
  )+
  scale_color_manual(values = my_colors)+
  theme(
    axis.title = element_text(size = 24),       
    axis.text = element_text(size = 20),       
    legend.title = element_text(size = 24),     
    legend.text = element_text(size = 20)       
  )

t2
############################################################################################
set.seed(123) 

ld2_feature2 <- ld2[, 3:31]  

ld2_feature_jitter2 <- ld2_feature2 + 
  matrix(rnorm(nrow(ld2_feature2) * ncol(ld2_feature2), mean = 0, sd = 1e-4), 
         nrow = nrow(ld2_feature2), 
         ncol = ncol(ld2_feature2))

tsne_result_ld2p <- Rtsne(ld2_feature_jitter2, dim = 2, pca = F, perplexity = 50)


tsne_data_ld2p <- data.frame(tsne_result_ld2p$Y)
colnames(tsne_data_ld2p) <- c("V1", "V2")  
tsne_data_ld2p$Genre <- all_genres
tsne_data_ld2p$Display.year <- as.numeric(as.character(all_data$Display.year))

t3 <- ggplot(tsne_data_ld2p, aes(x = V1, y = V2, color = Genre)) +
  geom_point(size = 2.5) +
  theme_minimal() +
  labs(
    x = "t-SNE dim1",
    y = "t-SNE dim2",
    color = "Genre",
  )+
  scale_color_manual(values = my_colors)+
  theme(
    axis.title = element_text(size = 24),       
    axis.text = element_text(size = 20),       
    legend.title = element_text(size = 24),     
    legend.text = element_text(size = 20)       
  )

t3
