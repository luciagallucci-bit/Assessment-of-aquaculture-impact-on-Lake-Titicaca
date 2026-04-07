utils_path <- file.path(getwd(), "Utils")
source(file.path(utils_path, "Packages.R"))

# you can either do 
# source("Preprocessing.R")
# or for the sake of brevity
final_data = read_sf(file.path(dir, "final.shp"))
colnames(final_data) = gsub(" ", "", colnames(final_data))
colnames(final_data) = gsub("/", "-", colnames(final_data))
colnames(final_data) = gsub("%", "", colnames(final_data))
colnames(final_data) = gsub("°", "", colnames(final_data))
final_data$Zona <-  as.factor(final_data$Zona)
levels(final_data$Zona) = tolower(levels(final_data$Zona))
acquatic = c("Transp", "T", "pH", "OD", "CE", 
             "CHl-a", "NO2(mg-l)", "NO3(mg-l)", "PO4(mg-l)", "NT(mg-l)", "PT(mg-l)")
colnames(final_data)[colnames(final_data) %in%acquatic] <-  c("Transp", "T", "pH", "OD", "CE", 
                                                              "CHl", "NO2", "NO3", "PO4", "NT", "PT")

acquatic <-  c("Transp", "T", "pH", "OD", "CE", 
               "CHl", "NO2", "NO3", "PO4", "NT", "PT")

ordered_periods <-  c("mar 2024", "apr 2024", "may 2024", "jun 2024", "jul 2024", "aug 2024")

final_data <- final_data %>%
  mutate(Period = factor(Period, levels = ordered_periods))


###### EDA starts from here #####

mat = as.data.frame(final_data)
mat

Hmisc::describe(mat[,-23])

acquatic[1:5]
M = cor(mat[, acquatic[1:5]], use = "pairwise.complete.obs")
M = cor(mat[, acquatic[6:11]], use = "pairwise.complete.obs")

?corrplot
mats <- mat[, acquatic]
?expression
colnames(mats) <- as.factor(colnames(mats))
colnames(mats) <- c("Transp","Temp","pH","DO","EC","Chl",
                    expression("NO"[2]),
                    expression("NO"[3]),
                    expression("PO"[4]),
                    "TN",
                    "TP")

expr

M = cor(mats, use = "pairwise.complete.obs")
colnames(M) <- c("Transp","Temp","pH","DO","EC","Chl",
                 "=NO[2]",
                 "=NO[3]",
                 "=PO[4]",
                 "TN",
                 "TP")
rownames(M) <- c("Transp","Temp","pH","DO","EC","Chl",
                 "=NO[2]",
                 "=NO[3]",
                 "=PO[4]",
                 "TN",
                 "TP")
col_pal = colorRampPalette(c("#2166AC", "white", "#B2182B"))(200)
# colnames(M) <- c("Transp", "Temp", "pH", "DO", "EC", "Chl", "NO₂", "NO₃", "PO₄", "TN", "PT")
# expression("PO"["4"])

custom_colors <- c("#457B9F", "#2A9D8F", "#E63946")
ol_pal <- colorRampPalette(rev(brewer.pal(11, "RdBu")))(200)

corrplot(M,
         method = "ellipse",     # ellissi: intuitive e pulite
         type = "upper",         # solo triangolo superiore
         order = "hclust",       # riordino gerarchico
         hclust.method = "ward.D2", # clustering più "compatto"
         col = col_pal,          # palette diverging
         tl.col = "black",       # etichette leggibili
         tl.cex = 1.5,           # grandezza etichette
         tl.srt = 45,            # angolo etichette
         cl.cex = 1.5,           # leggenda più leggibile
         cl.align.text = "l",    # legenda allineata a sinistra
         addrect = 3,            # cluster evidenziati
         rect.col = "grey30",    # colore rettangoli più elegante
         rect.lwd = 2,         # spessore rettangoli
         diag = FALSE,           # niente diagonale
         mar = c(0,0,2,0)
)
'corrp'



# Use them in corrplot
corrplot(M,
         method = "ellipse",
         type = "upper",
         order = "hclust",
         hclust.method = "ward.D2",
         col = col_pal,
         tl.col = "black",
         tl.cex = 1.5,
         tl.srt = 45,
         cl.cex = 1.5,
         cl.align.text = "l",
         addrect = 3,
         rect.col = "grey30",
         rect.lwd = 2,
         diag = FALSE,
         mar = c(0,0,2,0),
         tl.lab = labels   # 👈 here you can pass the expression vector
)

plot_path = file.path(getwd(), "Pictures")
plot_path_method = file.path(plot_path, "Livelli_new")
dir.create(plot_path_method)

i = 1
colnames(final_data)

long_data <- final_data %>%
  pivot_longer(cols = all_of(acquatic), 
               names_to = "Parameter", 
               values_to = "Value")



matts = matrix(NA, nrow = 2, ncol = length(acquatic))
i = 1
mat = as.data.frame(final_data)
colnames(matts) = acquatic
rownames(matts) = unique(final_data$Sitos)
for (i in 1:length(acquatic)) {
  variab = acquatic[i]
  matts[1,i] = kruskal.test(mat[final_data$Sitos == "barco", variab] ~ final_data$Niveles[final_data$Sitos == "barco"])$p.value
  matts[2,i] = kruskal.test(mat[final_data$Sitos == "chucasuyo", variab] ~ final_data$Niveles[final_data$Sitos == "chucasuyo"])$p.value
  
}

print(matts)
kruskal.test(final_data$Transp[final_data$Sitos == "barco"] ~ final_data$Niveles[final_data$Sitos == "barco"])$p.value
kruskal.test(final_data$T[final_data$Sitos == "barco"] ~ final_data$Niveles[final_data$Sitos == "barco"])
kruskal.test(final_data$pH[final_data$Sitos == "barco"] ~ final_data$Niveles[final_data$Sitos == "barco"])
kruskal.test(final_data$OD[final_data$Sitos == "barco"] ~ final_data$Niveles[final_data$Sitos == "barco"])
kruskal.test(final_data$SO[final_data$Sitos == "barco"] ~ final_data$Niveles[final_data$Sitos == "barco"])
acquatic
?p.adjust
matts_bonf = apply(matts, 1, function(x) p.adjust(x, method = "bonferroni"))
matts_bonf = t(matts_bonf)  # trasponiamo per mantenere le dimensioni originali

print(matts_bonf)
# Single plot with facet_wrap


matts2 = matrix(NA, nrow = 1, ncol = length(acquatic))
i = 1
mat = as.data.frame(final_data)
colnames(matts2) = acquatic
rownames(matts2) = unique(final_data$Sitos)
for (i in 1:length(acquatic)) {
  variab = acquatic[i]
  matts2[1,i] = kruskal.test(mat[,variab] ~ final_data$Sitos)$p.value
  
}
matts_bonf2 = apply(matts2, 1, function(x) p.adjust(x, method = "bonferroni"))
matts_bonf2 = t(matts_bonf2) 
?geom_boxplot
plot2 <- ggplot(long_data, aes(x = Sitos, y = Value, fill = Niveles)) +
  geom_boxplot(width = 0.6, outlier.shape = 21, outlier.size = 1 ) +
  facet_wrap(~Parameter, ncol = 4, scales = "free_y") +
  labs(x = "Method", y = NULL, fill = "Levels") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 11),
    strip.text = element_text(face = "bold", size = 13),  # bold facet titles
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    panel.grid.major.x = element_blank(),   
    panel.grid.major.y = element_line(color = "gray85"),  
    panel.grid.minor = element_blank())
#plot.margin = margin(10, 15, 10, 10)  # add some breathing room


print(plot2)
ggsave(plot2, filename = "Levels.png", dpi = 300, width = 15, height = 10, bg = "white")

colnames(long_data)
plot3 <- ggplot(long_data, aes(x = Sitos, y = Value, fill = Period)) +
  geom_boxplot(position = position_dodge(width = 0.7)) +
  facet_wrap(~Parameter, ncol = 4, scales = "free_y") +
  labs(x = "Method", y = NULL, fill = "Levels") +
  # scale_fill_manual(values = custom_colors) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 11),
    strip.text = element_text(face = "bold", size = 13),  # bold facet titles
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    panel.grid.major.x = element_blank(),   
    panel.grid.major.y = element_line(color = "gray85"),  
    panel.grid.minor = element_blank())
#plot.margin = margin(10, 15, 10, 10)  # add some breathing room


print(plot3)
for (i in 1:length(acquatic)) {
  plot2 = ggplot(final_data, aes(x = Sitos, y = !!sym(acquatic[i]), fill = Niveles)) +
    geom_boxplot(position = position_dodge(width = 0.8)) +
    facet_wrap(~ , ncol = 3, scales = "free_x") +
    labs(x = "Method", y = paste(acquatic[i]), fill = "Month") +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  print(plot2)
  # ggsave(plot2, filename = file.path(plot_path_method, paste(acquatic[i], ".png")), dpi = 300, bg = "white")
}

acquatic



plot_path = file.path(getwd(), "Pictures")
plot_path_method = file.path(plot_path, "Metodo_per_Mesi")
dir.create(plot_path_method)

i = 1
for (i in 1:length(acquatic)) {
  plot2 = ggplot(final_data, aes(x = method, y = !!sym(acquatic[i]), fill = Period)) +
    geom_boxplot(position = position_dodge(width = 0.8)) +
    facet_wrap(~ Sitos, ncol = 3, scales = "free_x") +
    labs(x = "Method", y = paste(acquatic[i]), fill = "Month") +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  ggsave(plot2, filename = file.path(plot_path_method, paste(acquatic[i], ".png")), dpi = 300, bg = "white")
}

acquatic




plot2 = ggpairs(mat[final_data$Sitos == "barco", acquatic])
plot2 = ggpairs(mat[final_data$Sitos == "chucasuyo", acquatic])

for (i in 1:length(acquatic)) {
  plot2 = ggplot(final_data, aes(x = Estaciones, y = !!sym(acquatic[i]), fill = Niveles)) +
    geom_boxplot(position = position_dodge(width = 0.8)) +
    facet_wrap(~ Sitos, ncol = 3, scales = "free_x") +
    labs(x = "Stations", y = paste(acquatic[i]), fill = "Level") +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  ggsave(plot2, filename = file.path(path, paste("Pictures/", sep = "", acquatic[i], ".png")), dpi = 300, bg = "white")
}

i = 1
colnames(final_data)
unique(final_data$Zona)
for (i in 1:length(acquatic)) {
  plot2 = ggplot(final_data, aes(x = Estaciones, y = !!sym(acquatic[i]))) +
    geom_boxplot(position = position_dodge(width = 0.8), aes(fill = Period)) +
    facet_wrap(~ Sitos, ncol = 2, scales = "free_x") +
    labs(x = "Period", y = paste(acquatic[i]), fill = "Stations") +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  ggsave(plot2, filename = file.path(path, paste("Pictures/", sep = "", acquatic[i], ".png")), dpi = 300, bg = "white")
}
i = 1

for (i in 1:length(acquatic)) {
  plot2 = ggplot(final_data, aes(x = Niveles, y = !!sym(acquatic[i]))) +
    geom_boxplot(position = position_dodge(width = 0.8)) +
    facet_wrap(~ Zona, ncol = 2, scales = "free_x") +
    labs(x = "Period", y = paste(acquatic[i]), fill = "Stations") +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  ggsave(plot2, filename = file.path(path, paste("Pictures/", sep = "", acquatic[i], ".png")), dpi = 300, bg = "white")
}

# zones = unique(final_data$Zona)
# plot(density(final_data$Prof))
# plot(density(final_data$Transp))
# plot(density(final_data$`T°`))
# plot(density(final_data$pH))
# plot(density(final_data$OD))
# plot(density(final_data$SO))
# plot(density(final_data$CE))
# plot(density(final_data$`CHl-a`))
# plot(density(final_data$`NO2(mg-l)`))
# plot(density(final_data$`NO3(mg-l)`))
# plot(density(final_data$`PO4(mg-l)`))
# plot(density(final_data$`NT(mg-l)`))
# plot(density(final_data$`PT(mg-kg)`))

final = as.data.frame(final_data)
length(acquatic)
pvalue = matrix(NA, ncol = length(acquatic), nrow = 1)
colnames(pvalue) = acquatic
for (i in 1:length(acquatic)) {
  pvalue[,i] = shapiro.test(final[, acquatic[i]])$p.value
  
}
pvalue

for (i in 1:length(acquatic)) {
  
  df_summary <- final_data %>%
    group_by(Sitos, method, Period) %>%
    summarise(
      mean_val = mean(.data[[acquatic[i]]], na.rm = TRUE),
      se_val   = sd(.data[[acquatic[i]]], na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
  
  plot2 <- ggplot(df_summary, aes(x = Period, y = mean_val, 
                                  color = method, group = method)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = mean_val - se_val, ymax = mean_val + se_val),
                  width = 0.2, linewidth = 0.6) +
    facet_wrap(~ Sitos, ncol = 2) +
    labs(x = "Period", y = acquatic[i], color = "Technique") +
    theme_minimal(base_size = 14) +
    theme(
      strip.text = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  ggsave(plot2, filename = file.path(path, paste0("Pictures/", acquatic[i], "_trend.png")),
         dpi = 300, width = 8, height = 5, bg = "white")
}



