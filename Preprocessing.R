##### Preliminaries #####
utils_path <- file.path(getwd(), "Utils")
source(file.path(utils_path, "Packages.R"))
path <- getwd()
dir <-  file.path(path, "Final_Df")
setwd(path)
crs_longlat = 4326
crs_projected = "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs"
boundary = read_sf(file.path(path, "Lago Titicaca/lago_titicaca_sideteva_puno.shp"))|>
  st_transform(crs = crs_projected)

ggplot(boundary)+
  geom_sf(aes(geometry = geometry))


data_sedimenti <- read_excel(file.path(dir, "data sediments and POM.xlsx"), sheet = "Sedimenti")
colnames(data_sedimenti) = c("Estaciones", "Date", "Zona", "d15N_Sed", "d13C_Sed")
data_sedimenti <- data_sedimenti %>%
  mutate(date = dmy(Date),
         nearest_month = floor_date(date + days(15), "month"),
         Period = month(nearest_month, label = TRUE, abbr = FALSE)) |>
  mutate(Period, case_when(
    Period == "maggio" ~ "may 2024",
    Period == "giugno" ~ "jun 2024",
    Period == "luglio" ~ "jul 2024",
    Period == "agosto" ~ "aug 2024",
    TRUE ~ Period
  ))
data_sedimenti <-  data_sedimenti[, -c(2, 7,8)]
colnames(data_sedimenti)[6] <-  c("Period")
data_sedimenti |>
  mutate(Estaciones, case_when(
    Estaciones == "B-J1" ~ "Cage", 
    Estaciones == "B-J3" ~ "Cage",
    Estaciones == "B-J5" ~ "Cage",
    Estaciones == "CH-J1" ~ "Cage",
    Estaciones == "CH-J4" ~ "Cage",
    Estaciones == "CH-J5" ~ "Cage",
    Estaciones == "B-B1" ~ "No Trouts", 
    Estaciones == "B-B2" ~ "No Trouts",
    Estaciones == "B-B3" ~ "No Trouts",
    Estaciones == "CH-B1" ~ "No Trouts",
    Estaciones == "CH-B2" ~ "No Trouts",
    Estaciones == "CH-B3" ~ "No Trouts",
    Estaciones == "P1SEFB" ~ "No Cage", 
    Estaciones == "P2SEFB" ~ "No Cage",
    Estaciones == "P3SEFB" ~ "No Cage",
    Estaciones == "P1SEFCH" ~ "No Cage",
    Estaciones == "P2SEFCH" ~ "No Cage",
    Estaciones == "P3SEFCH" ~ "No Cage",
  ))->data_sedimenti
colnames(data_sedimenti)[7] = c("method")
data_sedimenti$Sitos = tolower(data_sedimenti$Zona)
data_sedimenti$Sitos[sapply(1:length(data_sedimenti$Zona), function(i){"barco" %in% str_split(data_sedimenti$Sitos[i], pattern = "-")[[1]]})] = "barco"
data_sedimenti$Sitos[sapply(1:length(data_sedimenti$Zona), function(i){"chucasuyo" %in% str_split(data_sedimenti$Sitos[i], pattern = "-")[[1]]})] = "chucasuyo"


data_POM <-  read_excel(file.path(dir, "data sediments and POM.xlsx"), sheet = "POM")
colnames(data_POM) <-  c("Estaciones", "Date", "Zona", "d15N_POM", "d13C_POM")

data_POM <- data_POM %>%
  mutate(date = dmy(Date),
         nearest_month = floor_date(date + days(15), "month"),
         Period = month(nearest_month, label = TRUE, abbr = FALSE)) |>
  mutate(Period, case_when(
    Period == "maggio" ~ "may 2024",
    Period == "giugno" ~ "jun 2024",
    Period == "luglio" ~ "jul 2024",
    Period == "agosto" ~ "aug 2024",
    Period == "marzo" ~ "mar 2024",
    Period == "aprile" ~ "apr 2024",
    TRUE ~ Period
  ))

data_POM = data_POM[, -c(2, 7,8)]
colnames(data_POM)[6] = c("Period")
data_POM |>
  mutate(Estaciones, case_when(
    Estaciones == "B-J1" ~ "Cage", 
    Estaciones == "B-J3" ~ "Cage",
    Estaciones == "B-J5" ~ "Cage",
    Estaciones == "CH-J1" ~ "Cage",
    Estaciones == "CH-J4" ~ "Cage",
    Estaciones == "CH-J5" ~ "Cage",
    Estaciones == "B-B1" ~ "No Trouts", 
    Estaciones == "B-B2" ~ "No Trouts",
    Estaciones == "B-B3" ~ "No Trouts",
    Estaciones == "CH-B1" ~ "No Trouts",
    Estaciones == "CH-B2" ~ "No Trouts",
    Estaciones == "CH-B3" ~ "No Trouts",
    Estaciones == "P1SEFB" ~ "No Cage", 
    Estaciones == "P2SEFB" ~ "No Cage",
    Estaciones == "P3SEFB" ~ "No Cage",
    Estaciones == "P1SEFCH" ~ "No Cage",
    Estaciones == "P2SEFCH" ~ "No Cage",
    Estaciones == "P3SEFCH" ~ "No Cage",
  ))->data_POM
colnames(data_POM)[7] = c("method")
data_POM$Sitos = tolower(data_POM$Zona)
data_POM$Sitos[sapply(1:length(data_sedimenti$Zona), function(i){"barco" %in% str_split(data_POM$Sitos[i], pattern = "-")[[1]]})] = "barco"
data_POM$Sitos[sapply(1:length(data_sedimenti$Zona), function(i){"chucasuyo" %in% str_split(data_POM$Sitos[i], pattern = "-")[[1]]})] = "chucasuyo"



data_without_truchas <- read_excel(file.path(dir, "DATA COMPLETA AQUARESIDUOS 2023-2024.xlsx"), 
                                                   sheet = "data sin truchas")


stations_without_cage_sf <-  data_without_truchas |>
  filter(!is.na(LAT), !is.na(LONG)) |>
  st_as_sf(coords = c("LAT", "LONG"), crs = crs_projected)

# values = unique(stations_without_sf$Zona)
# coords_chucku = unique(st_coordinates(stations_without_sf[stations_without_sf$Zona == values[1],]))
# coords_barco = unique(st_coordinates(stations_without_sf[stations_without_sf$Zona == values[2],]))
rm(data_without_truchas)
# p1 = ggplot() +
#   geom_sf(data = boundary, fill = "lightblue", color = "black", alpha = 0.3) +
#   geom_sf(data = stations_without_sf, aes(color = Zona), size = 4) +
#   scale_color_manual(values = c("firebrick", "purple")) +  
#   theme_minimal() +
#   labs(title = "Two sites over Lake Titicaca with trouts ",
#        color = "Zona")  
# 
# 
# p1
data_with_truchas <- read_excel(file.path(dir, "DATA COMPLETA AQUARESIDUOS 2023-2024.xlsx"), 
                                   sheet = "data con truchas")


stations_with_sf <-  data_with_truchas |>
  filter(!is.na(LAT), !is.na(LONG)) |>
  st_as_sf(coords = c("LAT", "LONG"), crs = crs_projected)
rm(data_with_truchas)

# p2 = ggplot() +
#   geom_sf(data = boundary, fill = "lightblue", color = "black", alpha = 0.3) +
#   geom_sf(data = stations_with_sf, aes(color = Zona), size = 4) +
#   scale_color_manual(values = c("firebrick", "purple")) +  
#   theme_minimal() +
#   labs(title = "Stations over Lake Titicaca without Trouts",
#        color = "Zona") 
# 
# p1 + p2


stations_with_sf|>
  mutate(Mes = tolower(Mes),
    Period = paste(Mes, Año)) ->stations_with_sf

stations_without_cage_sf|>
  mutate(Mes = tolower(Mes),Period = paste(Mes, Año)) -> stations_without_cage_sf

stations_with_sf_reduce <-  stations_with_sf |>
  filter(Period %in% unique(stations_without_cage_sf$Period))


# ggplot(stations_with_sf_reduce, aes(x = pH, fill = Estaciones)) +
#   geom_boxplot(position = "dodge") +
#   labs(
#     x = "Station",
#     y = "Count",
#     fill = "Period",
#     title = "Barplot by Station and Period"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   facet_wrap(~ Period)

stations_without_cage_sf$`NO2 (mg/l)`= as.numeric(stations_without_cage_sf$`NO2 (mg/l)`)
stations_without_cage_sf$`NO2 (mg/l)`[is.na(stations_without_cage_sf$`NO2 (mg/l)`)] = 0

stations_without_cage_sf$`NO3 (mg/l)`= as.numeric(stations_without_cage_sf$`NO3 (mg/l)`)
stations_without_cage_sf$`NO3 (mg/l)`[is.na(stations_without_cage_sf$`NO3 (mg/l)`)] = 0
colnames(stations_without_cage_sf)
stations_without_cage_sf_def = stations_without_cage_sf[, c("Period", "geometry", "Zona", "Estaciones", "Niveles", "Prof", 
           "Transp", "T°", "pH", "OD", "%SO", "CE", "CHl-a", "NO2 (mg/l)", "NO3 (mg/l)", 
           "PO4 (mg/l)", "NT (mg/l)", "PT (mg/l)", "MOT", "NT (mg/kg)", "PT (mg/kg)")]
stations_without_cage_sf_def$method = "No Cage"

stations_with_sf_def = stations_with_sf_reduce[, c("Period", "geometry", "Zona", "Estaciones", "Niveles", "Prof", 
                                            "Transp", "T°", "pH", "OD", "%SO", "CE", "CHl-a", "NO2 (mg/l)", "NO3 (mg/l)", 
                                            "PO4 (mg/l)", "NT (mg/l)", "PT (mg/l)", "MOT", "NT (mg/kg)", "PT (mg/kg)")]
stations_with_sf_def$method = 0
for (i in 1:nrow(stations_with_sf_def)) {
  stations_with_sf_def$method[i] = ifelse("J" %in% str_split(stations_with_sf_def$Estaciones[i], "")[[1]] == TRUE, "Cage", "No Trouts")
  
}

stations_with_sf_def$Prof= as.numeric(stations_with_sf_def$Prof)
stations_with_sf_def$Prof[is.na(stations_with_sf_def$Prof)] = 0

stations_with_sf_def$`NO2 (mg/l)`= as.numeric(stations_with_sf_def$`NO2 (mg/l)`)
stations_with_sf_def$`NO2 (mg/l)`[is.na(stations_with_sf_def$`NO2 (mg/l)`)] = 0

stations_with_sf_def$`NO3 (mg/l)`= as.numeric(stations_with_sf_def$`NO3 (mg/l)`)
stations_with_sf_def$`NO3 (mg/l)`[is.na(stations_with_sf_def$`NO3 (mg/l)`)] = 0

stations_with_sf_def$`NT (mg/l)`= as.numeric(stations_with_sf_def$`NT (mg/l)`)
stations_with_sf_def$`NT (mg/l)`[is.na(stations_with_sf_def$`NT (mg/l)`)] = 0

stations_with_sf_def$`PT (mg/l)`= as.numeric(stations_with_sf_def$`PT (mg/l)`)
stations_with_sf_def$`PT (mg/l)`[is.na(stations_with_sf_def$`PT (mg/l)`)] = 0
total_sf = rbind(stations_with_sf_def, stations_without_cage_sf_def)
dir <-  file.path(path, "Final_Df")

# dir.create(dir)
view(total_sf)

total_sf$Sitos = tolower(total_sf$Zona)
total_sf$Sitos[sapply(1:length(total_sf$Zona), function(i){"barco" %in% str_split(total_sf$Sitos[i], pattern = "-")[[1]]})] = "barco"
total_sf$Sitos[sapply(1:length(total_sf$Zona), function(i){"chucasuyo" %in% str_split(total_sf$Sitos[i], pattern = "-")[[1]]})] = "chucasuyo"

total_sf <- total_sf |>
  mutate(Estaciones = case_when(
    Estaciones == "P1SEFB" ~ "P1-B",
    Estaciones == "P1SEMB" ~ "P1-B",
    Estaciones == "P1SESB" ~ "P1-B",
    Estaciones == "P2SEFB" ~ "P2-B",
    Estaciones == "P2SEMB" ~ "P2-B",
    Estaciones == "P2SESB" ~ "P2-B",
    Estaciones == "P3SEFB" ~ "P3-B",
    Estaciones == "P3SEMB" ~ "P3-B",
    Estaciones == "P3SESB" ~ "P3-B",
    Estaciones == "P1SEFCH" ~ "P1-CH",
    Estaciones == "P1SEMCH" ~ "P1-CH",
    Estaciones == "P1SESCH" ~ "P1-CH",
    Estaciones == "P2SEFCH" ~ "P2-CH",
    Estaciones == "P2SEMCH" ~ "P2-CH",
    Estaciones == "P2SESCH" ~ "P2-CH",
    Estaciones == "P3SEFCH" ~ "P3-CH",
    Estaciones == "P3SEMCH" ~ "P3-CH",
    Estaciones == "P3SESCH" ~ "P3-CH",
    TRUE ~ Estaciones  
  ))

#write_sf(total_sf, file.path(dir, "final.shp"))


##### Analisi Esplorative sulle differenze tra Gabbia e non Gabbia #####
final_data = read_sf(file.path(dir, "final.shp"))
colnames(final_data) = gsub(" ", "", colnames(final_data))
colnames(final_data) = gsub("/", "-", colnames(final_data))
colnames(final_data) = gsub("%", "", colnames(final_data))
colnames(final_data) = gsub("°", "", colnames(final_data))
final_data$Zona = as.factor(final_data$Zona)
levels(final_data$Zona) = tolower(levels(final_data$Zona))
acquatic = c("Transp", "T", "pH", "OD", "CE", 
             "CHl-a", "NO2(mg-l)", "NO3(mg-l)", "PO4(mg-l)", "NT(mg-l)", "PT(mg-l)")
colnames(final_data)[colnames(final_data) %in%acquatic] = c("Transp", "T", "pH", "OD", "CE", 
                                                            "CHl", "NO2", "NO3", "PO4", "NT", "PT")

acquatic = c("Transp", "T", "pH", "OD", "CE", 
             "CHl", "NO2", "NO3", "PO4", "NT", "PT")

sum(colnames(final_data) %in%acquatic)
levels(final_data$Period)

ordered_periods = c("mar 2024", "apr 2024", "may 2024", "jun 2024", "jul 2024", "aug 2024")

final_data = final_data %>%
  mutate(Period = factor(Period, levels = ordered_periods))



##### brms models ####
unique(final_data$Sitos)

final_data2 = final_data %>%
  mutate(across(all_of(acquatic), scale))


colnames(final_data)
final_data2 = as.data.frame(final_data)
final_data2[, acquatic[1:5]] = scale(final_data2[, acquatic[1:5]], center = T, scale = T)
final_data2[, acquatic[6:11]] = scale(final_data2[, acquatic[6:11]], center = T, scale = T)

bform1 <- bf(mvbind(Transp, T, pH, OD, CE) ~ Period + Sitos + Sitos:method) + 
  set_rescor(TRUE)
fit1 <- brm(bform1, data = final_data2, chains = 4, cores = 2, iter = 4000, warmup = 1500, thin = 5,
            prior =  c(prior(normal(0, 10), "b"),
                       prior(normal(0, 10), "Intercept")))


bform1 <- bf(mvbind(CHl, NO2, NO3, PO4, NT, PT) ~ Period + Sitos + Sitos:method) + 
  set_rescor(TRUE)
fit1 <- brm(bform1, data = final_data2, chains = 4, cores = 2, iter = 4000, warmup = 1500, thin = 5,
            prior =  c(prior(normal(0, 10), "b"),
                       prior(normal(0, 10), "Intercept")))


colnames(final_data)
bform1 <- bf(mvbind(CHl, NO2, NO3, PO4, NT, PT) ~ Period + Sitos + Sitos:method) + 
  set_rescor(TRUE)
fit1 <- brm(bform1, data = final_data, chains = 4, cores = 2, iter = 3000, warmup = 1500, thin = 5,
            prior =  c(prior(normal(0, 10), "b"),
                       prior(normal(0, 10), "Intercept")))
loo1 <- loo(fit1)

waic1 <- waic(fit1)
loo1
fit1 <- add_criterion(fit1, "loo")
fit1$criteria
summary(fit1)
save(fit1, file = "Nutrienti_Nested.RData")
load(file = "Nutrienti_Nested.RData")
ranef(fit1)
plot(fit1, variable = c("b_pH_Intercept", "b_OD_Intercept", "b_SO_Intercept", "b_NT_Intercept",
                        "b_Transp_Intercept"))


plot(fit1, variable = c("b_pH_Sitoschucasuyo", "b_OD_Sitoschucasuyo", 
                        "b_SO_Sitoschucasuyo", "b_NT_Sitoschucasuyo", "b_Transp_Sitoschucasuyo"))
plot(fit1, variable = c("b_pH_methodOther", "b_OD_methodOther", 
                        "b_SO_methodOther", "b_NT_methodOther", "b_Transp_methodOther"))
plot(fit1, variable = c("b_pH_methodOther", "b_OD_methodOther", 
                        "b_SO_methodOther", "b_NT_methodOther", "b_Transp_methodOther"))


plot(fit1, variable = c("b_pH_methodWithout", "b_OD_methodWithout", 
                        "b_SO_methodWithout", "b_NT_methodWithout", "b_Transp_methodWithout"))
conditional_effects(fit1)
summary(final_data2[, "NO2"])
fit1$Dic

plot(fit1, variable = c("b_pH_Periodmar2024", "b_pH_Periodmay2024", 
                        "b_pH_Periodjun2024", "b_pH_Periodjul2024"))

plot(fit1, variable = c("b_SO_Periodmar2024", "b_SO_Periodmay2024", 
                        "b_SO_Periodjun2024", "b_SO_Periodjul2024"))

plot(fit1, variable = c("b_NT_Periodmar2024", "b_NT_Periodmay2024", 
                        "b_NT_Periodjun2024", "b_NT_Periodjul2024"))

plot(fit1, variable = c("b_OD_Periodmar2024", "b_OD_Periodmay2024", 
                        "b_OD_Periodjun2024", "b_OD_Periodjul2024"))

plot(fit1, variable = c("b_Transp_Periodmar2024", "b_Transp_Periodmay2024", 
                        "b_Transp_Periodjun2024", "b_Transp_Periodjul2024"))
plot(fit1)
fit1$fit
launch_shinystan(fit1)
pairs(fit1)
