utils_path <- file.path(getwd(), "Utils")
source(file.path(utils_path, "Packages.R"))
path <- getwd()
dir <-  file.path(path, "Final_Df")
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



###### Different Models Start from here ###### 


final_data2 <-  as.data.frame(final_data)
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
