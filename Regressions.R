library(tidyverse)
library(readxl)
library(latex2exp)

#####
# Wrangling
vars <- c("NY.GDP.MKTP.CN", "NE.CON.PRVT.CN",
          "NE.GDI.TOTL.CN", "NE.CON.GOVT.CN",
          "NE.EXP.GNFS.CN", "NE.IMP.GNFS.CN",
          "FM.LBL.BMNY.CN", "FR.INR.LEND")

abb <- c("Y", "C",
         "I", "G",
         "Xm", "Im",
         "M", "r")

(code <- tibble(vars,abb) %>% arrange(vars))

(df <- tibble(read.csv("C:/Users/jason/Google Drive/Projects/2263AK Macroeconomics/API_IDN_DS2_en_csv_v2_2254026/API_IDN_DS2_en_csv_v2_2254026.csv")))

(df <- df %>%
    filter(Indicator.Code %in% code$vars) %>%
    arrange(Indicator.Code) %>%
    add_column(abb = code$abb) %>%
    select(abb, num_range("X", c(1986:2019))))

(df <- df %>%
    pivot_longer(cols = -abb, names_to = "year") %>%
    pivot_wider(id_cols = year, names_from = abb))

(df$year <- c(1986:2019))

write.csv(df, "C:/Users/jason/Google Drive/Projects/2263AK Macroeconomics/data.csv")
#####

(df <- tibble(read.csv("C:/Users/jason/Google Drive/Projects/2263AK Macroeconomics/data.csv")) %>% select(-c(1)))

#####
# Yhat
Y_to_GM <- lm(Y ~ G + M, data = df)
summary(Y_to_GM)

(df <- df %>%
    mutate(Yhat = predict(Y_to_GM)))

# rhat
r_to_GM <- lm(r ~ G + M, data = df)
summary(r_to_GM)

(df <- df %>%
    mutate(rhat = predict(r_to_GM)))
#####

(df <- tibble(read.csv("C:/Users/jason/Google Drive/Projects/2263AK Macroeconomics/data_modified.csv")) %>% select(-c(1)))

#####
# C_to_Yhat
C_to_Yhat <- lm(C ~ Yhat, data = df)
summary(C_to_Yhat)

(plot <- ggplot(data = df) +
    geom_line(aes(x = Yhat/10^15, y = predict(C_to_Yhat)/10^15),colour = "darkred") +
    geom_point(aes(x = Yhat/10^15, y = C/10^15),colour = "dodgerblue4") +
    labs(title = "Terdapat Hubungan Linear Kuat Antara\nKonsumsi Rumah Tangga dan Pendapatan",
         x = TeX("\\hat{Y} (Rp1000^{5})"),
         y = TeX("C (Rp1000^{5})")))

plot + ggsave(filename = 'C:/Users/jason/Google Drive/Projects/2263AK Macroeconomics/Plot/1 Diagram C to Y.png',
              width = 15.92, height = 15.92*0.75, units = 'cm', type = 'cairo', dpi = 96)
#####



#####
# I_to_Yhat
I_to_Yhatrhat <- lm(I ~ Yhat + rhat, data = df)
summary(I_to_Yhatrhat)

(plot <- ggplot(data = df) +
    geom_line(aes(x = Yhat/10^15, y = predict(I_to_Yhatrhat)/10^15),colour = "darkred") +
    geom_point(aes(x = Yhat/10^15, y = I/10^15),colour = "dodgerblue4") +
    labs(title = "Terdapat Hubungan \"Linear\" Kuat Antara Investasi dan Pendapatan",
         x = TeX("\\hat{Y} (Rp1000^{5})"),
         y = TeX("I (Rp1000^{5})")))

plot + ggsave(filename = 'C:/Users/jason/Google Drive/Projects/2263AK Macroeconomics/Plot/3 Diagram I to Y.png',
              width = 15.92, height = 15.92*0.75, units = 'cm', type = 'cairo', dpi = 96)

(plot <- ggplot(data = df) +
    geom_line(aes(x = rhat, y = predict(I_to_Yhatrhat)/10^15),colour = "darkred") +
    geom_point(aes(x = rhat, y = I/10^15),colour = "dodgerblue4") +
    labs(title = "Terdapat Hubungan \"Linear\" Kuat Antara Investasi dan Pendapatan",
         x = TeX("\\hat{r} (%)"),
         y = TeX("I (Rp1000^{5})")))

plot + ggsave(filename = 'C:/Users/jason/Google Drive/Projects/2263AK Macroeconomics/Plot/3 Diagram I to r.png',
              width = 15.92, height = 15.92*0.75, units = 'cm', type = 'cairo', dpi = 96)
#####



#####
# Im_to_Yhat
Im_to_Yhat <- lm(Im ~ Yhat, data = df)
summary(Im_to_Yhat)

(plot <- ggplot(data = df) +
    geom_line(aes(x = Yhat/10^15, y = predict(Im_to_Yhat)/10^15),colour = "darkred") +
    geom_point(aes(x = Yhat/10^15, y = Im/10^15),colour = "dodgerblue4") +
    labs(title = "Terdapat Hubungan Linear Kuat Antara Impor dan Pendapatan",
         x = TeX("\\hat{Y} (Rp1000^{5})"),
         y = TeX("IM (Rp1000^{5})")))

plot + ggsave(filename = 'C:/Users/jason/Google Drive/Projects/2263AK Macroeconomics/Plot/2 Diagram Im to Y.png',
              width = 15.92, height = 15.92*0.75, units = 'cm', type = 'cairo', dpi = 96)
#####



write.csv(df, "C:/Users/jason/Google Drive/Projects/2263AK Macroeconomics/data_modified.csv")
cor(df$Yhat,df$C)
