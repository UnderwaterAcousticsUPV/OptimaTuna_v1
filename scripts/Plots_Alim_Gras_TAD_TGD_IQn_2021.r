

# Preparacion de datos

library(ggplot2)
library(patchwork)

df_byjal <- list()
for(i in 1:length(jal_order_seq_dates)){
  s <- sacrif_byjal[[i]]
  s <- s[1:length(seq_dates_byjal[[i]])]
  df <- data.frame(
    fecha = seq_dates_byjal[[i]],
    alim = alimento_diario_byjal[[i]],
    gras = grasa_diaria_byjal[[i]],
    alim_ind = TAD_byjal[[i]],
    gras_ind = TGD_byjal[[i]],
    calid = iqn_diario_byjal[[i]],
    gras_porc_macroAve = grasa_porc_byjal[[i]]
  )
  df_byjal[[i]] <- df
  df_byjal[[i]]$subt <- s
}

jal_order_seq_dates

# Jal index
i = 1
df <- df_byjal[[i]]
df$subt[df$subt == 0] <- NA
p_j2 <- ggplot(df, aes(x = fecha, y = alim_ind, fill = gras_porc_macroAve)) +
  geom_bar(stat = 'identity') +
  scale_fill_gradient2(mid='navy', high="limegreen", name="Grasa [%]") +
  scale_x_date(date_breaks = "1 month") +
  theme_grey() +
  labs(title = paste0("Alimentación JAL02: campaña 2021")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = " ") + labs(y = "Alimento [Kg/ind/d]") +
  geom_segment(aes(x = fecha, xend = fecha, y = 0, yend = subt/15), col="red", size=0.6) +
  geom_point(data=df, aes(x = fecha, y = subt/15), col="red", size=4) +
  scale_y_continuous(sec.axis = sec_axis(~. *15, name = "Sacrificios"), limits=c(0,20)) +
  theme(axis.text.y.right = element_text(color =  'red'), 
        axis.title.y.right = element_text(color =  'red'),
        axis.line.y.right = element_line(color = "red"),
        axis.ticks.y.right = element_line(color = "red"))


i = 2
df <- df_byjal[[i]]
df$subt[df$subt == 0] <- NA
p_j3 <- ggplot(df, aes(x = fecha, y = alim_ind, fill = gras_porc_macroAve)) +
  geom_bar(stat = 'identity') +
  scale_fill_gradient2(mid='navy', high="limegreen", name="Grasa [%]") +
  scale_x_date(date_breaks = "1 month") +
  theme_grey() +
  labs(title = paste0("Alimentación JAL03: campaña 2021")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = " ") + labs(y = "Alimento [Kg/ind/d]") +
  geom_segment(aes(x = fecha, xend = fecha, y = 0, yend = subt/15), col="red", size=0.6) +
  geom_point(data=df, aes(x = fecha, y = subt/15), col="red", size=4) +
  scale_y_continuous(sec.axis = sec_axis(~. *15, name = "Sacrificios"), limits=c(0,20)) +
  theme(axis.text.y.right = element_text(color =  'red'), 
        axis.title.y.right = element_text(color =  'red'),
        axis.line.y.right = element_line(color = "red"),
        axis.ticks.y.right = element_line(color = "red"))


i = 3
df <- df_byjal[[i]]
df$subt[df$subt == 0] <- NA
p_j4 <- ggplot(df, aes(x = fecha, y = alim_ind, fill = gras_porc_macroAve)) +
  geom_bar(stat = 'identity') +
  scale_fill_gradient2(mid='navy', high="limegreen", name="Grasa [%]") +
  scale_x_date(date_breaks = "1 month") +
  theme_grey() +
  labs(title = paste0("Alimentación JAL04: campaña 2021")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = " ") + labs(y = "Alimento [Kg/ind/d]") +
  geom_segment(aes(x = fecha, xend = fecha, y = 0, yend = subt/15), col="red", size=0.6) +
  geom_point(data=df, aes(x = fecha, y = subt/15), col="red", size=4) +
  scale_y_continuous(sec.axis = sec_axis(~. *15, name = "Sacrificios"), limits=c(0,20)) +
  theme(axis.text.y.right = element_text(color =  'red'), 
        axis.title.y.right = element_text(color =  'red'),
        axis.line.y.right = element_line(color = "red"),
        axis.ticks.y.right = element_line(color = "red"))


i = 4
df <- df_byjal[[i]]
df$subt[df$subt == 0] <- NA
p_j5 <- ggplot(df, aes(x = fecha, y = alim_ind, fill = gras_porc_macroAve)) +
  geom_bar(stat = 'identity') +
  scale_fill_gradient2(mid='navy', high="limegreen", name="Grasa [%]") +
  scale_x_date(date_breaks = "1 month") +
  theme_grey() +
  labs(title = paste0("Alimentación JAL05: campaña 2021")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = " ") + labs(y = "Alimento [Kg/ind/d]") +
  geom_segment(aes(x = fecha, xend = fecha, y = 0, yend = subt/15), col="red", size=0.6) +
  geom_point(data=df, aes(x = fecha, y = subt/15), col="red", size=4) +
  scale_y_continuous(sec.axis = sec_axis(~. *15, name = "Sacrificios"), limits=c(0,20)) +
  theme(axis.text.y.right = element_text(color =  'red'), 
        axis.title.y.right = element_text(color =  'red'),
        axis.line.y.right = element_line(color = "red"),
        axis.ticks.y.right = element_line(color = "red"))


i = 5
df <- df_byjal[[i]]
df$subt[df$subt == 0] <- NA
p_j6 <- ggplot(df, aes(x = fecha, y = alim_ind, fill = gras_porc_macroAve)) +
  geom_bar(stat = 'identity') +
  scale_fill_gradient2(mid='navy', high="limegreen", name="Grasa [%]") +
  scale_x_date(date_breaks = "1 month") +
  theme_grey() +
  labs(title = paste0("Alimentación JAL06: campaña 2021")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = " ") + labs(y = "Alimento [Kg/ind/d]") +
  geom_segment(aes(x = fecha, xend = fecha, y = 0, yend = subt/15), col="red", size=0.6) +
  geom_point(data=df, aes(x = fecha, y = subt/15), col="red", size=4) +
  scale_y_continuous(sec.axis = sec_axis(~. *15, name = "Sacrificios"), limits=c(0,20)) +
  theme(axis.text.y.right = element_text(color =  'red'), 
        axis.title.y.right = element_text(color =  'red'),
        axis.line.y.right = element_line(color = "red"),
        axis.ticks.y.right = element_line(color = "red"))


i = 6
df <- df_byjal[[i]]
df$subt[df$subt == 0] <- NA
p_j7 <- ggplot(df, aes(x = fecha, y = alim_ind, fill = gras_porc_macroAve)) +
  geom_bar(stat = 'identity') +
  scale_fill_gradient2(mid='navy', high="limegreen", name="Grasa [%]") +
  scale_x_date(date_breaks = "1 month") +
  theme_grey() +
  labs(title = paste0("Alimentación JAL07: campaña 2021")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = " ") + labs(y = "Alimento [Kg/ind/d]") +
  geom_segment(aes(x = fecha, xend = fecha, y = 0, yend = subt/15), col="red", size=0.6) +
  geom_point(data=df, aes(x = fecha, y = subt/15), col="red", size=4) +
  scale_y_continuous(sec.axis = sec_axis(~. *15, name = "Sacrificios"), limits=c(0,20)) +
  theme(axis.text.y.right = element_text(color =  'red'), 
        axis.title.y.right = element_text(color =  'red'),
        axis.line.y.right = element_line(color = "red"),
        axis.ticks.y.right = element_line(color = "red"))


i = 7
df <- df_byjal[[i]]
df$subt[df$subt == 0] <- NA
p_j11 <- ggplot(df, aes(x = fecha, y = alim_ind, fill = gras_porc_macroAve)) +
  geom_bar(stat = 'identity') +
  scale_fill_gradient2(mid='navy', high="limegreen", name="Grasa [%]") +
  scale_x_date(date_breaks = "1 month") +
  theme_grey() +
  labs(title = paste0("Alimentación JAL11: campaña 2021")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = " ") + labs(y = "Alimento [Kg/ind/d]") +
  geom_segment(aes(x = fecha, xend = fecha, y = 0, yend = subt/15), col="red", size=0.6) +
  geom_point(data=df, aes(x = fecha, y = subt/15), col="red", size=4) +
  scale_y_continuous(sec.axis = sec_axis(~. *15, name = "Sacrificios"), limits=c(0,20)) +
  theme(axis.text.y.right = element_text(color =  'red'), 
        axis.title.y.right = element_text(color =  'red'),
        axis.line.y.right = element_line(color = "red"),
        axis.ticks.y.right = element_line(color = "red"))


i = 8
df <- df_byjal[[i]]
df$subt[df$subt == 0] <- NA
p_j14 <- ggplot(df, aes(x = fecha, y = alim_ind, fill = gras_porc_macroAve)) +
  geom_bar(stat = 'identity') +
  scale_fill_gradient2(mid='navy', high="limegreen", name="Grasa [%]") +
  scale_x_date(date_breaks = "1 month") +
  theme_grey() +
  labs(title = paste0("Alimentación JAL14: campaña 2021")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = " ") + labs(y = "Alimento [Kg/ind/d]") +
  geom_segment(aes(x = fecha, xend = fecha, y = 0, yend = subt/15), col="red", size=0.6) +
  geom_point(data=df, aes(x = fecha, y = subt/15), col="red", size=4) +
  scale_y_continuous(sec.axis = sec_axis(~. *15, name = "Sacrificios"), limits=c(0,20)) +
  theme(axis.text.y.right = element_text(color =  'red'), 
        axis.title.y.right = element_text(color =  'red'),
        axis.line.y.right = element_line(color = "red"),
        axis.ticks.y.right = element_line(color = "red"))


i = 9
df <- df_byjal[[i]]
df$subt[df$subt == 0] <- NA
p_j18 <- ggplot(df, aes(x = fecha, y = alim_ind, fill = gras_porc_macroAve)) +
  geom_bar(stat = 'identity') +
  scale_fill_gradient2(mid='navy', high="limegreen", name="Grasa [%]") +
  scale_x_date(date_breaks = "1 month") +
  theme_grey() +
  labs(title = paste0("Alimentación JAL18: campaña 2021")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = " ") + labs(y = "Alimento [Kg/ind/d]") +
  geom_segment(aes(x = fecha, xend = fecha, y = 0, yend = subt/15), col="red", size=0.6) +
  geom_point(data=df, aes(x = fecha, y = subt/15), col="red", size=4) +
  scale_y_continuous(sec.axis = sec_axis(~. *15, name = "Sacrificios"), limits=c(0,20)) +
  theme(axis.text.y.right = element_text(color =  'red'), 
        axis.title.y.right = element_text(color =  'red'),
        axis.line.y.right = element_line(color = "red"),
        axis.ticks.y.right = element_line(color = "red"))


i = 10
df <- df_byjal[[i]]
df$subt[df$subt == 0] <- NA
p_j19 <- ggplot(df, aes(x = fecha, y = alim_ind, fill = gras_porc_macroAve)) +
  geom_bar(stat = 'identity') +
  scale_fill_gradient2(mid='navy', high="limegreen", name="Grasa [%]") +
  scale_x_date(date_breaks = "1 month") +
  theme_grey() +
  labs(title = paste0("Alimentación JAL19: campaña 2021")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = " ") + labs(y = "Alimento [Kg/ind/d]") +
  geom_segment(aes(x = fecha, xend = fecha, y = 0, yend = subt/15), col="red", size=0.6) +
  geom_point(data=df, aes(x = fecha, y = subt/15), col="red", size=4) +
  scale_y_continuous(sec.axis = sec_axis(~. *15, name = "Sacrificios"), limits=c(0,20)) +
  theme(axis.text.y.right = element_text(color =  'red'), 
        axis.title.y.right = element_text(color =  'red'),
        axis.line.y.right = element_line(color = "red"),
        axis.ticks.y.right = element_line(color = "red"))


i = 11
df <- df_byjal[[i]]
df$subt[df$subt == 0] <- NA
p_j20 <- ggplot(df, aes(x = fecha, y = alim_ind, fill = gras_porc_macroAve)) +
  geom_bar(stat = 'identity') +
  scale_fill_gradient2(mid='navy', high="limegreen", name="Grasa [%]") +
  scale_x_date(date_breaks = "1 month") +
  theme_grey() +
  labs(title = paste0("Alimentación JAL20: campaña 2021")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = " ") + labs(y = "Alimento [Kg/ind/d]") +
  geom_segment(aes(x = fecha, xend = fecha, y = 0, yend = subt/15), col="red", size=0.6) +
  geom_point(data=df, aes(x = fecha, y = subt/15), col="red", size=4) +
  scale_y_continuous(sec.axis = sec_axis(~. *15, name = "Sacrificios"), limits=c(0,20)) +
  theme(axis.text.y.right = element_text(color =  'red'), 
        axis.title.y.right = element_text(color =  'red'),
        axis.line.y.right = element_line(color = "red"),
        axis.ticks.y.right = element_line(color = "red"))




(p_j2 + p_j3 + p_j4)/
  (p_j6 + p_j7 + p_j11)/
    (p_j14 + p_j18 + p_j19)