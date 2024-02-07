# Instala el paquete data.table si no lo has hecho
# install.packages("data.table")
install.packages("png")
install.packages("ggplot2")
install.packages("cowplot")

library(png)
library(grid)
library(data.table)
library(ggplot2)
library(cowplot)
library(gridExtra)


# Cargar el conjunto de datos completo y manejar las fechas
dt <- read.csv("C:/Users/roros/Documents/Cursos y Capacitaciones/R/Curso Data Science R/Curso 4/household_power_consumption.txt", sep = ";",
               header = TRUE, na.strings = "?", stringsAsFactors = FALSE)

df <- as.data.frame(dt)

# Convertir 'Date' a Date class y 'Time' a POSIXct class
df$Date <- as.Date(df$Date, format = "%d/%m/%Y")
df$Time <- strptime(df$Time, format = "%H:%M:%S")

# Filtrar las filas dentro del rango de fechas especificado
df_filtered <- subset(df, Date >= as.Date("2007-02-01") & Date <= as.Date("2007-02-02"))

str(df_filtered)
head(df_filtered,10)

df_filtered$DateTime <- as.POSIXct(paste(df_filtered$Date, format(df_filtered$Time, "%H:%M:%S")), format="%Y-%m-%d %H:%M:%S")

# Paso 2: Graficar el Global Active Power en función del tiempo

plot2 <- ggplot(df_filtered, aes(x=DateTime, y=Global_active_power)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 day", labels = function(x) format(x, "%a")) +
  labs(x = NULL, y = "Global Active Power") +
  theme_minimal()+theme_minimal() + 
  theme(plot.background = element_rect(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  coord_cartesian(clip = "off") +
  geom_hline(yintercept = -Inf, color = "black") +
  geom_hline(yintercept = Inf, color = "black") +
  geom_vline(xintercept = -Inf, color = "black") +
  geom_vline(xintercept = Inf, color = "black")
plot2