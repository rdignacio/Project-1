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
df_filtered$DateTime <- as.POSIXct(paste(df_filtered$Date, format(df_filtered$Time, "%H:%M:%S")), format="%Y-%m-%d %H:%M:%S")

#########################################################################
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
#########################################################################
plot3 <- ggplot(df_filtered, aes(x=DateTime)) +
  geom_line(aes(y=Sub_metering_1, color="Sub_metering_1"), linetype="solid") +
  geom_line(aes(y=Sub_metering_2, color="Sub_metering_2"), linetype="solid") +
  geom_line(aes(y=Sub_metering_3, color="Sub_metering_3"), linetype="solid") +
  scale_x_datetime(date_breaks = "1 day", labels = function(x) format(x, "%a")) +
  labs(x = NULL, y = "Energy Sub Metering") +
  scale_color_manual(name="Sub Metering",
                     values=c("Sub_metering_1"="red", "Sub_metering_2"="orange", "Sub_metering_3"="blue")) +
  theme_minimal() +
  theme(plot.background = element_rect(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.7, 0.85),  # Colocar la leyenda en la posición deseada
        legend.spacing.x = unit(0.1, "cm"),  # Espacio horizontal entre elementos de la leyenda
        legend.spacing.y = unit(0.1, "cm"),  # Espacio vertical entre elementos de la leyenda
        legend.key.size = unit(0.5, "lines"),
        legend.text = element_text(size = 7, color = "black"),
        legend.title = element_text(size = 9, color = "black"),# Tamaño de los símbolos de la leyenda
        plot.margin = margin(1, 1, 1, 1, "cm"))+
  geom_hline(yintercept = -Inf, color = "black") +
  geom_hline(yintercept = Inf, color = "black") +
  geom_vline(xintercept = -Inf, color = "black") +
  geom_vline(xintercept = Inf, color = "black")

plot3

#########################################################################

plot4 <- ggplot(df_filtered, aes(x=DateTime, y=Voltage)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 day", labels = function(x) format(x, "%a")) +
  labs(x = NULL, y = "Voltage") +
  theme_minimal()+ 
  theme(plot.background = element_rect(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  coord_cartesian(clip = "off") +
  geom_hline(yintercept = -Inf, color = "black") +
  geom_hline(yintercept = Inf, color = "black") +
  geom_vline(xintercept = -Inf, color = "black") +
  geom_vline(xintercept = Inf, color = "black")

plot4
#########################################################################

plot5 <- ggplot(df_filtered, aes(x=DateTime, y=Global_reactive_power)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 day", labels = function(x) format(x, "%a")) +
  labs(x = NULL, y = "Global Reactive Power") +
  theme_minimal() +
  theme(plot.background = element_rect(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  coord_cartesian(clip = "off") +
  geom_hline(yintercept = -Inf, color = "black") +
  geom_hline(yintercept = Inf, color = "black") +
  geom_vline(xintercept = -Inf, color = "black") +
  geom_vline(xintercept = Inf, color = "black")

plot5
#########################################################################

# Definir la configuración de los márgenes y el tamaño del borde negro
margins <- 0.5  # Tamaño del margen en cm
size <- 1  # Tamaño relativo del borde negro

# Crear el gráfico combinado
combined_plot <- plot_grid(plot2 + theme(plot.margin = unit(rep(margins, 4), "cm"), plot.title = element_text(hjust = 0.5)),
                           plot4 + theme(plot.margin = unit(rep(margins, 4), "cm"), plot.title = element_text(hjust = 0.5)),
                           plot3 + theme(plot.margin = unit(rep(margins, 4), "cm"), plot.title = element_text(hjust = 0.5)),
                           plot5 + theme(plot.margin = unit(rep(margins, 4), "cm"), plot.title = element_text(hjust = 0.5)),
                           ncol = 2, align = "hv", rel_widths = c(size, size), rel_heights = c(size, size))

combined_plot
