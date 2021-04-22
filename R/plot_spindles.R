#` This function takes spindle data to plot spindles

#' Graph meiotic time course spindle data
#'
#' @param file_dir file directory to excel file with spindle data with the columns strain, time, two_poles, four_poles, and total.
#' total column denotes the total number of cells counted
#' @param  color colors to be used for the graphs, you will need as many colors as strains, defaults to Okabe Ito color palette
#' @return three graphs with % of cells in MI + MII (two + four poles), %cells in MI(two poles), and % of cells in MII(four poles).


plot_spindles <- function(file_dir, color = "") {

  spindles_all <- read_excel(file_dir)
  time_points <- unique(spindles_all$time)
  spindles_all$M1_perc <- (spindles_all$two_poles/spindles_all$total)*100
  spindles_all$M2_perc <- (spindles_all$four_poles/spindles_all$total)*100
  spindles_all$M1_M2_sum <- spindles_all$M1_perc + spindles_all$M2_perc

  if(color == "") {
    cl <- c("#000000", "#E69F00", "#56B4E9", "#009E73","#F0E442", "#0072B2","#D55E00
", "#CC79A7")
    color <- cl[1:length(unique(spindles_all$strain))]

  }
  ####plots spindles MI
  a <- ggplot(spindles_all, aes(time, M1_perc))+
    geom_line(aes(color = factor(strain)))+
    theme_classic()+
    theme(axis.text.x = element_text(face="bold", size=12), axis.text.y = element_text(face = "bold", size = 12), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))+
    labs(y= "% cells in Meiosis I", x = "hours into sporulation")  +
    labs(color = "Strain") +
    scale_x_continuous(breaks = time_points) +
    scale_colour_manual(values = color)

  b <- ggplot(spindles_all, aes(time, M2_perc))+
    geom_line(aes(color = factor(strain)))+
    theme_classic()+
    theme(axis.text.x = element_text(face="bold", size=12), axis.text.y = element_text(face = "bold", size = 12), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))+
    labs(y= "% cells in Meiosis II", x = "hours into sporulation")  +
    labs(color = "Strain") +
    scale_x_continuous(breaks = time_points) +
    scale_colour_manual(values = color)

  c <- ggplot(spindles_all, aes(time, M1_M2_sum))+
    geom_line(aes(color = factor(strain)))+
    theme_classic()+
    theme(axis.text.x = element_text(face="bold", size=12), axis.text.y = element_text(face = "bold", size = 12), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))+
    labs(y= "% cells in Meiosis I + Meiosis II", x = "hours into sporulation")  +
    labs(color = "Strain") +
    scale_x_continuous(breaks = time_points) +
    scale_colour_manual(values = color)

return(ggarrange(a,b,c))
}
