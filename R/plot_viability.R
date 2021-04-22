#` This function graphs spore viability data

#' Graph spore viability data
#'
#' @param file_dir file directory to excel file with spore viability with the columns strain, alive, dead, replicates (if applicable), and treatment (if applicable).
#' @param  replicates "TRUE" or "FALSE", to denote whether the data has biological replicates
#' @param  treatment "TRUE" or "FALSE", to denote whether the data has samples treated with a drug or another chemical
#' @return a graph with % of spores viable on y axis, and strains on X axis


plot_viability <- function(file_dir, replicates = c("TRUE", "FALSE"), treatment =c("TRUE", "FALSE")) {

  spo_via_all <- read_excel(file_dir)
  strains <- unique(spo_via_all$strain)
  spo_via_all$via_percentage <- (spo_via_all$alive/(spo_via_all$alive + spo_via_all$dead))*100



  if(replicates == "TRUE") {
# plots spore viability for data with replicates and when there is treatment with drug, etc.
    if(treatment == "TRUE") {
      a <- ggplot(spo_via_all, aes(strain, via_percentage))+
        geom_boxplot(aes(color = interaction(strain,treatment)))+
        geom_point(aes(color = interaction(strain,treatment)))+
        theme_classic()+
        theme(axis.text.x = element_text(face="bold", size=12), axis.text.y = element_text(face = "bold", size = 12), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))+
        labs(y= "% Spore Viability", x = "Strain + Condition")  +
        labs(color = "strain") +
        facet_grid(cols = vars(treatment)) +
        ylim(0,100)

    }
    # plots spore viability for data with replicates and when there is no treatment with drug, etc.

    if(treatment == "FALSE") {

      a <- ggplot(spo_via_all, aes(strain, via_percentage))+
        geom_boxplot(aes(color = interaction(strain,treatment)))+
        geom_point(aes(color = interaction(strain,treatment)))+
        theme_classic()+
        theme(axis.text.x = element_text(face="bold", size=12), axis.text.y = element_text(face = "bold", size = 12), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))+
        labs(y= "% Spore Viability", x = "Strain")  +
        labs(color = "strain") +
        ylim(0,100)

    }

  }

  if(replicates == "FALSE") {
    ####plots spore viability data when there is no replicates and  there is treatment

    if(treatment == "TRUE") {

      a <- ggplot(spo_via_all, aes(strain, via_percentage))+
        geom_col(aes(color = interaction(strain,treatment), fill = interaction(strain,treatment)))+
        theme_classic()+
        theme(axis.text.x = element_text(face="bold", size=12), axis.text.y = element_text(face = "bold", size = 12), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))+
        labs(y= "% Spore Viability", x = "Strain + Condition")  +
        labs(color = "strain") +
        facet_grid(cols = vars(treatment)) +
        ylim(0,100)


    }
    ####plots spore viability data when there is no replicates and  there is treatment
    if(treatment == "FALSE") {

      a <- ggplot(spo_via_all, aes(strain, via_percentage))+
        geom_col(aes(color = strain, fill = strain))+
        theme_classic()+
        theme(axis.text.x = element_text(face="bold", size=12), axis.text.y = element_text(face = "bold", size = 12), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))+
        labs(y= "% Spore Viability", x = "Strain")  +
        labs(color = "strain") +
        ylim(0,100)


    }

  }

  return(a)

}
