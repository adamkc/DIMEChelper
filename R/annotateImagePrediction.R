#' Image Annotation
#'
#' @param plotDataOne A single line to annotate from a plotData file
#' @param imageDir Location of images to annotate
#' @param outputDir Target directory for new images
#' @param classes vector of class names
#'
#' @return The function exports the provided chips with neat bar graph of
#' classification.
#'
#' @export
annotateImagePrediction <- function(plotDataOne=plotData[1,],
                                    imageDir=positiveLoc,
                                    classes = classNames,
                                    outputDir=file.path(outputDir,
                                                        "PositiveImages")){
  if(!dir.exists(outputDir)) dir.create(outputDir)
  img <- jpeg::readJPEG(file.path(imageDir,plotDataOne$Image))
  g <- grid::rasterGrob(img)
  plot <- plotDataOne %>% tidyr::gather(Var,Val,1:length(classes)) %>%
    .[order(.$Val,decreasing=TRUE),] %>%
    dplyr::mutate(Val = round(Val,2),
           Var = factor(Var,levels=rev(Var)),
           Trespass = (Var=="TrespassPlants" |
                         Var == "TrespassHoles" |
                         Var == "TrespassBushes")) %>%
    .[1:3,] %>%
    ggplot(aes(x=Var,y=Val)) +
    geom_bar(stat="identity",aes(fill=Trespass)) +
    geom_text(aes(label=Var,y=0.05),hjust="left",size=2.5) +
    geom_text(aes(label=Val,y=0.6),hjust="left",size=2.5) +
    ylim(0,1) + coord_flip() + theme_classic() + guides(fill=FALSE) +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks.x = element_blank(),plot.margin = unit(c(0,0,-10,0),"pt"))

  combinedplot <- cowplot::plot_grid(plot,g,ncol = 1,rel_heights = c(.3,1))
  ggsave(plot = combinedplot,
         filename = plotDataOne$Image,
         path = outputDir,
         height=2.5,width=1.7,units = "in")

}

