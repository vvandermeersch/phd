create_consensus_map <- function(shp, species, map, type, annotate=FALSE, max=NULL){
  if(type=="n_sources"){
    plot <- ggplot() +
      geom_sf(data = map, fill = 'grey', alpha=0.1, color='grey') +
      geom_sf(data= shp, aes(fill=factor(n)), color=NA) + 
      scale_fill_manual(name = "Number of data sources", values=c("#52b788", "#2d6a4f")) + 
      theme_void() +
      theme(legend.text = ggtext::element_markdown(size = 15, family = 'Linux Libertine G'),
            legend.title = ggtext::element_markdown(size = 20, family = 'Linux Libertine G'),
            legend.title.align = 0.5,
            legend.position="bottom", legend.direction = "horizontal",
            plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm")) +
      guides(fill = guide_legend(title.position="top")) 
    
  }else if(type=="sources"){
    plot <- ggplot() +
      geom_sf(data = map, fill = 'grey', alpha=0.1, color='grey') +
      geom_sf(data= shp, aes(fill=origin_of_data), color=NA) + 
      scale_fill_manual(name = "Source(s) of data", values=c("#9b9b7a", "#e4b074", "#997b66")) + 
      theme_void() +
      theme(legend.text = ggtext::element_markdown(size = 15, family = 'Linux Libertine G'),
            legend.title = ggtext::element_markdown(size = 20, hjust = 0.5, family = 'Linux Libertine G'),
            legend.title.align = 0.5, legend.direction = "horizontal",
            legend.position="bottom",
            plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm")) +
      guides(fill = guide_legend(title.position="top"))
    
  }else if(type=="density"){
    plot <- ggplot() +
      geom_sf(data = map, fill = 'grey', alpha=0.1, color='grey') +
      geom_sf(data= shp, aes(fill=n_occ), color=NA) + 
      scale_fill_gradient(name = "Number of occurrences", low="#e9d8a6", high="#9b2226", limits = c(0,max), guide = "colourbar") + 
      theme_void() +
      guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5)) + 
      theme(legend.title = ggtext::element_markdown(size = 20, hjust = 0.5, family = 'Linux Libertine G'),
            legend.title.align = 0.5,
            text=element_text(size = 15, family="Linux Libertine G"),
            legend.position="bottom", 
            plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm")) 
    
  }else if(type=="filter"){
    plot <- ggplot() +
      geom_sf(data = map, fill = 'grey', alpha=0.1, color='grey') +
      geom_sf(data= shp, fill='darkgrey', alpha=0.6, color=NA) + 
      theme_void() +
      theme(plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"))
    
  }
  
  if(annotate){
    plot <- plot + 
      annotate(geom = "text", x = 2000000, y = 3500000, label = species, color = "darkgrey",
               angle = 90, size=10, fontface="italic", family='Linux Libertine G')
  }
  
  
  
  return(plot)
  
}
