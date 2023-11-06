plotfun <- function(df,disthresh=6.5){
  # disthresh : threshold value to judge whether it belongs to same point or not
  ndf <- df%>%filter(type=="ntu")
  gdf <-   df%>%filter(type=="truth")
  if(nrow(gdf)==0){
    stop(sprintf("no ground truth found for  %s!",df$pic_name[1]))
  }
  
  shp <-c(5,9,1,8,10)
  names(shp) <- c("blurry.complete","blurry.incomplete","complete","hair","incomplete")
  
  ntd<- map_dfr(1:nrow(ndf),~{
    d <- bind_rows(ndf[.x,],
                   gdf) %>% 
      dplyr::select(stomata.cx,stomata.cy) %>% 
      stats::dist() %>%
      as.matrix(diag = TRUE, upper = TRUE) 
    # focus on distance not larger than 20 (subset distance matrix)
    condi <- setdiff(which(d[1,]<20),1)
    
    ndf[.x,] %>% 
      mutate(gid=  ifelse(length(condi)==0,NA,condi)) %>% 
      mutate(        gx=ifelse(!is.na(gid),gdf[gid,"stomata.cx"],NA),
                     gy=ifelse(!is.na(gid),gdf[gid,"stomata.cy"],NA)
      )
  })%>% mutate(display.y=1944-stomata.cy) 
  
  SummaryTable <- df %>% 
    group_by(type) %>% mutate(totaln=n()) %>% 
    group_by(type,class) %>% mutate(indivin=n()) %>% 
    select(type,class,totaln,indivin) %>% distinct()
  briefdf <- SummaryTable %>%.[,-c(2,4)] %>% distinct()
  detaildf <- SummaryTable %>% .[,-3] %>%
    tidyr::pivot_wider(names_from = type,
                       values_from = indivin) %>% 
    mutate(ratio=(ntu/truth) %>%
             round(.,digits = 2))
  tt <- gridExtra::ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
  
  tbl <- gridExtra::tableGrob(detaildf %>% 
                                mutate(ratio=toolPhD::round_scale(ratio)),
                   rows=NULL, theme=tt)
  tbl2 <- gridExtra::tableGrob(briefdf %>% t(), rows=NULL, theme=tt)
  
  tb <- gridExtra::arrangeGrob(tbl2,tbl,nrow=2,
                    # as.table=TRUE,
                    heights=c(3,3))
  # calculate distance of each points in the new detected df
  d <- ndf %>% 
    dplyr::select(stomata.cx,stomata.cy) %>% 
    stats::dist() %>%
    as.matrix(diag = TRUE, upper = TRUE) 
  d[lower.tri(d,diag = T)] <- NA
  #generate from-to index 
  mat <- as.data.frame(t(combn(dim(ndf)[1],2))) 
  colnames(mat) <- c('from','to')
  mat <- mat %>% mutate(
    # transform the upper triangle into linear by row
    dist=na.omit(as.vector(t(d))))
  repdf <- mat %>% group_by(from) %>%
    dplyr::filter(dist==min(dist,na.rm = T)) %>% filter(dist<disthresh) 


  repn <-repdf%>% nrow()
  unmd <- nrow(gdf)-ntd$gid %>% na.omit()%>% unique() %>% length()
  
  plotdf <- df%>% 
    ggplot(aes(stomata.cx,display.y))+
    geom_point(data=ntd %>% 
                 filter(is.na(gid)),
               mapping=aes(x = stomata.cx, y = display.y,shape=class),
               show.legend = F,size=3,
               color="black",stroke=2.5,alpha=.5)+
    geom_point(aes(shape=class,color=type),size=3,stroke=1.5,alpha=.5)+
    scale_shape_manual(values=1:5)+
    theme_bw()+
    ggtitle(df$pic_name[1])+
    labs(subtitle = paste0("detect/truth = ",
                   round(briefdf[2,2]/briefdf[1,2],digit=2),
                   "% \nestimated repeated coordinates = ",repn,
                   "\nmismatch = ",ntd %>%filter(is.na(gid)) %>% nrow(),
                   "                , unmatch = ",unmd),
         caption = paste0("dist threshold",disthresh))+

    # facet_grid(~pic_name)+
    ggrepel::geom_text_repel(data=ntd %>% 
                               filter(is.na(gid)),
                             mapping=aes(x = stomata.cx, y = display.y,
                                         label=class),
                             point.padding = 1,box.padding = .8,
                             show.legend =F)+
    
    coord_fixed()+
    theme(axis.title = element_blank(),
          axis.text = element_blank())
  

  # plotdf

  
  
  # Plot chart and table into one object
  if(nrow(repdf)>0){
    tbl3 <- gridExtra::tableGrob(repdf %>%
                                   mutate(dist=toolPhD::round_scale(dist)), 
                                 rows=NULL, theme=tt)
    plotdf <- plotdf+
      ggrepel::geom_text_repel(data=ndf[repdf$from,],
                               mapping=aes(x = stomata.cx, y = display.y,
                                          ), label="R",color="darkred",
                               point.padding = 1,box.padding = .8,
                               show.legend =F)
    # Plot chart and table into one object
    p <-  cowplot::plot_grid(plotdf, tb,tbl3,
                             nrow=1,rel_widths = c(5,1.9,1))
  }else{
    p <-  cowplot::plot_grid(plotdf, tb,
                             nrow=1,rel_widths = c(5,1.9))
  }


  # print(p)
  
  resdf <- data.frame(pic_name=df$pic_name[1],
                      detect=briefdf[2,]$totaln,
                      ground=briefdf[1,]$totaln,
                      rep_cor=repn,
                      mismatch = ntd %>%filter(is.na(gid)) %>% nrow(),
                      unmatch = unmd)
  return(list(resdf,p))
}