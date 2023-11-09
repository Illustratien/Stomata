shp <-c(5,9,1,8,10)
names(shp) <- c("blurry.complete","blurry.incomplete","complete","hair","incomplete")

plotfun <- function(df,disthresh=30){
  # disthresh : threshold value to judge whether it belongs to same point or not
  ndf <- df%>%filter(type=="ntu")
  gdf <-   df%>%filter(type=="truth")
  if(nrow(gdf)==0){
    stop(sprintf("no ground truth found for  %s!",df$pic_name[1]))
  }
  # match each point to ground truth
  ntd<- map_dfr(1:nrow(ndf),~{
    # for each point in estimated pipeline
    # bind witn ground truth and calculate the distance
    d <- bind_rows(ndf[.x,],
                   gdf) %>% 
      dplyr::select(stomata.cx,stomata.cy) %>% 
      stats::dist() %>%
      as.matrix(diag = TRUE, upper = TRUE) 
    # focus on distance not larger than 20 (subset distance matrix)
    condi <- setdiff(which(d[1,]<disthresh),1)-1
    
    ndf[.x,] %>% 
      mutate(
        # row index of ground truth that falls within this threshold
        gid=  ifelse(length(condi)==0,NA,condi),
        # ground truth x
        truth.cx=ifelse(!is.na(gid),gdf[gid,]$"stomata.cx",NA),
        # ground truth y
        truth.cy=ifelse(!is.na(gid),gdf[gid,]$"stomata.cy",NA)
      )
  })%>%
    mutate(display.y=1944-stomata.cy) 
  
  # check how many points share the same ground truth 
  repeateddf <- ntd %>%
    na.omit() %>% 
    group_by(gid) %>%
    summarise(n=n()-1) %>% 
    filter(n>0)
  
  ntd <- rbind(ntd %>%
                 filter(!is.na(gid)) %>% 
                 group_by(gid) %>%
                 filter(confidence==max(confidence)),
               ntd %>%
                 filter(is.na(gid))) %>% ungroup()
  
  # repn <-repeateddf%>% nrow()
  unmd <- nrow(gdf)-ntd$gid %>% na.omit()%>% unique() %>% length()
  # -------------------------------------------------------------------------
  SummaryTable <- bind_rows(gdf,ntd) %>% 
    group_by(type) %>% mutate(totaln=n()) %>% 
    group_by(type,class) %>% mutate(indivin=n()) %>% 
    select(type,class,totaln,indivin) %>% distinct()
  briefdf <- SummaryTable %>%.[,-c(2,4)] %>% distinct()
  detaildf <- SummaryTable %>% .[,-3] %>%
    tidyr::pivot_wider(names_from = type,
                       values_from = indivin) %>% 
    mutate(ratio=(ntu/truth) %>%
             round(.,digits = 2))
  
  # -------------------------------------------------------------------------
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
                           # "% \nestimated repeated coordinates = ",repn,
                           "\nmiss match = ",ntd %>%filter(is.na(gid)) %>% nrow(),
                           "                , new match= ",unmd),
         # caption = paste0("dist threshold",disthresh)
    )+
    
    # facet_grid(~pic_name)+
    #Label new
    ggrepel::geom_text_repel(data=ntd %>% 
                               filter(is.na(gid)),
                             mapping=aes(x = stomata.cx, y = display.y,
                                         label=class),
                             point.padding = 1,box.padding = .8,
                             show.legend =F)+
    
    coord_fixed()+
    theme(axis.title = element_blank(),
          # axis.text = element_blank()
    )
  
  # plotdf
  
  # table
  tt <- gridExtra::ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
  
  tbl <- gridExtra::tableGrob(detaildf %>% 
                                mutate(ratio=toolPhD::round_scale(ratio)),
                              rows=NULL, theme=tt)
  tbl2 <- gridExtra::tableGrob(briefdf %>% t(), rows=NULL, theme=tt)
  
  tb <- gridExtra::arrangeGrob(tbl2,tbl,nrow=2,
                               # as.table=TRUE,
                               heights=c(3,3))
  # Plot chart and table into one object
  # if(nrow(repdf)>0){
  #   tbl3 <- gridExtra::tableGrob(repdf %>%
  #                                  mutate(dist=toolPhD::round_scale(dist)), 
  #                                rows=NULL, theme=tt)
  #   plotdf <- plotdf+
  #     ggrepel::geom_text_repel(data=ndf[repdf$from,],
  #                              mapping=aes(x = stomata.cx, y = display.y,
  #                              ), label="R",color="darkred",
  #                              point.padding = 1,box.padding = .8,
  #                              show.legend =F)
  #   # Plot chart and table into one object
  #   p <-  cowplot::plot_grid(plotdf, tb,tbl3,
  #                            nrow=1,rel_widths = c(5,1.9,1))
  # }else{
  p <-  cowplot::plot_grid(plotdf, tb,
                           nrow=1,rel_widths = c(5,1.9))
  # }
  
  
  # print(p)
  
  resdf <- data.frame(pic_name=df$pic_name[1],
                      detect=briefdf[2,]$totaln,
                      ground=briefdf[1,]$totaln,
                      mismatch = ntd %>%filter(is.na(gid)) %>% nrow(),
                      unmatch = unmd)
  ntd <- ntd %>% ungroup() %>% dplyr::select(-c(id,gid,type,display.y))
  return(list(resdf,p,ntd))
}
