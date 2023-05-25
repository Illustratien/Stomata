# raw_data read -----------------------------------------------------------

xmlread <- function(filename,thr){
  tmp<- XML::xmlToList(filename)
  df <- tmp%>% 
    .[grepl("object",names(.))] %>% 
    purrr::map_dfr(.,~{data.frame(t(unlist(.x)))}) %>% 
    mutate(across(robndbox.cx:robndbox.angle,as.numeric),
           pic=tmp$filename,
           width=tmp$size %>% .$width,
           height=tmp$size %>% .$height,
           display.y=as.numeric(height)-robndbox.cy) %>% 
    add_row_class(.,thr)
}

add_row_class <- function(df,thr){
  # chat GPT
  dist_matrix <- dist(df$robndbox.cy)
  # Apply hierarchical clustering to the distance matrix
  hc <- hclust(dist_matrix)
  # Cut the dendrogram at a suitable height to obtain clusters
  # The 'h' parameter determines the height at which to cut the dendrogram
  df %>% 
    mutate(rowclass=cutree(hc, h = thr) %>% factor())
  
}

plot_fun <- function(df){
  # check the stomata
  df %>% 
    ggplot(aes(stomata.cx,display.y,color=stomata.row))+
    geom_point(aes(shape=stomata.type),size=3)+
    theme_bw()+ggtitle(df[["pic_name"]][1])+
    scale_x_continuous("x",limits = c(0,df[["pic_width"]][1]))+
    scale_y_continuous("y",limits = c(0,df[["pic_height"]][1]))+
    ggforce::geom_ellipse(data=df %>% filter(!grepl("incomplete",stomata.type)),
                          mapping=aes(x0 = stomata.cx, y0 = display.y,
                                      a = stomata.w/2, b = stomata.h/2, 
                                      angle = stomata.angle %>% map_dbl(.,~{
                                        rad2trans(.x)})),
                          show.legend =F)+
    coord_fixed()
}

ck_pic <- function(pic.name,df){
  # wraper of plot_fun
  subdf <- df %>% dplyr::filter(pic_name==pic.name)
  plot_fun(subdf) %>% print()
}


rad2trans <- function(rad){
  # angle transformation from rad to angle.
  if(rad>(pi/2)){
    rad <- pi-rad
  }
  return(rad)
}

# simple statistics-------------------------------------------------------------------------
Disperse_1D<- function(gdf,tar,row=NULL){
  # one dimensional disperse
  if (is.null(row)){
    row=tar
  }
  # wrapper
  gdf %>%
    dplyr::summarise(across({{tar}},
                            list(min=min,
                                 max=max,
                                 count=length,
                                 var=var,
                                 sd=sd,
                                 iqr=IQR,
                                 skewness=moments::skewness,
                                 kurtosis=e1071::kurtosis),
                            .names = paste0(row,"_{.fn}")))
}

complete_count <- function(df){
  # select the complete stomata at each picture and calculate the geometry
  # g could also be pic c("pic_name","stomata.type")
  # here blurry.complete and complete are treated the same.
  
  g <- c("pic_name") %>% rlang::syms()
  # should only the complete be considered?
  # dplyr::filter(stomata_type=='complete') %>% 
  df %>% 
    dplyr::filter(grepl("\\b\\.?complete\\b",stomata.type)) %>% 
    group_by(!!!g) %>% 
    dplyr::summarise(across(stomata.w:stomata.h,list(mean=mean,sd=sd)),
                     stomata.complete_count=n())
  # %>% 
  # tidyr::pivot_longer(-c(!!!g),names_to = 'trait',values_to = 'Trait')
}

row_dispersion <- function(df){
  # for each row, calculate the dispersion of points
  df %>% 
    dplyr::filter(stomata.row.count>1) %>% 
    group_by(pic_name,stomata.row) %>% 
    Disperse_1D(.,"stomata.cx","row")
  
  
}

weight_statistic <- function(df){
  # weighted statistic for complete 1, others 0.5
  df %>% 
    group_by(pic_name) %>% 
    mutate(stomata.Nr.weight=case_when(grepl("\\b\\.?complete\\b",stomata.type)~1,
                                       T~.5),
           area=pi*(stomata.w/2)*(stomata.h/2),
           pic_area=(pic_width*pic_height)
    ) %>% 
    summarise(effective.stomatal.count=sum(stomata.Nr.weight),
              total.stomata.area=sum(stomata.Nr.weight*area) ,
              ratio.stomata.area=total.stomata.area/.data[["pic_area"]][1])
  
}

fun_ls<- list(row_dispersion,weight_statistic,complete_count)

simple_stat <- function(df){
  # wrapper of functions
  lapply(fun_ls,function(f,...) f(...),df) %>% Reduce(merge,.)
}

# slope distance -------------------------------------------------------------------------

row_dist_fun<- function(slope.df){
  # calculate sequential row distance based on intercept
  # based on ascending order
  
  row.dist<- slope.df %>%
    arrange(intercept) %>% .$intercept %>% 
    diff(.)
  
  vec <- slope.df %>%
    arrange(desc(intercept)) %>%
    .$stomata.row %>% 
    as.character()
  
  row.combi<- map2_chr(vec[1:(length(vec)-1)],
                       vec[2:length(vec)],
                       ~{paste(.x,.y,sep="_")})
  data.frame(pic_name=slope.df$pic_name[1],
             row_dist=row.dist,
             row_combi=row.combi
  )
  
}

