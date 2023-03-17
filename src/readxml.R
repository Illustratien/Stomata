# install.packages('XML')
# install.packages('purrr')
pacman::p_load(dplyr,ggplot2)
flist <- list.files('data/detected pictures')

# -------------------------------------------------------------------------

classfun <-function(vec){
  # first rough classification
  hist.res <- hist(vec,plot=F)
  # cut(vec,c(0,hist.res$mids[which(hist.res$counts>0)],5000)) %>% as.numeric()
  cut(vec,length(which(hist.res$counts>0))) %>% as.numeric()
} 
# rotation should not be based on the labeled data, but the slope
rad2deg <- function(rad){
  (180-((rad * 180) / (pi)))*-1
}
angle_check<- function(df){
  if(any(df$robndbox.angle>0)){
    rad <- df$robndbox.angle
    rad <- rad %>% median() #[!ang==0]
    newxy<- df%>% 
      select(robndbox.cx,robndbox.cy) %>%
      xyrotate(.,rad2deg(rad))
    df$robndbox.cx <- newxy$robndbox.cx
    df$robndbox.cy <- newxy$robndbox.cy
  }
  return(df)
}

xyrotate<- function(xy,angle){
  # make sure xyz is a dataframe with column order x,y,z
  # turn based on the z axis
  # https://www.mathworks.com/help/phased/ref/rotz.html?searchHighlight=rotz&s_tid=srchtitle_rotz_1
  rotz <- matrix(c(cos(angle),-sin(angle),
                   sin(angle),cos(angle)),
                 byrow = T,
                 nrow=2)
  
  new.df <- rotz%*%t(xy)%>%t()%>%data.frame()
  names(new.df) <- c("robndbox.cx","robndbox.cy")
  return(new.df)
}
row_check <- function(df){
  # double check class result from hist
  sumdf <- df %>% group_by(rowclass) %>% 
    summarise(s=sd(robndbox.cy),
              m=median(robndbox.cy),
              r=diff(range(robndbox.cy))) 
  sd.thresh <- 20
  # extreme standard deviation 
  ckdf <-sumdf%>% filter(s>sd.thresh|is.na(s))# sd =na when only one value in the class
  if(nrow(ckdf)>0){
    for (i in 1:nrow(ckdf)){
      # find class with large sd
      ori.class <- ckdf[i,]$"rowclass"
      # find y with large sd
      ori.y <- df[df$rowclass==ori.class,]$"robndbox.cy"
      
      if (is.na(ckdf[i,]$s)){ 
        # when only one value per class
        out.y <- ori.y
        # calculate distance to other classes
        distdf <- sumdf %>% 
          mutate(dist=abs(m-mean(out.y))) %>% 
          filter(!is.na(s)) %>% 
          # find the nearest one
          filter(dist==min(dist)) 
        # check the nearest class is within the range of threshold
        if(distdf$s<100){
          new.class <- distdf%>% .$rowclass
        } else{
          warning(paste("canot reclass based on sd.thresh",sd.thresh))
        }
        df <- df %>%
          mutate(rowclass=case_when(robndbox.cy%in%out.y~new.class,
                                    T~rowclass) )
      }else if( ckdf[i,]$"s"<100){
        # in case of just need to reclassify into two classes
        # give a new class that will not overlap with other situation
        cnumber <- diff(range(ori.y))%/%min(df$robndbox.h)+1
        if(cnumber>1){
          re.class <- cut(ori.y,cnumber) %>% as.numeric()+30+i*3
          # look uptable to merge
          ltable <- data.frame(robndbox.cy=ori.y,newclass=re.class)
          # replace old class with new class
          df <- df %>%
            left_join(.,ltable,"robndbox.cy") %>% 
            mutate(rowclass=case_when(!is.na(newclass)~newclass,
                                      T~rowclass) ) %>% 
            select(-newclass)
        }
        
      } else{
        warning(paste("check data",df$pic[1]))
      }
    }
    
  }
  # find classes that are too close
  # merge them into one
  sumdf2 <- df %>% group_by(rowclass) %>% 
    summarise(s=sd(robndbox.cy),
              m2=median(robndbox.cy),
              m=median(robndbox.cy)) %>%
    arrange(m) 
  ind <- which(diff(sumdf2$m)<20)
  for ( j in ind){
    class.vec <- sumdf2[c(j,j+1),]$rowclass
    df <- df %>%
      mutate(rowclass=case_when(rowclass%in%class.vec~class.vec[1],
                                T~rowclass) )
  }
  return(df)
}
xmlread<- function(filename){
  # read xml file and turn to dataframe
  tmp <- XML::xmlToList(filename) 
  res <- tmp%>% 
    .[grepl("object",names(.))] %>% 
    purrr::map_dfr(.,~{data.frame(t(unlist(.x)))}) %>% 
    mutate(across(robndbox.cx:robndbox.angle,as.numeric),
           pic=tmp$filename,
           width=tmp$size %>% .$width,
           height=tmp$size %>% .$height %>% as.numeric())
  # %>% 
    # angle_check(.)
  clas <- classfun(res$robndbox.cy) 
  # rowclass check function 
  res <- res %>% 
    mutate(rowclass=clas) %>% 
    row_check(.) %>%
    row_check(.) %>%
    group_by(rowclass) %>%
    mutate(rowclass=mean(robndbox.cy)) %>% 
    ungroup() %>% 
    mutate( rowclass=rowclass%>% as.factor()%>% 
              as.numeric() %>% as.character(),
            display.y=height-robndbox.cy) 
}
xmlread2<- function(filename){
  # read xml file and turn to dataframe
  tmp <- XML::xmlToList(filename) 
  res <- tmp%>% 
    .[grepl("object",names(.))] %>% 
    purrr::map_dfr(.,~{data.frame(t(unlist(.x)))}) %>% 
    mutate(across(robndbox.cx:robndbox.angle,as.numeric),
           pic=tmp$filename,
           width=tmp$size %>% .$width,
           height=tmp$size %>% .$height) 
  # %>% 
    # angle_check(.)
  clas <- classfun(res$robndbox.cy) 
  # rowclass check function 
  res <- res %>% mutate(rowclass=clas) %>% 
    # row_check(.) %>%
    group_by(rowclass) %>%
    mutate(rowclass=mean(robndbox.cy)) %>% 
    ungroup() %>% 
    mutate( rowclass=rowclass%>% as.factor()%>% as.numeric() %>% as.character(),
            display.y=as.numeric(height)-robndbox.cy) 
}
# -------------------------------------------------------------------------
# filename <- paste0('data/detected pictures/',flist[14])
for (i in c(29,26,23,22,21,20,18,15,9,7,3)){
  print(i)
  f <- xmlread(paste0('data/detected pictures/',flist[i]))
  
  p <- f%>% 
    ggplot(aes(robndbox.cx,display.y,color=rowclass))+
    geom_point()+
    theme_bw()+ggtitle(paste("a",i))+
    scale_x_continuous(c(0,f[["width"]][1]))+
    scale_y_continuous(c(0,f[["height"]][1]))
  print(p)
  
}
# 29 26 23 22 21 20 18 15 9 7 3
# 12
# res %>%   ggplot(aes(robndbox.cx,display.y,color=factor(rowclass)))+geom_point()+
#   theme_bw()
xy <- data.frame(robndbox.cx=c(500,600),
                 display.y=c(1880,253))
xy %>% ggplot(aes(robndbox.cx,display.y))+geom_point()+
  scale_x_continuous(500,1)
a %>% select(robndbox.cx,display.y) %>%
  xyrotate(.,max(a$robndbox.angle)) %>% 
  ggplot(aes(robndbox.cx,display.y))+geom_point()

# next step-------------------------------------------------------------------------
library(dplyr)
library(broom)
library(tidyr)
library(purrr)
map_dfr(1,function(i){
  # https://stackoverflow.com/questions/22713325/fitting-several-regression-models-with-dplyr
  df <- xmlread(paste0('data/detected pictures/',flist[i])) 
  sloptable <- df%>%
    group_by(rowclass) %>%
    summarise(mod = broom::tidy(lm(display.y ~ robndbox.cx , data = .)))%>%
    unnest(cols = c(mod)) %>% 
    filter(term=="robndbox.cx") %>%
    select(rowclass,estimate) %>% 
    ungroup()
  df <- df %>% left_join(sloptable)
  
  
  
}) 
dist2d <- function(slope,from,to) {
  v1 <- c(1,slope) # slope of target line 
  v2 <- a2 - b2 # slope of intersect
  m <- cbind(v1,v2)
  d <- abs(det(m))/sqrt(sum(v1*v1))
} 


dist2d <- function(a,b,c) {
  v1 <- b2 - c2 # slope of target line 
  v2 <- a2 - b2 # slope of intersect
  m <- cbind(v1,v2)
  d <- abs(det(m))/sqrt(sum(v1*v1))
} 

## two-dimensional case:
a2 <- c(0,2)
b2 <- c(2,0)
c2 <- c(1,3)
d2 <- dist2d(a2,b2,c2)
# row distance
# classify the row group 
# calculate slope and intercept for each group
# calculate distance between each line 

# average distance between row 

# consider angle data in the calculation??

