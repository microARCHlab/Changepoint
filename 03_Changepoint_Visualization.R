
# Changepoint Script | Visualizations and Stats on ChangePoint Outputs 
#
# Abigail S. Gancz, 02/22/2024 (Edit by Christine Ta 20250218)
#
# Usage: Create ChangePoint Heatmaps and Excel Tables
#
# This script will not work for R versions higher than 4.3.2 (see README.txt for sessioninfo())
# You do have to change four variables: 
# setwd("PATH to prop from 02_Changepoint.sh"), (see line 196)
# FILE="PATH to prop from 02_Changepoint.sh",  (see line 208)
# DIR="PATH to prop from 02_Changepoint.sh",  (see line 209)
# you will also need to change file names for figure and resulting excel files (see line 278 and 280)
#
############################################################################################################################

# Package Installation

############################################################################################################################

# Function to help with package calling/installation | DO NOT CHANGE
using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}

# Call desired packages, and install if not available 
using("optimx", "LaplacesDemon", "readxl", "tidyr", "readr", "cowplot", "janitor", "qqplotr", "dplyr", "stringr", "tibble", "ggplot2", "forcats", "plotly", "writexl")

############################################################################################################################

# Set up functions

############################################################################################################################

LL.simple <- function(p){
  pHat <- rep(mean(p),length(p))
  res.p <- p-pHat
  return(sum(dnorm(res.p,0,sqrt(sum(res.p^2))/length(res.p),log=T)))
}

AIC.simple <- function(p){
  return(2*2-2*LL.simple(p))
}

Q <- function(X){
  B <- X[1]
  C <- X[2]
  k <- X[3]
  A <- 1
  return(A/(B-C*exp(1)^(-k*t)))
}
Q.f <- function(X){
  B <- X[1]
  C <- X[2]
  k <- X[3]
  A <- 1
  return(sum((p - A/(B-C*exp(1)^(-k*t)))^2))
}
Q.g <- function(X){
  B <- X[1]
  C <- X[2]
  k <- X[3]
  A <- 1
  G1 <- sum(2*(p - A/(B-C*exp(1)^(-k*t)))*(A/(B-C*exp(1)^(-k*t))^2))
  G2 <- sum(2*(p - A/(B-C*exp(1)^(-k*t)))*(A*exp(1)^(-k*t)/(B-C*exp(1)^(-k*t))^2))
  G3 <- sum(2*(p - A/(B-C*exp(1)^(-k*t)))*(A*exp(1)^(-k*t)*C*t/(B-C*exp(1)^(-k*t))^2))
  return(c(G1,G2,G3))
}
Q.t <- function(X){
  B <- X[1]
  C <- X[2]
  k <- X[3]
  A <- 1
  return(-(A*C*exp(1)^(-k*t))/(B-C*exp(1)^(-k*t))^2)
}


damage.fit <- function(positionV,frequencyV,optimx.obj,CI=0.95,TITLE=NA,PLOT=T,printRes=F,ylim=NA,xlim=NA){
  require(qqplotr)
  t <- positionV
  p <- frequencyV
  op <- optimx.obj
  
  theta.hat <- coef(op) %>%
    as_tibble() %>%
    dplyr::select(p1,p2,p3) %>%
    na.omit() %>%
    rowwise() %>%
    dplyr::mutate(Q.f=Q.f(c(p1,p2,p3))) %>%
    dplyr::slice(which.min(Q.f)) %>%
    unlist(., use.names=FALSE) %>%
    head(n=4)
  names(theta.hat) <- c('A','B','k','Q.f')
  fitted.tib <- tibble(position=t,pi=p,fitted=Q(theta.hat)) %>%
    dplyr::mutate(residuals=p-fitted)
  
  z.star <- qt((1-CI)/2,df=length(p)-1,lower.tail=F)
  p.overlap <- p>(1/theta.hat[1]+z.star*sd(fitted.tib$residuals))
  CO <- max(min(t[!p.overlap]),1)#max(min(setdiff(t,which(p.overlap)))-1,0)
  LL.f <- sum(dnorm(fitted.tib$residuals,0,sqrt(sum(fitted.tib$residuals^2))/length(p),log=T))
  # AIC.f <- 2*4-2*LL.f
  LL.lm <- LL.simple(p)
  # AIC.lm <- AIC(tp.lm)
  p.val <- pchisq(2*(LL.f-LL.lm),df=2,lower.tail=F)
  
  if(p.val>0.05){#(AIC.f<AIC.lm){
    fitted.tib$fitted <- mean(fitted.tib$pi)
    fitted.tib$residuals <- fitted.tib$pi- mean(fitted.tib$pi)
    M <- mean(fitted.tib$pi)
    CO <- NA
  }else{
    M <- 1/theta.hat[1]
  }
  
  
  if(printRes){
    s1 <- sprintf('AIC values: F(X): %.2f, lm: %.2f',AIC.f,AIC.lm)
    s2 <- paste0('AIC suggests ',ifelse(AIC.f<AIC.lm,paste0("damage until position ",CO),"no damage"))
    s3 <- sprintf(' (p-value = %.2e).',p.val)
    cat(paste0(s1,"\n",s2,s3))
  }
  
  damage.gg <- ggplot(data=fitted.tib,aes(x=position))+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))+
    #geom_errorbar(aes(ymax=fitted+z.star*sd(fitted.tib$residuals),
    #                  ymin=fitted-z.star*sd(fitted.tib$residuals)),col='blue',alpha=0.5)+
    geom_point(aes(y=p),pch=4,size=4,col='red')+
    geom_point(aes(y=fitted),pch=1,size=2,col='blue',alpha=0.5)+
    geom_line(aes(y=fitted),col='blue',alpha=0.5)+
    geom_hline(yintercept=mean(fitted.tib$pi),linetype='dotted',alpha=0.5)+
    geom_hline(yintercept=M+c(-1,0,1)*z.star*sd(fitted.tib$residuals),col='blue',linetype='dashed')+
    scale_x_continuous(breaks=t)
  if(!is.na(CO)){
    damage.gg <- damage.gg + 
      geom_segment(aes(x=CO,xend=CO,y=M-z.star*sd(fitted.tib$residuals),yend=Inf),
                   col='red',linetype='dashed')
  }
  
  if(!is.na(TITLE)){
    damage.gg <- damage.gg + ggtitle(TITLE)
  }
  if(!any(is.na(xlim)) & any(is.na(ylim))){
    damage.gg <- damage.gg + coord_cartesian(xlim=xlim)
  }
  if(any(is.na(xlim)) & !any(is.na(ylim))){
    damage.gg <- damage.gg + coord_cartesian(ylim=ylim)
  }
  if(!any(is.na(xlim)) & !any(is.na(ylim))){
    damage.gg <- damage.gg + coord_cartesian(xlim=xlim,ylim=ylim)
  }
  
  qq.gg <- ggplot(data=fitted.tib,aes(sample=residuals))+
    theme_bw()+
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())+
    geom_qq_band(bandType = "ks", mapping = aes(fill = "KS"), alpha = 0.5)+
    stat_qq_line() +
    stat_qq_point()
  gg.plot <- ggdraw()+
    draw_plot(damage.gg,x=0,y=0,width=1,height=1)+
    draw_plot(qq.gg,x=0.75,y=0.70,width=0.2,height=0.2)
  if(PLOT){
    print(gg.plot)
  }
  
  damage.list <- list(theta.hat=theta.hat,
                      fitted.tib=fitted.tib,
                      p.val=p.val,trimAt=CO,
                      plot.obj=gg.plot)
  
  return(damage.list)
}

############################################################################################################################

# Run on: China 

############################################################################################################################


#set working directory
setwd("/Users/christineta/Dissertation/Project/AncientResistome/Aim1Dissertation_Analysis_aDNAAuthentication/China_prop")

getFirstN <- function(x){
  return(substr(tolower(x),start=1,stop=8))
}

getSampleID <- function(x){
  return(tail(unlist(strsplit(gsub('_base_freq_R.txt','',x),split='/')),n=1))
}
options(readr.num_columns = 0)
set.seed(12345)

DIR <- dir("/Users/christineta/Dissertation/Project/AncientResistome/Aim1Dissertation_Analysis_aDNAAuthentication/China_prop")
files <- file.path("/Users/christineta/Dissertation/Project/AncientResistome/Aim1Dissertation_Analysis_aDNAAuthentication/China_prop", DIR)
results <- tibble(sample_id=basename(files),end=0,change.point=0,p.val=0,sig='xxx',cc=-1,kkt1=F,kkt2=F,bases='xxx')
counter <- 1

for(fn in files){
  print(round(counter/length(files)*100,2))
  # pos.n <- ifelse(any(c(grepl('6882',fn),grepl('6838',fn),grepl('6852',fn),
  #                     grepl('6859',fn)),grepl('7920',fn)),3,2)
  pos.n <- 2
  end35 <-as.numeric(str_sub(fn,-10,-10))
  
  results.row <- which(results$sample_id==basename(fn))
  curr.tib <- read_tsv(fn) %>%
    dplyr::rename_if(startsWith(names(.),"P"),getFirstN) %>%
    gather(base,pi,A:G) %>%
    janitor::clean_names() %>%
    dplyr::filter(position<=25,position>=pos.n) %>%
    {if (end35==5) dplyr::filter(.,base=='T') else dplyr::filter(., base=='A')}
  # dplyr::group_by(position) %>%
  # dplyr::summarise(pi=sum(count_total)/sum(read_count))
  t <- curr.tib$position
  p <- curr.tib$pi
  startx <- c(1/median(curr.tib$pi),1e-6,1e-6)
  curr.opt <- optimx(startx,fn=Q.f,gr=Q.g,method='bobyqa',lower=c(0,0,0),
                     upper=c(1/min(p),1e6,1e2),control=list(starttests=F,dowarn=F))
  curr.fit <- damage.fit(curr.tib$position,curr.tib$pi,curr.opt,PLOT=F)
  results$end[results.row] <- end35
  results$change.point[results.row] <- curr.fit$trimAt+1
  results$p.val[results.row] <- curr.fit$p.val
  results$cc[results.row] <- curr.opt$convcode
  results$kkt1[results.row] <- curr.opt$kkt1
  results$kkt2[results.row] <- curr.opt$kkt2
  results$bases[results.row] <- paste0(unique(curr.tib$base))
  counter <- counter +1
}

results %<>% 
  dplyr::mutate(id=paste0(lapply(sapply(sample_id,strsplit,split='_'),head,n=1))) %>%
  dplyr::mutate(func=unlist(lapply(lapply(lapply(sapply(sample_id,strsplit,split='_'),head,n=2),tail,n=1),str_c,collapse='_'))) %>%
  rowwise() %>%
  dplyr::mutate(p.val.adj=p.adjust(p.val,method='fdr',n=16)) %>%
  dplyr::mutate(sigM=case_when(p.val.adj>=0.05 ~ 'Non-Sig',
                               p.val.adj<0.05 ~ 'Sig')) %>%
  rowwise() %>%
  dplyr::mutate(sig=case_when(p.val.adj>=0.1 ~ '-',
                              p.val.adj<0.1 & p.val.adj>=0.05 ~ '.',
                              p.val.adj<0.05 & p.val.adj>=0.01 ~ '*',
                              p.val.adj<0.01 & p.val.adj>=0.001 ~ '**',
                              p.val.adj<0.001 ~ '***')) %>%
  dplyr::mutate(change.point=ifelse(is.na(change.point),1,change.point))


heatMap <- results %>%
  #dplyr::filter(end==3)
  ggplot(aes(x=id, y=end, fill=sigM))+
  geom_tile(col='white')+
  geom_text(aes(label=paste0(change.point,'\n',sig)),col='white',size=2)+
  scale_fill_manual(values=c('steelblue','red'),name='')+
  theme_grey(base_size = 9) + labs(x = "",y = "")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_discrete(expand = c(0, 0))+
  scale_y_discrete(expand = c(0, 0)) +
  aes(x = fct_inorder(id))


print(heatMap)

install.packages("svglite")
library(svglite)
ResultOutput<-write_xlsx(list(ChangePoint_ProportionResults=results), path = "CHINA_CHANGEPOINT_RESULTS_20250218.xlsx", col_names = TRUE,format_headers = FALSE ,use_zip64 = FALSE )

ggsave("~/Dissertation/Project/AncientResistome/Aim1Dissertation_Analysis_aDNAAuthentication/Figures/China_ChangePointResults.svg", width=7, height=5)

############################################################################################################################

# Run on: SouthAmerica Huaca Pucllana and Caral

############################################################################################################################

#set working directory
setwd("/Users/christineta/Dissertation/Project/AncientResistome/Aim1Dissertation_Analysis_aDNAAuthentication/SAm_HP_Caral_prop")

getFirstN <- function(x){
  return(substr(tolower(x),start=1,stop=8))
}

getSampleID <- function(x){
  return(tail(unlist(strsplit(gsub('_base_freq_R.txt','',x),split='/')),n=1))
}
options(readr.num_columns = 0)
set.seed(12345)

DIR <- dir("/Users/christineta/Dissertation/Project/AncientResistome/Aim1Dissertation_Analysis_aDNAAuthentication/SAm_HP_Caral_prop")
files <- file.path("/Users/christineta/Dissertation/Project/AncientResistome/Aim1Dissertation_Analysis_aDNAAuthentication/SAm_HP_Caral_prop", DIR)
results <- tibble(sample_id=basename(files),end=0,change.point=0,p.val=0,sig='xxx',cc=-1,kkt1=F,kkt2=F,bases='xxx')
counter <- 1

for(fn in files){
  print(round(counter/length(files)*100,2))
  # pos.n <- ifelse(any(c(grepl('6882',fn),grepl('6838',fn),grepl('6852',fn),
  #                     grepl('6859',fn)),grepl('7920',fn)),3,2)
  pos.n <- 2
  end35 <-as.numeric(str_sub(fn,-10,-10))
  
  results.row <- which(results$sample_id==basename(fn))
  curr.tib <- read_tsv(fn) %>%
    dplyr::rename_if(startsWith(names(.),"P"),getFirstN) %>%
    gather(base,pi,A:G) %>%
    janitor::clean_names() %>%
    dplyr::filter(position<=25,position>=pos.n) %>%
    {if (end35==5) dplyr::filter(.,base=='T') else dplyr::filter(., base=='A')}
  # dplyr::group_by(position) %>%
  # dplyr::summarise(pi=sum(count_total)/sum(read_count))
  t <- curr.tib$position
  p <- curr.tib$pi
  startx <- c(1/median(curr.tib$pi),1e-6,1e-6)
  curr.opt <- optimx(startx,fn=Q.f,gr=Q.g,method='bobyqa',lower=c(0,0,0),
                     upper=c(1/min(p),1e6,1e2),control=list(starttests=F,dowarn=F))
  curr.fit <- damage.fit(curr.tib$position,curr.tib$pi,curr.opt,PLOT=F)
  results$end[results.row] <- end35
  results$change.point[results.row] <- curr.fit$trimAt+1
  results$p.val[results.row] <- curr.fit$p.val
  results$cc[results.row] <- curr.opt$convcode
  results$kkt1[results.row] <- curr.opt$kkt1
  results$kkt2[results.row] <- curr.opt$kkt2
  results$bases[results.row] <- paste0(unique(curr.tib$base))
  counter <- counter +1
}

results %<>% 
  dplyr::mutate(id=paste0(lapply(sapply(sample_id,strsplit,split='_'),head,n=1))) %>%
  dplyr::mutate(func=unlist(lapply(lapply(lapply(sapply(sample_id,strsplit,split='_'),head,n=2),tail,n=1),str_c,collapse='_'))) %>%
  rowwise() %>%
  dplyr::mutate(p.val.adj=p.adjust(p.val,method='fdr',n=16)) %>%
  dplyr::mutate(sigM=case_when(p.val.adj>=0.05 ~ 'Non-Sig',
                               p.val.adj<0.05 ~ 'Sig')) %>%
  rowwise() %>%
  dplyr::mutate(sig=case_when(p.val.adj>=0.1 ~ '-',
                              p.val.adj<0.1 & p.val.adj>=0.05 ~ '.',
                              p.val.adj<0.05 & p.val.adj>=0.01 ~ '*',
                              p.val.adj<0.01 & p.val.adj>=0.001 ~ '**',
                              p.val.adj<0.001 ~ '***')) %>%
  dplyr::mutate(change.point=ifelse(is.na(change.point),1,change.point))


heatMap <- results %>%
  #dplyr::filter(end==3)
  ggplot(aes(x=id, y=end, fill=sigM))+
  geom_tile(col='white')+
  geom_text(aes(label=paste0(change.point,'\n',sig)),col='white',size=2)+
  scale_fill_manual(values=c('steelblue','red'),name='')+
  theme_grey(base_size = 9) + labs(x = "",y = "")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_discrete(expand = c(0, 0))+
  scale_y_discrete(expand = c(0, 0)) +
  aes(x = fct_inorder(id))


print(heatMap)

ResultOutput<-write_xlsx(list(ChangePoint_ProportionResults=results), path = "SAmerica_HuacaPucllanaANDCaral_CHANGEPOINT_RESULTS_20250218.xlsx", col_names = TRUE,format_headers = FALSE ,use_zip64 = FALSE )
ggsave("~/Dissertation/Project/AncientResistome/Aim1Dissertation_Analysis_aDNAAuthentication/Figures/SAmerica_HuacaPucllana_and_Caral_Changepoint_Results.svg", width=7, height=5)
