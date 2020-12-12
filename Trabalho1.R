library(readxl)
library(dplyr)
library(ggplot2)
library(moments)
library(EnvStats)
getmode <- function(v) {
  uniqv <- unique(v) 
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
descritiva=function(x){
  y=c(summary(x),
      var=var(x),
      sd=sd(x),
      cv=cv(x),
      moda=getmode(x))
  data.frame(t(y))
}

library(readxl)
#dados1<- read_excel("Dados_trabalho_20201_.xlsx")
#set.seed(566)
#dados=dados1[sample(1:nrow(dados1),57,F),]
#saveRDS(dados,"dados.rds")

dados=readRDS("dados.rds")

dados$X8=factor(dados$X8)
levels(dados$X8)=c("NE", "NC", "S", "W")
dados$X7=factor(dados$X7)
dados <- rename(dados, Y = X10)
levels(dados$X7)=c("Sim","Não")
dados$X11=dados$X11*35/100

###cor(dados)#problema com multicolinealidade
library(GGally)
ggcorr(dados[,-c(7,8)], method = c("everything", "pearson"))+
  theme_light()



ggpairs(dados[,-c(7,8)], 
        lower = list(continuous = wrap("smooth", 
                                       alpha = 0.6, 
                                       size=0.1)), 
        diag = list(continuous = wrap("densityDiag", 
                                      alpha=0.5, fill = "#80d956")), 
        upper = list(continuous = wrap("cor", size=4, alignPercent=0.8))) + 
  theme(panel.grid.major = element_blank())+
  theme_light()


################################### Análise descritiva #####################################


######## Qualitativas


### X7


### Gráfico de barra
x7 <- dados %>% select(X7) %>%
  table() %>% data.frame() %>% 
  mutate(freq.rel = round((Freq/sum(Freq))*100, digits = 2)) %>% 
  rename(var =1)
ggplot(x7,aes(x = var, y = Freq))+
  geom_col(fill=c("#006ff4","#a25d5d"), 
           width = 0.5, position = position_dodge(width = .5))+
  labs(x="Filiação a Escola de Medicina",y="Proporção", 
       title = "Proporção de hosiptais filiados a escola de medicina")+
  geom_label(aes(label = paste0(format(freq.rel, decimal.mark = ","), "%")),
             nudge_y = 5)+
  theme_light()+
  theme(legend.position = "bottom", 
        plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))



### Tabela de frequência (absoluta e relativa)



x7$cumsum <- cumsum(x7$Freq)
x7$cumsum_freq <- cumsum(x7$freq.rel)
x7 <- x7%>% 
  rename(`Frequência absoluta` = 2 , 
         `Frequência relativa (%)` = 3, 
         `Frequência acumulada`= 4,
         `Frequência relativa acumulada (%)` = 5, 
         ` ` = 1)



library(kableExtra)
kable(x7)


##Teste de Homogeniedade
chisq.test(dados$Y,dados$X7)# as variancia são iguais para o nº de enfermeiro que possuem ou não
#Filiação a Escola de Medicina 


### X8
### Gráfico de barra


x8 <- dados %>% select(X8) %>%
  table() %>% data.frame() %>% 
  mutate(freq.rel = round((Freq/sum(Freq))*100, digits = 2)) %>% 
  rename(var =1)
ggplot(x8,aes(x = var, y = Freq))+
  geom_col(fill=c("#006ff4","#a25d5d","#80d956", "#f44300"), 
           width = 0.5, position = position_dodge(width = .5))+
  labs(x="Filiação a Escola de Medicina",y="Proporção", 
       title = "Localização dos hospitais")+
  geom_label(aes(label = paste0(format(freq.rel, decimal.mark = ","), "%")),
             nudge_y = 3)+
  theme_light()+
  theme(legend.position = "bottom", 
        plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))



### Tabela de frequência (absoluta e relativa)
x8$cumsum <- cumsum(x8$Freq)
x8$cumsum_freq <- cumsum(x8$freq.rel)
x8 <- x8%>% 
  rename(`Frequência absoluta` = 2 , 
         `Frequência relativa (%)` = 3, 
         `Frequência acumulada`= 4,
         `Frequência relativa acumulada (%)` = 5, 
         ` ` = 1)



library(kableExtra)
kable(x8)

### Homegenidade

chisq.test(dados$Y,dados$X8)# as variancia são iguais para o nº de enfermeiro entre as regioes


####### Quantitativa



##### Discreta X6, X9 E X10



### X6
#Histograma



ggplot(dados, aes(x= X6))+
  geom_histogram(colour="black", fill="#80d956")+
  labs(x = "Número de leitos" ,
       title = "Histograma de Número de leitos",
       y = "Densidade")+
  theme_light()+
  theme(legend.position = "bottom",
        plot.subtitle = element_text(color = 'gray',
                                     size = 10,
                                     face = 'bold'))
#Box-plot



ggplot(dados, aes(y = " ",x= X6))+
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=F,
               fill="#80d956", alpha = .8,color="black")+
  stat_summary(fun.x=mean, geom="point", shape=1, size=2)+
  labs(x = "Número de Leitos" , title = "Boxplot de Número de leitos",
       y = "")+
  theme_light()+
  theme(legend.position = "bottom",
        plot.subtitle = element_text(color = 'gray', size = 10,
                                     face = 'bold'))



#Medidas descritivas

descritiva(dados$X6)


## Tabela de intervalos

br = seq(0,900,by=100)
ranges = paste(head(br,-1), br[-1], sep=" - ")
freq   = hist(dados$X6, breaks=br, include.lowest=TRUE, plot=FALSE)
data.frame(`Intervalo` = ranges,
           `Frequência` = freq$counts,
           `Frequência relativa`=round(freq$counts/sum(freq$counts),3),
           `Frequência Acumulada`=cumsum(freq$counts),
           `Frequência relativa acumulada`=round(cumsum(freq$counts)/sum(freq$counts),3))




### X9
#Histograma



hist(dados$X9, main="Histograma de Média diária de pacientes",
     prob=F, xlab="Média diária de pacientes", ylab="Frequência",
     col=c("#80d956"), labels=TRUE)




ggplot(dados, aes(x= X9))+
  geom_histogram(colour="black", fill="#80d956")+
  labs(x = "Média diária de pacientes" ,
       title = "Histograma de Média diária de pacientes",
       y = "Densidade")+
  theme_light()+
  theme(legend.position = "bottom",
        plot.subtitle = element_text(color = 'gray',
                                     size = 10,
                                     face = 'bold'))
#Box-plot



ggplot(dados, aes(y = " ",x= X9))+
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=F,
               fill="#80d956", alpha = .8,color="black")+
  stat_summary(fun.x=mean, geom="point", shape=1, size=2)+
  labs(x = "Média diária de pacientes" , title = "Boxplot de Média diária de pacientes",
       y = "")+
  theme_light()+
  theme(legend.position = "bottom",
        plot.subtitle = element_text(color = 'gray', size = 10,
                                     face = 'bold'))
#Medidas descritivas

descritiva(dados$X9)


## Tabela de intervalos

br = seq(0,600,by=100)
ranges = paste(head(br,-1), br[-1], sep=" - ")
freq   = hist(dados$X9, breaks=br, include.lowest=TRUE, plot=FALSE)
data.frame("Intervalo" = ranges,
           "Frequência" = freq$counts,
           "Frequência relativa"=round(freq$counts/sum(freq$counts),3),
           "Frequência Acumulada"=cumsum(freq$counts),
           "Frequência relativa acumulada"=round(cumsum(freq$counts)/sum(freq$counts),3))



### X10
#Histograma



ggplot(dados, aes(x= Y))+
  geom_histogram( colour="black", fill="#80d956")+
  labs(x = "Número de enfermeiros" ,
       title = "Histograma de Número de enfermeiros",
       y = "Densidade")+
  theme_light()+
  theme(legend.position = "bottom",
        plot.subtitle = element_text(color = 'gray',
                                     size = 10,
                                     face = 'bold'))
#Box-plot
ggplot(dados, aes(y = " ",x= Y))+
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=F,
               fill="#80d956", alpha = .8,color="black")+
  stat_summary(fun.x=mean, geom="point", shape=1, size=2)+
  labs(x = "Número de enfermeiros" , title = "Boxplot de Número de enfermeiros",
       y = "")+
  theme_light()+
  theme(legend.position = "bottom",
        plot.subtitle = element_text(color = 'gray', size = 10,
                                     face = 'bold'))
#Medidas descritivas

descritiva(dados$Y)


## Tabela de intervalos

br = seq(0,700,by=100)
ranges = paste(head(br,-1), br[-1], sep=" - ")
freq   = hist(dados$Y, breaks=br, include.lowest=TRUE, plot=FALSE)
data.frame("Intervalo" = ranges,
           "Frequência" = freq$counts,
           "Frequência relativa"=round(freq$counts/sum(freq$counts),3),
           "Frequência Acumulada"=cumsum(freq$counts),
           "Frequência relativa acumulada"=round(cumsum(freq$counts)/sum(freq$counts),3))



##### Contínua X1, X2, X3, X4, X5 e X11


###X1
##Densidade 

##Boxplot 

##Tabela de Intervalos 
br = seq(5,20,by=5)
ranges = paste(head(br,-1), br[-1], sep=" - ")
freq   = hist(dados$X1, breaks=br, include.lowest=TRUE, plot=FALSE)
data.frame("Intervalo" = ranges,
           "Frequência" = freq$counts,
           "Frequência relativa"=round(freq$counts/sum(freq$counts),3),
           "Frequência Acumulada"=cumsum(freq$counts),
           "Frequência relativa acumulada"=round(cumsum(freq$counts)/sum(freq$counts),3))



##Medidas descritivas
descritiva(dados$X1)[1:9]


##Teste de normalidade 
shapiro.test(dados$X1)

###X2
##Densidade 

##Boxplot 

##Tabela de Intervalos 
br = seq(40,65,by=5)
ranges = paste(head(br,-1), br[-1], sep=" - ")
freq   = hist(dados$X2, breaks=br, include.lowest=TRUE, plot=FALSE)
data.frame("Intervalo" = ranges,
           "Frequência" = freq$counts,
           "Frequência relativa"=round(freq$counts/sum(freq$counts),3),
           "Frequência Acumulada"=cumsum(freq$counts),
           "Frequência relativa acumulada"=round(cumsum(freq$counts)/sum(freq$counts),3))



##Medidas descritivas
descritiva(dados$X2)[1:9]

##Teste de normalidade 
shapiro.test(dados$X2)

###X3
##Densidade 

##Boxplot 

##Tabela de Intervalos 
br = seq(0,10,by=2)
ranges = paste(head(br,-1), br[-1], sep=" - ")
freq   = hist(dados$X3, breaks=br, include.lowest=TRUE, plot=FALSE)
data.frame("Intervalo" = ranges,
           "Frequência" = freq$counts,
           "Frequência relativa"=round(freq$counts/sum(freq$counts),3),
           "Frequência Acumulada"=cumsum(freq$counts),
           "Frequência relativa acumulada"=round(cumsum(freq$counts)/sum(freq$counts),3))



##Medidas descritivas
descritiva(dados$X3)[1:9]

##Teste de normalidade 
shapiro.test(dados$X3)

###X4
##Densidade 

##Boxplot 

##Tabela de Intervalos 
br = seq(0,70,by=10)
ranges = paste(head(br,-1), br[-1], sep=" - ")
freq   = hist(dados$X4, breaks=br, include.lowest=TRUE, plot=FALSE)
data.frame("Intervalo" = ranges,
           "Frequência" = freq$counts,
           "Frequência relativa"=round(freq$counts/sum(freq$counts),3),
           "Frequência Acumulada"=cumsum(freq$counts),
           "Frequência relativa acumulada"=round(cumsum(freq$counts)/sum(freq$counts),3))



##Medidas descritivas
descritiva(dados$X4)[1:9]

##Teste de normalidade 
shapiro.test(dados$X4)

###X5
##Densidade 

##Boxplot 

##Tabela de Intervalos 
br = seq(35,125,by=10)
ranges = paste(head(br,-1), br[-1], sep=" - ")
freq   = hist(dados$X5, breaks=br, include.lowest=TRUE, plot=FALSE)
data.frame("Intervalo" = ranges,
           "Frequência" = freq$counts,
           "Frequência relativa"=round(freq$counts/sum(freq$counts),3),
           "Frequência Acumulada"=cumsum(freq$counts),
           "Frequência relativa acumulada"=round(cumsum(freq$counts)/sum(freq$counts),3))



##Medidas descritivas
descritiva(dados$X5)[1:9]

##Teste de normalidade 
shapiro.test(dados$X5)

###X11
##Densidade 

##Boxplot 

##Tabela de Intervalos 
br = seq(10,80,by=10)
ranges = paste(head(br,-1), br[-1], sep=" - ")
freq   = hist(dados$X11, breaks=br, include.lowest=TRUE, plot=FALSE)
data.frame("Intervalo" = ranges,
           "Frequência" = freq$counts,
           "Frequência relativa"=round(freq$counts/sum(freq$counts),3),
           "Frequência Acumulada"=cumsum(freq$counts),
           "Frequência relativa acumulada"=round(cumsum(freq$counts)/sum(freq$counts),3))



##Medidas descritivas
descritiva(dados$X11)[1:9]

##Teste de normalidade 
shapiro.test(dados$X11)

################################################################################################################################################
x8 <- dados %>%
  select(X8) %>%
  distinct() %>%
  mutate(x8.1 = ifelse(X8 == "NE", 1, 0),
         x8.2 = ifelse(X8 == "S", 1, 0),
         x8.3 = ifelse(X8 == "W", 1, 0),
         x8.4=ifelse(X8 == "NC",1,0))

x7= dados %>%
  select(X7) %>%
  distinct() %>%
  mutate(x7i = ifelse(X7 == "Sim", 1, 0))


dados <- dados %>%
  inner_join(x8)
dados=dados%>%inner_join(x7)
####################################################################################################################################################
##########Seleção de variavel
modmin<-lm(log(Y) ~ 1, data=dados)
step(modmin, direction='forward', scope=( ~ X1+X4+X5+X6+X9+x7i+x8.1+x8.2+x8.3+X11))
modcompl=lm(log(Y)~X1+X4+X5+X6+X9+x7i+x8.1+x8.2+x8.3+X11,data=dados)

step(modmin, scope=list(lower=modmin, upper=modcompl), direction="both",data=dados)
step(modcompl, direction = 'backward')

####################################################################################################
library(lmtest)
#primeiro modelo
mod=lm(log(Y)~X6+X11+X5,dados)
summary(mod)#0.7992
mean(vif(mod))
bptest(mod)
shapiro.test(mod$residuals)


#segundo modelo
mod=lm(log(Y)~X11+X9,dados)
summary(mod)#0.7928
mean(vif(mod))
bptest(mod)
shapiro.test(mod$residuals)

#terceiro modelo
mod=lm(log(Y)~X9+X11+X4,dados)
summary(mod)#0.7985
mean(vif(mod))
bptest(mod)
shapiro.test(mod$residuals)

#quarto modelo - tem que tratar os dados
mod=lm(log(Y)~I(X6^2)+X6+X11+I(X11^2),dados)
summary(mod)#0.8542
mean(vif(mod))
bptest(mod)
shapiro.test(mod$residuals)

#quinto modelo
mod=lm(log(Y)~X6+X11,dados)
summary(mod)#0.7992
mean(vif(mod))
bptest(mod)
shapiro.test(mod$residuals)

