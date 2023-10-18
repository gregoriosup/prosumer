#### pacotes####
library(ggplot2)
library(dplyr)

#### dados ####

meses <- c(0,1,2,3)
teor <- c(104.02,98.74,92.87,89.9)
dados <- data.frame(meses, teor)

#### analise ####

modelo <- lm(teor~meses, dados)

summary(modelo)


#### previsao ####

dados_1 <- cbind(dados, predict(modelo, interval = 'confidence'))

dados_pred <- cbind(data.frame(meses=dados[nrow(dados),1]:48), 
                    predict(modelo, interval = "prediction", newdata = data.frame(meses=dados[nrow(dados),1]:48)))

linha_inf <- rbind(dados_1[,c(1,4)], dados_pred[,c(1,3)])

dados_ic <- cbind(data.frame(meses=dados[nrow(dados),1]:48), 
      predict(modelo, interval = "confidence", newdata = data.frame(meses=dados[nrow(dados),1]:48)))
linha_ic <- rbind(dados_1[,c(1,4)], dados_ic[,c(1,3)])

shelflife <- approx(y=linha_ic$meses, x=linha_ic$lwr, xout = 90)$y

modelo2 <- lm(meses~lwr, linha_ic)
shelflife2 <- predict(modelo2, newdata = data.frame(lwr = (90)))

#### grafico final ####

ggplot(data = dados_1, aes(x=meses, y= teor))+
  geom_smooth(method = "lm", color = "black", linewidth = 0.5, se=T)+
  geom_hline(yintercept = 90, alpha = 0.5, linetype = "dashed")+ 
  coord_cartesian(xlim = c(0, 5), ylim = c(80, 105))+
  theme_classic()+
  geom_point()+
  #geom_text(aes(label= format(teor, nsmall=2)), vjust= -2.5)+
  labs(x="Mês", y= "Teor (%)")+
  geom_smooth(aes(y=fit),data= dados_pred, method = "lm", color = "black", linewidth = 0.5, linetype= "solid")+
  # geom_smooth(aes(y=lwr), data=linha_inf, linetype= "dashed", method = "loess", se=F, color= "black", size=0.5)+
  geom_smooth(aes(y=lwr), data=linha_ic, linetype= "dashed", method = "loess",span=0.1, se=F, color= "red", size=0.5)+
  #geom_vline(xintercept = shelflife, linetype = "dashed")+
  #geom_point( x=shelflife, y=90, color = "red", shape=15, size=2.5)+
  geom_text(x=5, y=90.5, label="Limite inferior", alpha = 0.2, hjust="right")+
  geom_text(x=2.32, y=102, label = "Validade = 2,3 meses", alpha = 0.2, angle = -90, vjust="up")+
  geom_vline(xintercept = shelflife, linetype = "dashed", alpha = 0.5)+
  geom_segment(x=4, xend=4.2, y=100, yend=100)+
  geom_segment(x=4, xend=4.2, y=98, yend=98, linetype="dashed", color="red")+
  geom_text(x=4.3, y=100, label="Reta ajustada", alpha = 0.5, hjust = "left")+
  geom_text(x=4.3, y=98, label="IC 97,5%", alpha = 0.5, hjust = "left")+
  geom_text(x=4.3, y=98, label="IC 97,5%", alpha = 0.5, hjust = "left")+
  geom_ribbon(data = dados_ic, aes(x = meses, ymin = lwr, ymax = upr, y=fit), alpha = 0.2)

  
  

#### analise grafica ####

#plot(dados$meses, dados$teor) #verificar linearidade 

#par(mfrow=c(2,2))
#plot(modelo)
#par(mfrow=c(1,1))

