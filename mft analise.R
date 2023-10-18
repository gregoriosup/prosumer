#### pacotes####
library(ggplot2)
library(dplyr)

#### dados ####

meses <- c(0,1,2,3)
teor <- c(104.84,101.53,94.73,90.61)
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

shelflife <- approx(y=linha_ic$meses, x=linha_ic$lwr, xout = 18)$y


#### grafico final ####

ggplot(data = dados_1, aes(x=meses, y= teor))+
  geom_smooth(method = "lm", color = "black", linewidth = 0.5, se=T)+
  geom_hline(yintercept = 18, alpha = 0.5, linetype = "dashed")+ 
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 102))+
  theme_classic()+
  geom_point()+
  #geom_text(aes(label= format(teor, nsmall=2)), vjust= -2.5)+
  labs(x="Mês", y= "Teor (%)")+
  geom_smooth(aes(y=fit),data= dados_pred, method = "lm", color = "black", linewidth = 0.5, linetype= "solid")+
  # geom_smooth(aes(y=lwr), data=linha_inf, linetype= "dashed", method = "loess", se=F, color= "black", size=0.5)+
  geom_smooth(aes(y=lwr), data=linha_ic, linetype= "dashed", method = "loess", span=0.1,se=F, color= "red", size=0.5)+
  #geom_vline(xintercept = shelflife, linetype = "dashed")+
  #geom_point( x=shelflife, y=29, color = "red", shape=15, size=3)+
  geom_text(x=20, y=20.2, label="Limite inferior", alpha = 0.2)+
  geom_text(x=13.3, y=92, label = "Validade = 13 meses", alpha = 0.2, angle = -90)+
  geom_vline(xintercept = shelflife, linetype = "dashed", alpha = 0.5)+
  geom_segment(x=15, xend=15.5, y=90, yend=90)+
  geom_segment(x=15, xend=15.5, y=85, yend=85, linetype="dashed", color="red")+
  geom_text(x=15.8, y=90, label="Reta ajustada", alpha = 0.5, hjust = "left")+
  geom_text(x=15.8, y=85, label="IC 97,5%", alpha = 0.5, hjust = "left")+
  geom_ribbon(data = dados_ic, aes(x = meses, ymin = lwr, ymax = upr, y=fit), alpha = 0.2)


  
  
  

#### analise grafica ####

#plot(dados$meses, dados$teor) #verificar linearidade 

#par(mfrow=c(2,2))
#plot(modelo)
#par(mfrow=c(1,1))

