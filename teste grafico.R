library(ggplot2)

# Dados de exemplo
dados <- data.frame(tempo = 1:10, teor = c(2, 4, 5, 7, 9, 11, 12, 14, 16, 18))

# Ajuste um modelo de regressão linear
modelo <- lm(teor ~ tempo, data = dados)

# Crie um conjunto de dados com os valores de "tempo" para previsões futuras
novos_dados <- data.frame(tempo = 11:15)

# Faça previsões com base no modelo ajustado
previsoes <- as.data.frame(predict(modelo, newdata = novos_dados, interval = "prediction"))
previsoes <- cbind(novos_dados,previsoes)

intervalos <- as.data.frame(predict(modelo, interval = "confidence", level = 0.95))
intervalos <- cbind(data.frame(tempo = 1:10), data.frame(fit = c(2, 4, 5, 7, 9, 11, 12, 14, 16, 18)) ,intervalos[,c("lwr", "upr")])

final <- rbind(intervalos, previsoes)

# Crie um gráfico de dispersão com os dados originais
p <- ggplot(data = final, aes(x = tempo, y = fit))

# Adicione uma linha de tendência com intervalo de previsão
p + geom_smooth(method = "lm", se = T) +
  geom_ribbon(data = final, aes(x = tempo, ymin = lwr, ymax = upr, y=fit), fill = "blue", alpha = 0.3)

