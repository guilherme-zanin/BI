###################################################
#             ATIVIDADE AVALIATIVA 
#             ENTREGA EM 26/12/2019
#          Guilherme Augusto Mello Zanin
###################################################

#Com a base pm25 (em materiais), elabore um relatório para:

# Explicação de cada variável:
# No: número da linha
# year: ano dos dados nesta linha
# month: mês dos dados nesta linha 
# day: dia dos dados nesta linha
# hour: hora dos dados nesta linha
# pm2.5: concentração de PM2.5 (ug/m^3) 
# DEWP: Ponto de condensação da água - temperatura a qual o vapor de água que está em suspensão no ar começa a se condensar 
# TEMP: Temperatura 
# PRES: Pressão (hPa) 
# cbwd: Direção do vento
# Iws: Velocidade do vento acumulada (m/s) 
# Is: Horas acumuladas de neve 
# Ir: Horas acumuladas de chuva

#a) análise descritiva (médias, desvios, quartis e o que for
# relevante) para as variÃ¡veis do arquivo

setwd("C:\\Users\\Tramujas\\Desktop\\MBA Business Inteligence\\Analise Dados\\Dados\\PRSA2017_Data_20130301-20170228")

#a) análise descritiva (médias, desvios, quartis e o que for
# relevante) para as variÃ¡veis do arquivo

install.packages("fields")
require(fields)
tab<- cbind(stats(analise_pm25$pm2.5),stats(analise_pm25$DEWP), stats(analise_pm25$TEMP), stats(analise_pm25$PRES),stats(analise_pm25$Iws),stats(analise_pm25$Is),stats(analise_pm25$Ir))
colnames(tab)<- c("pm2.5","DEWP","TEMP","PRES","Iws","Is","Ir")
round(tab,2)

CV_pm25 <-(sd(analise_pm25$pm2.5,na.rm = TRUE))/(mean(analise_pm25$pm2.5,na.rm = TRUE))
CV_pm25
CV_DEWP <-(sd(analise_pm25$DEWP))/(mean(analise_pm25$DEWP))
CV_DEWP
CV_TEMP <-(sd(analise_pm25$TEMP))/(mean(analise_pm25$TEMP))
CV_TEMP
CV_PRES <-(sd(analise_pm25$PRES))/(mean(analise_pm25$PRES))
CV_PRES
CV_Iws <-(sd(analise_pm25$Iws))/(mean(analise_pm25$Iws))
CV_Iws
CV_Is <-(sd(analise_pm25$Is))/(mean(analise_pm25$Is))
CV_Is
CV_Ir <-(sd(analise_pm25$Ir))/(mean(analise_pm25$Ir))
CV_Ir

#A amplitude mais provável de pm2.5 é de 6,56 ug/m^3 a 190,66 ug/m^3
#Sua disperção é bastante alta, sendo pouco estável
#A classificação em quadrantes fica dessa forma:até 29 ug/m^3 concentração baixa,
#de 29,1 a 72 concentração moderadamente baixa, de 72,1 a 137 concentração
#moderadamente alta e maior que 137 concentração alta
#As demais variáveis também são bastante dispersa, apenas a variável PRES
#apresentou mais estabilidade.

#b) Elabore uma análise de correlação entre as variáveis
for(i in 6:12) {
  for(j in (i+1):12) {
    if(j <= 13 & j > i & !is.factor(analise_pm25[,i]) & !is.factor(analise_pm25[,j])) {
      print(paste(colnames(analise_pm25)[i], "&", colnames(analise_pm25)[j], sep = " "))
      print(cor(analise_pm25[,i], analise_pm25[,j], use= "complete.obs"))
    }
  }
}

# "DEWP & TEMP" tem uma correlação positiva muito forte
# "DEWP & PRES" tem uma correlação negativa forte
# "TEMP & PRES" tem uma correlação negativa muito forte
# "pm2.5 & Iws" e # "DEWP & Iws" apresentaram uma correlação negativa fraca
# Os demais não apresentaram correlação


#c) Crie um modelo preditivo, com regressão múltipla, para
#prever a quantidade futura de particulas pm25 suspensas no ar.

regression.model <- lm(analise_pm25$pm2.5 ~ analise_pm25$DEWP+ analise_pm25$TEMP+analise_pm25$PRES+analise_pm25$Iws+analise_pm25$Is+analise_pm25$Ir, data = analise_pm25)
regression.model

#pm.25 = 1728,4313 + 4,2822 DEWP + -6,0681 TEMP + -1,5291 PRES + -0,2616 Iws +
# -2,2669 Is + -7,2063 Ir

