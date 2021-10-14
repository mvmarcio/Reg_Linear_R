#============================================================================
# Exemplo do investimento nos EUA de jan/1950 a dez/2000
# Dados da tabela F5-2 disponivel em "exemplo_12.txt"
# Ver descricao dos modelos em Aula_02.pdf
#============================================================================
#============================================================================
# Parte I - Ajustes e testes
#============================================================================
# Analise: Equacao de Investimento
#============================================================================
dataxy     <- read.table("exemplo_12.txt",header=T)
dataxy[1:5,]
n          <- dim(dataxy)[1]
k          <- dim(dataxy)[2]
lninvest   <- log(dataxy[2:n,5])
txjuros    <- dataxy[2:n,10]
inflacao   <- diff(log(dataxy[,8]))*4*100 # Dados trimestrais, 100%
lnPIB      <- log(dataxy[2:n,3])
tempo      <- 1:(n-1)
SST        <- t(lninvest-mean(lninvest))%*%(lninvest-mean(lninvest))
#============================================================================
# Analise do modelo 1: irrestrito
#============================================================================
yfit1      <- lm(lninvest ~ txjuros + inflacao + lnPIB + tempo)
summary(yfit1)
#============================================================================
#lm(formula = lninvest ~ txjuros + inflacao + lnPIB + tempo)

#Residuals:
#  Min       1Q   Median       3Q      Max
#-0.22313 -0.05540 -0.00312  0.04246  0.31989
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept) -9.134092   1.366459  -6.684  2.3e-10 ***
#  txjuros     -0.008598   0.003196  -2.691  0.00774 **
#  inflacao     0.003306   0.002337   1.415  0.15872   
#lnPIB        1.930156   0.183272  10.532  < 2e-16 ***
#  tempo       -0.005659   0.001488  -3.803  0.00019 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.08618 on 198 degrees of freedom
#Multiple R-squared:  0.9798, Adjusted R-squared:  0.9793
#F-statistic:  2395 on 4 and 198 DF,  p-value: < 2.2e-16
#=================================================================
X          <- cbind(txjuros,inflacao,lnPIB,tempo)
anova(yfit1)
#=================================================================
#Analysis of Variance Table

#Response: lninvest
#Df Sum Sq Mean Sq  F value    Pr(>F)   
#txjuros     1 19.631  19.631 2643.118 < 2.2e-16 ***
#  inflacao    1  1.575   1.575  212.079 < 2.2e-16 ***
#  lnPIB       1 49.845  49.845 6711.270 < 2.2e-16 ***
#  tempo       1  0.107   0.107   14.463 0.0001904 ***
#  Residuals 198  1.471   0.007                      
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#=================================================================

yfit1x     <- lm(lninvest ~ X)
anova(yfit1x)      # Tabela anova
#=================================================================
#Analysis of Variance Table

#Response: lninvest
#Df Sum Sq Mean Sq F value    Pr(>F)   
#X           4 71.158 17.7896  2395.2 < 2.2e-16 ***
#  Residuals 198  1.471  0.0074                     
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#=================================================================

options(digits=4) 
vcov(yfit1)          # Matriz de covariancia
#====================================================================
#             (Intercept)    txjuros   inflacao      lnPIB      tempo
#(Intercept)   1.8672103  1.079e-03  8.312e-04 -0.2504208  2.026e-03
#txjuros       0.0010789  1.021e-05 -3.717e-06 -0.0001465  9.861e-07
#inflacao      0.0008312 -3.717e-06  5.462e-06 -0.0001121  9.697e-07
#lnPIB        -0.2504208 -1.465e-04 -1.121e-04  0.0335887 -2.718e-04
#tempo         0.0020256  9.861e-07  9.697e-07 -0.0002718  2.214e-06
#====================================================================
confint(yfit1)       # Intervalo de confianca
#==================================
#                 2.5 %    97.5 %
#(Intercept) -11.828773 -6.439411
#txjuros      -0.014899 -0.002296
#inflacao     -0.001302  0.007915
#lnPIB         1.568740  2.291572
#tempo        -0.008593 -0.002724
#==================================

R2.yfit1   <- 1 - t(yfit1$res)%*%yfit1$res / SST
R2.yfit1
#===========
#      [,1]
#[1,] 0.9798
#===========
#============================================================================
# Analise do modelo 2: restrito
#============================================================================
txjurosx   <- txjuros - inflacao
yfit2      <- lm(lninvest ~ txjurosx + lnPIB + tempo)
summary(yfit2)
#=================================================================
#lm(formula = lninvest ~ txjurosx + lnPIB + tempo)

#Residuals:
#  Min       1Q   Median       3Q      Max
#-0.22790 -0.05454 -0.00243  0.03999  0.31393

#Coefficients:
# Estimate Std. Error t value Pr(>|t|)   
#(Intercept) -7.90716    1.20063   -6.59  3.9e-10 ***
#  txjurosx    -0.00443    0.00227   -1.95   0.0526 . 
#lnPIB        1.76406    0.16056   10.99  < 2e-16 ***
#  tempo       -0.00440    0.00133   -3.31   0.0011 **
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.0867 on 199 degrees of freedom
#Multiple R-squared:  0.979, Adjusted R-squared:  0.979
#F-statistic: 3.15e+03 on 3 and 199 DF,  p-value: <2e-16
#================================================================

X          <- cbind(txjurosx,lnPIB,tempo)
anova(yfit2)
#================================================================
#Analysis of Variance Table

#Response: lninvest
#Df Sum Sq Mean Sq F value Pr(>F)   
#txjurosx    1    6.3     6.3   837.7 <2e-16 ***
#  lnPIB       1   64.8    64.8  8614.8 <2e-16 ***
#  tempo       1    0.1     0.1    10.9 0.0011 **
#  Residuals 199    1.5     0.0                  
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#================================================================

yfit2x     <- lm(lninvest ~ X)
anova(yfit2x)        # Tabela anova
#================================================================
#Analysis of Variance Table

#Response: lninvest
#Df Sum Sq Mean Sq F value Pr(>F)   
#X           3   71.1   23.71    3154 <2e-16 ***
#  Residuals 199    1.5    0.01                  
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#================================================================

options(digits=4)  
vcov(yfit2)          # Matriz de covariancia
#=========================================================
#             (Intercept)   txjurosx      lnPIB      tempo
#(Intercept)   1.4415149 -4.319e-04 -1.928e-01  1.591e-03
#txjurosx     -0.0004319  5.154e-06  5.802e-05 -5.623e-07
#lnPIB        -0.1927647  5.802e-05  2.578e-02 -2.129e-04
#tempo         0.0015911 -5.623e-07 -2.129e-04  1.771e-06
#==========================================================
confint(yfit2)       # Intervalo de confianca
#===================================
#                  2.5 %     97.5 %
#(Intercept) -10.274751 -5.540e+00
#txjurosx     -0.008903  5.015e-05
#lnPIB         1.447442  2.081e+00
#tempo        -0.007027 -1.778e-03
#===================================
R2.yfit2   <- 1 - t(yfit2$res)%*%yfit2$res / SST
R2.yfit2
#===========
#      [,1]
#[1,] 0.9794
#===========
#============================================================================
# Testando a restricao atraves do teste t
#============================================================================
#obs. n numero de observacoes da base de dados!
#  n-1 (devido ao lag da variavel se perde uma observacao
#       e 5 eh o numero de parametros da regressao)
# o alpha eh 0.025

qchapeu  <- -0.00860 + 0.00331
EP.q     <- sqrt( 0.00320^2 + 0.00234^2 + 2*(-3.717e-06) )  
valort   <- qchapeu / EP.q
abs(valort) > qt(0.975,(n-1)-5)
# valor do teste t calulcado usando 97,5% de confianca com 198 (204-1-5) graus de liberdade.
abs(valort)
#========
#[1] 1.838
#========
# o valor do teste t tabelado sera
testet = qt(0.975,(n-1)-5)
testet
#========
#[1] 1.972
#========

#============================================================================
# Testando H_0: beta_1+beta_2=0, beta_4=1, e beta_5=0
#============================================================================
R        <- matrix(0,3,5)
R[1,2:3] <- c(1,1)
R[2,4]   <- 1
R[3,5]   <- 1
q        <- c(0,1,0)
m        <- R%*%yfit1$coef-q


# variancia estimada de m (ou melhor
#matriz de variancia-covariancia de yfit1 pre e pos multiplicada por R que
#sera o numerador do teste F pag. 119 cap. 5)
estvarm  <- R%*%vcov(yfit1)%*%t(R)

valorF   <- t(m)%*%solve(estvarm)%*%m/3
valorF
# qf(0.95,3,5) - estatistica f com 3 graus de liberdade no numerador e
#5 graus no denominador.
valorF   > qf(0.95,3,(n-1)-5)
pf(valorF,3,(n-1)-5,lower.tail=F)  # p-valor


#============================================================================
# Analise do modelo 3: restrito 
# Calculando a estatistica F com R^2 (irrestrito) e R*^2 (restrito)
# Testando H_0: beta_1+beta_2=0, beta_4=1, e beta_5=0
#============================================================================
lninvestx  <-  lninvest - lnPIB    # porque beta_4=1
yfit3      <- lm(lninvestx ~ txjurosx )
summary(yfit3)
R2.yfit3   <- 1 - t(yfit3$res)%*%yfit3$res / SST
valorFx    <- (R2.yfit1-R2.yfit3)/3 / ( (1- R2.yfit1)/yfit1$df.residual  )
# Verificando os valores da estatistica F
valorF
valorFx
# Verificando a diminuicao no R^2 (irrestrito) para R*^2 (restrito)
R2.yfit1    # R^2 (irrestrito)
R2.yfit3    # R*^2 (restrito)







#============================================================================
#============================================================================
# Parte 2 - Predicao ()
#============================================================================
#             c(cst, txjuros,      inflacao,          lnPIB,       tempo)
x0         <- c(  1,    4.48, log(528.0/521.1)*4*100, log(9316.8), 204  )
ypred      <- t(x0)%*%yfit1$coef
s2         <- t(yfit1$res)%*%yfit1$res/yfit1$df.residual
ep.ypred   <- sqrt( s2 + t(x0)%*%vcov(yfit1)%*%x0  )
#talpha    <- qt(0.975,yfit1$df.residual)
talpha     <- qnorm(0.975)
c(x0)
c(ypred,  ep.ypred)
c(ypred-talpha*ep.ypred, ypred+talpha*ep.ypred) # Intervalo de confianca

#============================================================================
# Fim
#============================================================================



#============================================================================
# Exemplo da funcao de producao translog
# Dados da tabela F5-3 disponivel em "exemplo_13.txt"
# Ver descricao dos modelos em Aula_02.pdf
#============================================================================
# Analise: Funcao de Producao
#============================================================================
dataxy     <- read.table("exemplo_13.txt",header=T)
n          <- dim(dataxy)[1]
lnprod     <- log(dataxy[,1])
lntrab     <- log(dataxy[,2])
lncapi     <- log(dataxy[,3])
lntrabx    <- 0.5*(lntrab^2)
lncapix    <- 0.5*(lncapi^2)
SST        <- t(lnprod-mean(lnprod))%*%(lnprod-mean(lnprod))
#============================================================================
# Analise do modelo 1: irrestrito (maior)
#============================================================================
yfit1      <- lm(lnprod ~ lntrab + lncapi + lntrabx + lncapix + lntrab:lncapi)
summary(yfit1)
X          <- cbind(lntrab,lncapi,lntrabx,lncapix,lntrab*lncapi)
anova(yfit1)
yfit1x     <- lm(lnprod ~ X)
anova(yfit1x)        # Tabela anova
vcov(yfit1)          # Matriz de covariancia
confint(yfit1)       # Intervalo de confianca
R2.yfit1   <- 1 - t(yfit1$res)%*%yfit1$res / SST
#============================================================================
# Analise do modelo 2: restrito (menor)
# Calculando a estatistica F com R^2 (irrestrito) e R*^2 (restrito)
# Testando H_0: beta_4 = beta_5 = beta_6 = 0
#============================================================================
yfit2      <- lm(lnprod ~ lntrab + lncapi)
summary(yfit2)
X          <- cbind(lntrab,lncapi)
anova(yfit2)
yfit2x     <- lm(lnprod ~ X)
anova(yfit2x)        # Tabela anova
vcov(yfit2)          # Matriz de covariancia
confint(yfit2)       # Intervalo de confianca
R2.yfit2   <- 1 - t(yfit2$res)%*%yfit2$res / SST
valorF     <- (R2.yfit1-R2.yfit2)/3 / ( (1- R2.yfit1)/yfit1$df.residual  )
valorF > qf(0.95,3,yfit2$df.residual) # Hipotese nula nao eh rejeitada
# Conclusao: a funcao Cobb-Douglas eh mais apropriada.

# Verificando a diminuicao no R^2 (irrestrito) para R*^2 (restrito)
R2.yfit1    # R^2 (irrestrito)
R2.yfit2    # R*^2 (restrito)

#============================================================================
# Testando H_0: beta_2 + beta_3 = 1  utilizando o teste t e o F
#============================================================================
m          <- 0.6030 + 0.3757 - 1
ep.m       <- sqrt(0.1260^2+0.0853^2-2*0.009616)
valort     <- m / ep.m
abs(valort) > qt(0.975,yfit2$df.residual) # Hipotese nula nao eh rejeitada
valorF     <- valort^2    
valorF     >  qf(0.95,1,yfit2$df.residual) # Hipotese nula nao eh rejeitada

#============================================================================
# Fim
#============================================================================

#============================================================================
# Exemplo da propensao marginal a consumir (PMC) de longo prazo.
# Os dados sao os mesmos do exemplo 12 do investimento nos EUA
# de jan/1950 a dez/2000
# Dados da tabela F5-2 disponivel em "exemplo_12.txt"
# Ver descricao dos modelos em Aula_02.pdf
#============================================================================
# Analise: Equacao de Investimento
#============================================================================
dataxy     <- read.table("exemplo_12.txt",header=T)
dataxy[1:5,]
n          <- dim(dataxy)[1]
k          <- dim(dataxy)[2]
lncons     <- log(dataxy[2:n,4])
lnconstm1  <- log(dataxy[1:(n-1),4])
lnyt       <- log(dataxy[2:n,7])
SST        <- t(lncons-mean(lncons))%*%(lncons-mean(lncons))
#============================================================================
# Analise do modelo 1: irrestrito
#============================================================================
options(digits=7,decimals=7)
yfit1      <- lm(lncons ~ lnyt + lnconstm1)
summary(yfit1)
covb       <- vcov(yfit1)          # Matriz de covariancia
delta      <- yfit1$coef[2] / (1 - yfit1$coef[3]) # PMC longo prazo
gbeta      <- 1 / (1 - yfit1$coef[3])
ggamma     <- delta / (1 - yfit1$coef[3])
g          <- c(gbeta,ggamma)
ep.delta   <- sqrt( t(g)%*%covb[2:3,2:3]%*%g )
valorz     <- (delta-1) / ep.delta
cbind( delta, ep.delta, abs(valorz), qnorm(0.975) )
abs(valorz) > qnorm(0.975) # A hipotese nula nao eh rejeitada
# Assim, a hipotese de delta = 1 nao eh rejeitada.

#============================================================================
# Fim
#============================================================================

#============================================================================
# Exemplo do rendimento em funcao da idade, educacao e filhos, Mroz(1987)
# Dados da tabela F5-1 disponivel em "exemplo_15.txt"
# Ver descricao dos modelos em Aula_02.pdf
#============================================================================
# Analise para os dados completos
#============================================================================
dataxy   <- read.table("exemplo_15.txt",header=T)
lnganhos <- log(dataxy[1:428,2]*dataxy[1:428,7])
idade    <- dataxy[1:428,5]
idade2   <- idade^2
educacao <- dataxy[1:428,6]
filhos   <- as.vector( (dataxy[1:428,3]+dataxy[1:428,4])>=1,mode="numeric" )
yfit     <- lm(lnganhos ~ idade + idade2 + educacao + filhos)
summary(yfit)
X        <- cbind(idade,idade2,educacao,filhos)
anova(yfit)
yfit2    <- lm(lnganhos ~ X)
anova(yfit2)        # Tabela anova
options(digits=4)  
vcov(yfit)          # Matriz de covariancia
confint(yfit)       # Intervalo de confianca

#============================================================================
# Fim
#============================================================================

#============================================================================
# Exemplo das companhias aereas
# Dados da tabela F6-1 disponivel em "exemplo_16.txt
# Ver descricao do modelo em Aula_02.pdf
#============================================================================
# Analise: Equacao de custos
#============================================================================
dataxy     <- read.table("exemplo_16.txt",header=T)
dataxy[1:5,]
n          <- dim(dataxy)[1]
k          <- dim(dataxy)[2]
lncit      <- log(dataxy[,3])
lnqit      <- log(dataxy[,4])
lnqitsq    <- lnqit^2
lnpit      <- log(dataxy[,5])
lfit       <- dataxy[,6]
D          <- matrix(0,n,14)
i          <- 1
j          <- 1
while(i <= n){
  D[i,j] <- 1
  i      <- i + 1
  j      <- j + 1
  if( j==15){
    i <- i+1
    j <- 1
  }
}
F          <- matrix(0,n,5)
i          <- 1
j          <- 1
while(i <= (n-15)){
  F[i,j] <- 1
  if( !(i %% 15) ){
    j <- j + 1
  }
  i      <- i + 1
}
SST        <- t(lncit-mean(lncit))%*%(lncit-mean(lncit))

#============================================================================
# Analise do modelo
#============================================================================
# Modelo sem efeitos # 14+5 = 19 restricoes
X          <- cbind(lnqit,lnqitsq,lnpit,lfit)
yfit1      <- lm(lncit ~ X)
SSE1       <- sum(yfit1$res^2)
SSE1

# Modelo com efeitos da firma somente # 14 restricoes
X          <- cbind(lnqit,lnqitsq,lnpit,lfit,F)
yfit2      <- lm(lncit ~ X)
SSE2       <- sum(yfit2$res^2)
SSE2

# Modelo com efeitos temporais somente # 5 restricoes
X          <- cbind(lnqit,lnqitsq,lnpit,lfit,D)
yfit3      <- lm(lncit ~ X)
SSE3       <- sum(yfit3$res^2)
SSE3

# Modelo completo # 0 restricao
X          <- cbind(lnqit,lnqitsq,lnpit,lfit,D,F)
yfit4      <- lm(lncit ~ X)
SSE4       <- sum(yfit4$res^2)
SSE4

# Os coeficientes sao conjuntamente iguais a zero
F1         <- ((SSE1 - SSE4) / 19 )/ ( SSE4/yfit4$df  )
F1 > qf(0.95,19,yfit4$df) # O modelo completo eh melhor
# Efeitos da firma e temporais sao importantes

# Os coeficientes sao conjuntamente iguais a zero
F2         <- ((SSE2 - SSE4) / 14 )/ ( SSE4/yfit4$df  )
F2 > qf(0.95,14,yfit4$df) # O modelo completo eh melhor
# Efeitos da firma sao importantes

# Os coeficientes sao conjuntamente iguais a zero
F3         <- ((SSE3 - SSE4) / 5 )/ ( SSE4/yfit4$df  )
F3 > qf(0.95,5,yfit4$df) # O modelo completo eh melhor
# Efeitos temporais sao importantes

#============================================================================
# Fim
#============================================================================ 
