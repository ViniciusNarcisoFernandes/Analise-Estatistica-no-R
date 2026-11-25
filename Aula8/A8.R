E <- seq(1,20,by=2)
V <- c(3.09, 7.26, 17.64, 23.73, 28.50, 36.31, 34.39, 54.20, 42.03, 57.09)

print(E)

correlacao_pearson <- cor(E,V)
correlacao_pearson

# Estatística do teste 
# Arco tangente hiperbólico da correlação 
z_cal <- atanh(correlacao_pearson)
# z_0 arco tangente de rho_0 (correlação populacional sob H_0)
z_0 <- atanh(0.95)

# Valor de p
estat_norm <- (z_cal-z_0)/(sqrt(1/(length(E)-3)))
estat_norm
qnorm(0.95)

# Estatística Normalizada < z_{0.95} # Não rejeita-se H0
1-pnorm(estat_norm)

# atanh(correlacao_pearson) < 
atanh(0.95) + qnorm(0.95)*(1/sqrt(10-7))
