LR2 <- function (g1, g2, n.num, n.den=n.num, p, M, kappa.num, kappa.den=c(1,0,0), 
				alpha, theta=0, silent=FALSE, beta=0.5){
	num <- lik2(g1, g2, n=n.num, p, M, kappa.num, alpha, theta, beta)
	den <- lik2(g1, g2, n=n.den, p, M, kappa.den, alpha, theta, beta)
	if(silent){
		homo1 <- g1[1]==g1[2]
		homo2 <- g2[1]==g2[2]
		S <- length(p)
		if(homo1 & !homo2){ 
			num <- num+lik2(c(g1[1],S), g2, n=n.num, p, M, kappa.num, alpha, theta, beta)
			den <- den+lik2(c(g1[1],S), g2, n=n.den, p, M, kappa.den, alpha, theta, beta)
		}
		if(!homo1 & homo2){ 
			num <- num+lik2(g1, c(g2[1],S),  n=n.num, p, M, kappa.num, alpha, theta, beta)
			den <- den+lik2(g1, c(g2[1],S),  n=n.den, p, M, kappa.den, alpha, theta, beta)
		}
		if(homo1 & homo2){ 
			num <- num+lik2(c(g1[1],S), g2, n=n.num, p, M, kappa.num, alpha, theta, beta)+
				lik2(g1, c(g2[1],S),  n=n.num, p, M, kappa.num, alpha, theta, beta)+
				lik2(c(g1[1],S), c(g2[1],S),  n=n.num, p, M, kappa.num, alpha, theta, beta)
			den <- den+lik2(c(g1[1],S), g2, n=n.den, p, M, kappa.den, alpha, theta, beta)+
				lik2(g1, c(g2[1],S),  n=n.den, p, M, kappa.den, alpha, theta, beta)+
				lik2(c(g1[1],S), c(g2[1],S),  n=n.den, p, M, kappa.den, alpha, theta, beta)
		}
	}
	LR <- num/den
	list(numerator=num, denominator=den, LR=LR)
}
