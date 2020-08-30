#Valor de Desafío (Challenge Rating) para DnD 5e.
rm(list = ls())

#Función XP para ajustar la experiencia.
XP <- function(Party, Monsters) {
  Level <- 1:20
  Easy <- c(25, 50, 75, 125, 250, 300, 350, 450, 550, 600, 800, 1000,
            1100, 1250, 1400, 1600, 2000, 2100, 2400, 2800)
  Medium <- c(50, 100, 150, 250, 500, 600, 750, 900, 1100, 1200,
              1600, 2000, 2200, 2500, 2800, 3200, 3900, 4200,
              4900, 5700)
  Hard <- c(75, 150, 225, 375, 750, 900, 1100, 1400, 1600, 1900,
            2400, 3000, 3400, 3800, 4300, 4800, 5900, 6300,
            7300, 8500)
  Deadly <- c(100, 200, 400, 500, 1100, 1400, 1700, 2100, 2400, 2800, 3600, 4500, 5100, 5700, 6400, 7200, 8800, 9500, 10900, 12700)
  UmbralParty <- cbind(Level, Easy, Medium, Hard, Deadly)
  #Definición de parámetros 2
  MonsterXP <- c(10, 25, 50, 100, 200, 450, 700, 1100, 1800, 2300, 2900, 3900, 5000, 5900, 7200, 8400, 10000, 11500, 13000, 15000, 18000, 20000, 22000, 25000)
  MonsterXP <- MonsterXP * 4/length(Party) #Ajustamos la XP en función del número de PJs
  CR <- c(0, 1/8, 1/4, 1/2, 1:20)
  Encounter <- cbind(CR, MonsterXP)
  #Calculamos la experiencia que debe dar el encuentro
  XP <- rep(0, 4)
  #Fácil
  XP[1] <- sum(UmbralParty[Party, 2])
  #Medio
  XP[2] <- sum(UmbralParty[Party, 3])
  #Difícil
  XP[3] <- sum(UmbralParty[Party, 4])
  #Letal
  XP[4] <- sum(UmbralParty[Party, 5])
  #Calculamos la XP del encuentro que introducimos.
  CRpos <- vector("numeric", length(Monsters))
  for(i in 1:length(Monsters)) {
    CRpos[i] <- which(CR == Monsters[i])
  }
  Multi <- c(1, 1.5, 2, 2, 2, 2, 2.5, 2.5, 2.5, 2.5)
  mXP <- sum(MonsterXP[CRpos])*Multi[length(Monsters)]
  res <- c(XP, mXP)
  names(res) <- c("Fácil", "Medio", "Difícil", "Letal", "Encuentro")
  return(res)
}

#Calculadora: XP(Vector niveles PJs+NPCs, Vector CR enemigos)
XP(c(3, 3, 3), c(1, 1))
