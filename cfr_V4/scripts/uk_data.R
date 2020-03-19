cruise_ship_ages <- c(16,23,347, 428,334,398,923,1015,216)

DPCFRSF <- 2.4/5
UKCFRSF <- 6.1/5

nCFRMid <- c(0,0.2,0.2,0.2,0.4,1.3,3.6,8,14.8)
cisLow <-  c(0, 0, 0.1, 0.1, 0.3, 1.1, 3.2, 7.2, 13.0)
cisHigh <-  c(0.9,1,0.4,0.4,0.6,1.5,4,8.9,16.7)

DPageSFs <- cruise_ship_ages/sum(cruise_ship_ages) * 10
UKageSFs <- c(1.18, 1.21, 1.36, 1.31, 1.46, 1.22, 1.08, 0.71, 0.39)

diamondPrincessCorrectedCFR = data.frame(cfrMid  =  nCFRMid * DPageSFs * DPCFRSF,
                                         cfrLow  =  cisLow * DPageSFs * DPCFRSF, 
                                        cfrHigh  =  cisHigh * DPageSFs * DPCFRSF)


UKCorrectedCFR = data.frame(cfrMid  =  nCFRMid * UKCFRSF* UKageSFs,
                            cfrLow  =  cisLow  * UKCFRSF * UKageSFs, 
                            cfrHigh  =  cisHigh * UKCFRSF * UKageSFs)

#UKageSFs <- c(1/9, 1/9, 1/9, 1/9, 1/9, 1/9, 1/9, 1/9, 1/9)
nicksData <- c(a1,a2,a3,a4,a5,a6,a7,a8,a9)

a1 <- 3924.49 + 4119.566
a2 <- 3956.34 + 3686.133
a3 <- 4074.64 + 4484.067
a4 <- 4706.828 + 4588.196
a5 <- 4308.13 + 4296.121
a6 <- 4634.54 + 4538.925
a7 <- 3905.016 + 3381.761
a8 <- 3388.488 + 2442.147
a9 <- 1736.567 +1077.555
