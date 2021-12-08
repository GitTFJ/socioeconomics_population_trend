PET_fun = function(tmean){
  if(exists("days")){
  } else {
    days = stack("C:/Users/mn826766/OneDrive - University of Reading/PhDResearch/UnderstandingDeclinesInLargeCarnivores/Chapters/CarnivorePopulationTrends/Documents/ModellingTrends/Data/Climate/DaysPerMonthRast.tif")
  }
  I_hold = stack()
  for (i in 1:12){
    tm =tmean[[i]];
    tm1 = tm>=0; #But only where tmean > 0, elsewhere PET is 0?
    tm = tm*tm1
    I = ((tm)/5)
    I=I**1.514
    I_hold = stack(I_hold, I)
  }
  Ival = sum(I_hold)
  
  alpha = ((0.000000675)*Ival**3) - ((0.0000771)*Ival**2) + ((0.01792)*Ival) + 0.49239
  
  #N array of number of days each month
  N <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  
  PET <- function(n, daylength, monthd, TM, alpha, I) { #n = iteration (month) # 'a' = daylength grid, 'b' = N -- array of days in month, 'c' tmean grid, 'd' alpha grid
    mdays = (daylength/12) * (monthd/30)
    #plot(mdays)
    P = 16 * mdays *  (((10 * TM)/I)**alpha); #in mm/month
    tm1 = TM>=0;
    P = P*tm1;
    return(P)
  }
  potET <- stack()
  for (i in 1:12) {
    est <- PET(i, daylength = days[[i]], monthd = N[i], TM = tmean[[i]], alpha = alpha, I = Ival)
    potET <- stack(potET, est)
  }
  potET[is.na(potET)] = 0;
  return(potET)
}
