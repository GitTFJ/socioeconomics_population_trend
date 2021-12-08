stringshist = c(
  #"1901_1910",
  #"1911_1920",
  "1921_1930",
  "1931_1940",
  "1941_1950",
  "1951_1960",
  "1961_1970",
  "1971_1980",
  "1981_1990",
  "1991_2000",
  #"2001_2010",
  "2011_2014"
)


for(a in stringshist){
  URL <- paste0("http://esg.pik-potsdam.de/thredds/fileServer/isimip_dataroot/isimip3b/input/clim_atm_sim/W5E5-ISIMIP3BASD2/MRI-ESM2-0/historical/prAdjust/daily/v20200619/mri-esm2-0_r1i1p1f1_w5e5_historical_prAdjust_global_daily_",a,".nc")
  download.file(URL, destfile = paste0("D:/ClimateData/Precipitation/hist_",a,".nc"), method="curl")
}

