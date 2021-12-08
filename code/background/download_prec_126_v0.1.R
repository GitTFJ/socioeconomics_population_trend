strings126 = c(
  "2015_2020",
  "2021_2030",
  "2031_2040",
  #"2041_2050",
  "2051_2060",
  "2061_2070",
  "2071_2080",
  "2081_2090"#,
  #"2091_2100",
)


for(a in strings126){
  URL <- paste0("http://esg.pik-potsdam.de/thredds/fileServer/isimip_dataroot/isimip3b/input/clim_atm_sim/W5E5-ISIMIP3BASD2/MRI-ESM2-0/ssp126/prAdjust/daily/v20200619/mri-esm2-0_r1i1p1f1_w5e5_ssp126_prAdjust_global_daily_",a,".nc")
  download.file(URL, destfile = paste0("D:/ClimateData/Precipitation/126_",a,".nc"), method="curl")
}
