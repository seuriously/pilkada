library(dplyr)
library(RMySQL)
library(jsonlite)
library(httr)

con = dbConnect(MySQL(), host="localhost", user= "root", db = 'Pilkada')
jsonlink = c("listNasional.json","listDps.json")
mainURL = "https://pilkada2017.kpu.go.id/pemilih/dps"
json2df = function(url){
  link = content(url, as = "text")
  df <- fromJSON(link, simplifyDataFrame = TRUE)
  if(length(df)<=1) return(df[[1]])
  return(df[[4]])
  
}

url = GET(paste(mainURL,jsonlink[1],sep = "/"))
dfProvinsi = json2df(url)
provinsi = dfProvinsi$namaWilayah
provinsi = gsub(" ", "%20", provinsi)
for(i in 6){
	url = GET(paste(mainURL,provinsi[i],jsonlink[2],sep = "/"))
  dfKota = json2df(url)
  kota = dfKota$namaKabKota
  kota = gsub(" ", "%20", kota)
	
  for(j in 1:length(kota)){
		url = GET(paste(mainURL, provinsi[i], kota[j],jsonlink[2],sep = "/"))
    dfkecamatan = json2df(url)
    kecamatan = dfkecamatan$namaKecamatan
    kecamatan = gsub(" ", "%20", kecamatan)

		for(k in 1:length(kecamatan)){
			url = GET(paste(mainURL, provinsi[i], kota[j],kecamatan[k], jsonlink[2],sep = "/"))
      dfKelurahan = json2df(url)
      kelurahan = dfKelurahan$namaKelurahan
      kelurahan = gsub(" ", "%20", kelurahan)

			for(l in 1:length(kelurahan)){
				url = GET(paste(mainURL, provinsi[i], kota[j],kecamatan[k], kelurahan[l], jsonlink[2],sep = "/"))
        dfTPS = json2df(url)
        tps = dfTPS$tps
        tps = gsub(" ", "%20", tps)

				for(m in 1:length(tps)){
				  url = GET(paste(mainURL, provinsi[i], kota[j],kecamatan[k], kelurahan[l], tps[m], jsonlink[2],sep = "/"))
				  possibleError = tryCatch({
				    dfdat = json2df(url)},
					  error = function(e) e)
				  if(inherits(possibleError, "error")| nrow(dfdat) < 1) next
					dfdat$kelurahan = gsub("%20", " ", kelurahan[l])
					dfdat$kecamatan = gsub("%20", " ", kecamatan[k])
					dfdat$kota = gsub("%20", " ", kota[j])
					message(paste0("provinsi ", provinsi[i], " kota ", kota[j], " kecamatan ", kecamatan[k], " kelurahan ", kelurahan[l], " tps ", tps[m]))
					ifelse(j==1 & k==1 & l==1 & m==1, dbWriteTable(con, "pilkadajakarta2017", dfdat), dbWriteTable(con, "pilkadajakarta2017", dfdat, append=T, overwrite=F))
		      message("written to database")
				
				}
			}
		}
	}
}
