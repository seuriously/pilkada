library(rvest)
library(dplyr)
library(RMySQL)

con = dbConnect(MySQL(), host="localhost", user= "root", db = 'Pilkada')

link = read_html("https://data.kpu.go.id/dps.php")
provinsi_id = link %>% html_nodes(xpath =  '//*[@name="wilayah_id"]/option') %>% html_attr("value")
provinsi = cbind(provinsi_id, provinsi = link %>% html_nodes(xpath =  '//*[@name="wilayah_id"]/option') %>% html_text()) %>% data.frame() %>% filter(provinsi_id!="")

for(i in 6){
	link = read_html(paste0("https://data.kpu.go.id/dps.php?cmd=select&grandparent=0", "&parent=", provinsi$provinsi_id[i]))
	city_id = link %>% html_nodes(xpath =  '//*[@name="wilayah_id"]/option') %>% html_attr("value")
	city = cbind(city_id, city = link %>% html_nodes(xpath =  '//*[@name="wilayah_id"]/option') %>% html_text()) %>% data.frame() %>% filter(city_id!="")
	
	for(j in 1:nrow(city)){
		link = read_html(paste0("https://data.kpu.go.id/dps.php?cmd=select&grandparent=",provinsi$provinsi_id[i], "&parent=", city$city_id[j]))
		kecamatan_id = link %>% html_nodes(xpath =  '//*[@name="wilayah_id"]/option') %>% html_attr("value")
		kecamatan = cbind(kecamatan_id, kecamatan = link %>% html_nodes(xpath =  '//*[@name="wilayah_id"]/option') %>% html_text()) %>% data.frame() %>% filter(kecamatan_id!="")
		
		for(k in 1:nrow(kecamatan)){
			link = read_html(paste0("https://data.kpu.go.id/dps.php?cmd=select&grandparent=",city$city_id[j], "&parent=", kecamatan$kecamatan_id[k]))
			kelurahan_id = link %>% html_nodes(xpath =  '//*[@name="wilayah_id"]/option') %>% html_attr("value")
			kelurahan = cbind(kelurahan_id, kelurahan = link %>% html_nodes(xpath =  '//*[@name="wilayah_id"]/option') %>% html_text()) %>% data.frame() %>% filter(kelurahan_id!="")
			
			for(l in 1:nrow(kelurahan)){
				link = read_html(paste0("https://data.kpu.go.id/dps.php?cmd=select&grandparent=",kecamatan$kecamatan_id[k], "&parent=", kelurahan$kelurahan_id[l]))
				tps_id = link %>% html_nodes("span select option") %>% html_attr("value")
				
				for(m in 1:length(tps_id)){
				  link = read_html(paste0("https://data.kpu.go.id/dps.php?cmd=Filter&column=filterTPS_new&filter=", tps_id[m], "&parent=", kelurahan$kelurahan_id[l], "&grandparent=",kecamatan$kecamatan_id[k]))
				  possibleError = tryCatch({
				    dat = link %>% html_node('#daftartps > table') %>% html_table()},
					  error = function(e) e)
				  if(inherits(possibleError, "error")) next
				  dat = data.frame(dat)
					dat = dat[!is.na(dat$X2),]
					names(dat) = dat[1,]
					dat = dat[-1, 2:6]
					dat$kelurahan = kelurahan$kelurahan[l]
					dat$kecamatan = kecamatan$kecamatan[k]
					dat$city = city$city[j]
					message(paste0("provinsi ", provinsi$provinsi[i], " kota ", city$city[j], " kecamatan ", kecamatan$kecamatan[k], " kelurahan ", kelurahan$kelurahan[l], " tps ", tps_id[m]))
					ifelse(j==1 & k==1 & l==1 & m==1, dbWriteTable(con, "allname", dat), dbWriteTable(con, "allname", dat, append=T, overwrite=F))
		      message("written to database")
				
				}
			}
		}
	}
}
