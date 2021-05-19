#ACCESS DATA BASE AND EXPORT DEPTH MEASURED BY ECHOSOUNDING
#AGGREGATE AT EVERY 10 PING

rm(list=ls())
library(RPostgreSQL)
library(dplyr)

#setting the connection with the database
LSP <- dbConnect(dbDriver("PostgreSQL"), dbname = "LSP",
                 host = "localhost", port = 5432,
                 user = "Morgan", password = {"new_user_password"})

#Get echo data
echo <- dbGetQuery(LSP, 
"SELECT campagne, ST_X(geom_utm)AS x_utm, ST_Y(geom_utm) AS y_utm,
timestamp, transect, report_no, secteur, depth_m
FROM
(SELECT * FROM coord_spatiale WHERE campagne LIKE 'GE%')A
JOIN
(SELECT id_spatial, valeur AS depth_m FROM mesure_spatiale WHERE mesure = 'profondeur')B
ON A.id_spatial = B.id_spatial
ORDER BY campagne, timestamp")

#Mean by transect and report_no (20 pings)
#Create and index to summarise per transect and ping (report number)
report <- split(echo, echo$transect)
for(i in 1:length(report)){
  report[[i]]$ag.ping <- rep(seq(min(report[[i]]$report_no),
                                 max(report[[i]]$report_no),
                                 20),
                             each = 2)[1:nrow(report[[i]])]
}
echo <- plyr::rbind.fill(report)

echo.20 <- echo %>% 
  group_by(campagne, transect, secteur, ag.ping) %>% 
  summarise(x.utm = mean(x_utm), y.utm = mean(y_utm),
            time = mean(timestamp), depth_m = mean(depth_m))


#Export data
write.csv(echo.20, "data/raw/echo_20pings_2012-2017", fileEncoding = 'UTF8', row.names = F)


