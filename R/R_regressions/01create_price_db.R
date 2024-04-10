
#import sale data
price_data <- read_csv("data/ksou.csv")

price_data_sf <- price_data %>% 
  filter(!is.na(longitude)) %>% 
  st_as_sf(coords = c("longitude","latitude")) %>%
  st_set_crs("wgs84") %>%
  rename_all(~if_else(.x == "geometry","geometry",paste0("pp_",.x)))

st_write(obj = price_data_sf, 
         dsn = con, 
         Id(schema="public", 
            table = 'property_prices'),
         append = F)


#left join sale data with the existing database of dwellings. 

#POIS

dbExecute(con,"ALTER TABLE property_prices
  ALTER COLUMN geometry TYPE geometry(Geometry, 7844)
    USING ST_Transform(geometry, 7844);")

dbExecute(con,  "UPDATE property_prices
SET geometry = ST_MakeValid(geometry)
WHERE NOT ST_IsValid(geometry);")

dbExecute(con, 'CREATE INDEX ON property_prices USING GIST (geometry);')
dbExecute(con, 'CREATE INDEX ON dwelling_data_clean USING GIST (geometry);')

dbExecute(con, "ALTER TABLE property_prices
RENAME COLUMN geometry TO pp_geometry;")

dbExecute(con,"DROP TABLE IF EXISTS dwellings_with_prices")

sql_query <- "CREATE TABLE dwellings_with_prices AS
 SELECT d.*, pp.*
 FROM dwelling_data_clean d
 JOIN property_prices pp
 ON ST_Intersects(d.geometry, pp.pp_geometry)
 "
dbExecute(con, sql_query)
