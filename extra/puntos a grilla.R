library(sf)

layer <- st_layers(system.file("gpkg/nc.gpkg", package = "sf"))$name[1]
nc_gpkg_sql = st_read(system.file("gpkg/nc.gpkg", package = "sf"),
                      query = sprintf("SELECT NAME, SID74, FIPS, geom  FROM \"%s\" WHERE BIR74 > 20000", layer))
data(wheat, package = "paar")
wheat <- st_as_sf(wheat, coords =  c(1:2), crs = 32720)

st_nearest_points()
data <- cut(wheat[,1], mean(wheat[,1]))
plot(data[sample(nrow(data), 10), ])
plot(data, add = T)
st_distance(data)
which.max(table(df1$Factor_1))


plot(st_make_grid(data, n = 10))
plot(data, add = T)
aggregate(data, st_make_grid(data, n = 10), FUN = length)

misdatitos <- sample(nrow(data), 10)

sapply(misdatitos, function(x) {
  st_distance(data[x,], 
              data[st_nearest_feature(data[x,], data[-x,]),], 
              by_element=TRUE)
})

plot(data[misdatitos,])
dist = st_distance(data[1,], 
                   data[st_nearest_feature(data[-1,]),], 
                   by_element=TRUE)


datadeso <- data[sample(nrow(data)), ]
sort(datadeso$geometry)
dist = st_distance(datadeso[1:10,], 
                   datadeso[st_nearest_feature(datadeso[1:10,]),], 
                   by_element=TRUE)


coords <- st_coordinates(st_centroid(data))
data_sort = data[order(coords[,"X"], coords[,"Y"]),]
dist = st_distance(data_sort[1:10,], 
                   data_sort[st_nearest_feature(data_sort[1:10,]),], 
                   by_element=TRUE)

cellsize = round(median(dist), 0)
dataGrid <- st_make_grid(data, cellsize = cellsize, what = "centers") 
dataGrid <- st_make_grid(dataGrid, cellsize = cellsize)
plot(dataGrid, col = 'red', add = T)
# dataGrid <- dataGrid |> tibble::as_tibble() |> st_as_sf()
# plot(dataGrid)

dataGrid <- aggregate(data, dataGrid, FUN = mode)

dataGrid[!is.na(dataGrid[[1]]),]
plot(dataGrid[!is.na(dataGrid[[1]]),], add = T)
dataGrid <- tibble::as_tibble(dataGrid)
plot(dataGrid)
dataGrid <- dataGrid[!is.na(dataGrid$grid),]
dataGrid$area_sqm = st_area(dataGrid$geometry)
dataGrid$area_sqkm = as.numeric(unlist(dataGrid$area_sqm * 10^-6))
dataGrid$area_deficit = (12*12) - dataGrid$area_sqkm

plot(st_as_sf(dataGrid))
