test_that("Spatial Transformation Works for DF", {

  pt1 = sf::st_point(c(181072, 333611))
  pt2 = sf::st_point(c(181025, 333558))
  df = data.frame(a = 1:2, b = c(2.4,1.9))
  df$geometry = sf::st_sfc(pt1, pt2)
  df_sf = sf::st_as_sf(df, crs = 28992)
  
  sf_df <- data.frame(
    sf::st_drop_geometry(df_sf),
    sf::st_coordinates(df_sf))
  df_sf_transf <- sf::st_transform(df_sf, 4326)
  myData <- 
    spatial_transformation(sf_df, 
                           c("X", "Y"),
                           28992,
                           4326)
  
  
  expect_equal(sf::st_crs(myData), sf::st_crs(4326))
  expect_s3_class(myData, "sf")
  expect_equal(myData, df_sf_transf)
  expect_error(spatial_transformation(sf_df,
                                      c("X", "Y")),
               "Original CRS must be specified")
  
})



test_that("Spatial Transformation Works for SF", {
  
  pt1 = sf::st_point(c(181072, 333611))
  pt2 = sf::st_point(c(181025, 333558))
  df = data.frame(a = 1:2, b = c(2.4,1.9))
  df$geometry = sf::st_sfc(pt1, pt2)
  df_sf = sf::st_as_sf(df, crs = 28992)
  
  df_sf_t <- sf::st_transform(df_sf, 4326)
  myData <-
    spatial_transformation(df_sf,
                           tgt_epsg = 4326)
  
  
  expect_equal(sf::st_crs(myData), sf::st_crs(df_sf_t))
  expect_s3_class(myData, "sf")
  expect_equal(myData, df_sf_t)
  
})


