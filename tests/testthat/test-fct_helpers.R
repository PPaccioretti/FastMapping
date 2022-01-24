test_that("Print sf as df", {
  
  
  pt1 = sf::st_point(c(181072, 333611))
  pt2 = sf::st_point(c(181025, 333558))
  df = data.frame(a = 1:2, b = c(2.4,1.9))
  df$geometry = sf::st_sfc(pt1, pt2)
  df_sf = sf::st_as_sf(df, crs = 28992)
  
  expect_equal(print_sf_as_df(df_sf)$data,
               data.frame(sf::st_drop_geometry(df_sf)))
  
  expect_equal(print_sf_as_df(df_sf)$geometry,
               sf::st_geometry(df_sf))
})
