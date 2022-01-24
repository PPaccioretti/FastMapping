test_that("read_file_guessing() handles all input types", {
  # Create sample data
  
  pt1 = sf::st_point(c(0,1))
  pt2 = sf::st_point(c(1,1))
  df = data.frame(a = 1:2, b = c(2.4,1.9))
  d <- df
  d$geom = sf::st_sfc(pt1, pt2)
  df_sf = sf::st_as_sf(d, crs = sf::st_crs(4326))
  attr(df_sf$geom, 'crs')$input <- "WGS 84"
  path_sf <- tempfile(fileext = ".gpkg")
  
  path_csv <- tempfile(fileext = ".csv")
  # path_tsv_comma <- tempfile(fileext = ".txt")
  path_xlsx <- tempfile(fileext = ".xlsx")
  
  sf::write_sf(df_sf, path_sf)
  write.csv(df, path_csv, row.names = FALSE)
  # write.table(df, path_tsv_comma, sep = "\t", dec = ",", row.names = FALSE)
  openxlsx::write.xlsx(df, path_xlsx)
  
  expect_equal(read_file_guessing(path_csv), df)
  expect_equal(read_file_guessing(path_xlsx), df)
  expect_equal(read_file_guessing(path_sf), df_sf)
  expect_s3_class(read_file_guessing(path_sf), "sf")
  expect_error(read_file_guessing("asd"), "Invalid file")
})
