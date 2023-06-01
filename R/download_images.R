

library(tinyForestR); library(rgee)

tinyForestR::initialise_tf()

ee <- import("ee")
geemap <- import("geemap")

rgee::ee_Initialize(drive = TRUE
                    )

df <- get_tf_data()

df <- df |>
  mutate(tf_age = lubridate::today() - date)


ic <- ee$ImageCollection("COPERNICUS/S2_SR")
start_date <- "2020-04-01"
end_date <- "2023-03-31"
bounds <- ee$Geometry$Point(c(df$lon[1], df$lat[1]))$buffer(1000)
cloud <- 20

ic_filt <- ic$filterBounds(bounds)
ic_filt <- ic_filt$filterDate(start = start_date, opt_end = end_date)
ic_filt <- ic_filt$filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE', cloud))
ic_filt <- ic_filt$select("B8", "B5", "B4", "B3", "B2")

dates <- rgee::ee_get_date_ic(ic_filt)

# geemap$download_ee_image_collection(ic_filt, file_path, scale = 10)

rgee::ee_imagecollection_to_local(ic_filt, region = bounds, scale = 10, add_metadata = TRUE,quiet = FALSE 
                                  )



