
process_image <- function(image_file_name, k_list)
{
  ## process_image(cluster_info) finds the RGB value and the DMC color closest 
  ## to the cluster center, and performs k-means for every k in the k_list.
  ##
  ## Input:
  ## - image_file_name: The image file name. It should be a string, and it must be
  ##                    a JPEG or PNG image.
  ## - k_list:          A vector of the number of centers in the clustering.
  ##
  ## Output:
  ## - A list of k-means output, the original image data frame, and the tidies clusters with 
  ##   their associated RGB values and their nearest DMC thread colour hex value.
  ##
  ## Example:
  ##   k_list <- c(2:10)
  ##   cluster_info <- process_image("image_name.jpg", k_list)
  
  image <- load.image(image_file_name)
  
  tidy_dat <- as.data.frame(image, wide = "c") %>% rename(R = c.1, G = c.2, B = c.3) %>% mutate(hex = rgb(R, G, B))
  
  dat <- select(tidy_dat, c(-x, -y))
  
  kclusts <- tibble(k = k_list) %>%
    mutate(kclusts = map(k, ~kmeans(x = select(dat, c(R, G, B)), centers = .x, nstart = 4)),
           glanced = map(kclusts, glance), )
  
  info <- c()
  for (j in 1:length(k_list)) {
    cent <- tidy(kclusts$kclusts[[j]]) %>% mutate(hex = rgb(R, G, B))
    cent <- cent %>% mutate(threads = vector("character", length = length(cent$hex)))
    for (i in 1:length(cent$hex)){
      cent$threads[[i]] <- dmc(cent$hex[[i]])$hex
    }
    info <- append(info, cent)
  }
  
  cluster_info <- list(kclusts, tidy_dat, info)
  return(cluster_info)
}

scree_plot <- function(cluster_info)
{
  ## scree_plot(cluster_info) plots the variance against the number of the clusters.
  ##
  ## Input:
  ## - cluster_info: Output of process_image(image_file_name, k_list), it is a list
  ##                 containing k-means output for every k in the k_list, and a tidied
  ##                 data frame of RGB values and hex values associated with the clusters.
  ##
  ## Output:
  ## - A scree plot.
  ##
  ## Example:
  ##   k_list <- c(2:10)
  ##   cluster_info <- process_image("image_name.jpg", k_list)
  ##   Scree_plot(cluster_info)
  
  clusterings <- 
    cluster_info[[1]] %>% 
    unnest(cols = c(glanced))
  
  ggplot(clusterings,aes(k, tot.withinss))+
    geom_line()+
    geom_point()
}

colour_strips <- function(cluster_info)
{
  ## colour_strips(cluster_info) finds the DMC color closest to the cluster center
  ## and produces color strips with it.
  ##
  ## Input:
  ## - cluster_info: Output of process_image(image_file_name, k_list), it is a list
  ##                 containing k-means output for every k in the k_list, and a tidied
  ##                 data frame of RGB values and hex values associated with the clusters.
  ##
  ## Output:
  ## - Color strips with the DMC colour closest to the cluster center colour for
  ##   every choice of k in k_list from process_image() call which outputs cluster_info.
  ##
  ## Example:
  ##   k_list <- c(2:10)
  ##   cluster_info <- process_image("image_name.jpg", k_list)
  ##   color_strips(cluster_info)
  
  for (i in 1:length(cluster_info[[3]])){
    if (i%%8 == 0) {
      show_col(cluster_info[[3]][[i]])
    }
  }
  
}

make_pattern <- function(cluster_info, k, x_size, black_white = FALSE, background_colour = NULL)
{ 
  ## make_pattern(cluster_info, k, x_size, black_white = FALSE, background_colour = NULL)
  ## changes the resolution of the picture in cluster_info, and clusters the image by the 
  ## choice of k. It produces a pattern of colored shapes for cross-stitch. 
  ##
  ## Input:
  ## - cluster_info:      Output of process_image(image_file_name, k_list), it is a list
  ##                      containing k-means output for every k in the k_list, and a tidied
  ##                      data frame of RGB values and hex values associated with the clusters.
  ## - k:                 The number of clusters for k-means call.
  ## - x_size:            The number of cells in the x-direction for change_resolution(image_df,
  ##                      x_size) call.
  ## - black_white:       Outputs the pattern in black and white (TRUE) or color (FALSE, default).
  ## - background_colour: The colour of the background, which should not be stitched in the pattern. 
  ##                      (Default is to not have a colour).
  ##
  ## Output:
  ## - A cross-stitch pattern that can followed, complete with a legend that
  ##   has thread color, and a guild grid.
  ##
  ## Example:
  ##   k_list <- c(2:10)
  ##   cluster_info <- process_image("image_name.jpg", k_list)
  ##   make_pattern(cluster_info, 6, 47)
  
  img <- change_resolution(cluster_info[[2]], x_size)
  
  dat <- select(img, c(-x, -y, -hex))
  kclust <- tibble(kk = c(k)) %>% mutate(kclust = map(kk, ~kmeans(dat, centers = kk, nstart = 20)), 
                                         glanced = map(kclust, glance), 
                                         centre = map(kk, ~(tidy(kclust[[1]]) %>% 
                                                              mutate(col = rgb(R, G, B), 
                                                                     threads = vector("character", length = .x)))))
  
  for (i in 1:length(kclust$centre[[1]]$col)){
    kclust$centre[[1]]$threads[i] <- dmc(kclust$centre[[1]]$col[i])$hex
  }
  
  kclust$kclust[[1]] <- augment(kclust$kclust[[1]], img)
  
  img <- change_resolution(kclust$kclust[[1]], x_size)
  if (black_white) {
    img %>% ggplot(aes(x=x, y=y)) +
      geom_point(aes(shape = factor(.cluster))) +
      scale_fill_manual(values=sort(kclust$centre[[1]]$threads), guide="none") +
      scale_y_reverse()+ theme_void()
  } else {
    img %>% ggplot(aes(x=x, y=y)) +
      geom_point(aes(col = factor(.cluster), shape = factor(.cluster))) +
      scale_fill_manual(values = select(kclust$centre[[1]], threads) %>% deframe, 
                        label = select(kclust$kclust[[1]], .cluster) %>% deframe,) +
      theme(panel.background = element_rect(fill = background_colour, colour = background_colour)) +
      scale_y_reverse()+ theme_void()
      
  }
  
}

change_resolution <- function(image_df, x_size)
{
  ## change_resolution(image_df, x_size) subsamples an image to produce
  ## a lower resolution image. Any non-coordinate columns in the data
  ## frame are summarized with their most common value in the larger
  ## grid cell.
  ##
  ## Input:
  ## - image_df: A data frame in wide format. The x-coordinate column MUST
  ##             be named 'x' and the y-coordinate column MUST be named 'y'.
  ##             Further columns have no naming restrictions.
  ## - x_size:   The number of cells in the x-direction. The number of cells
  ##             in the vertical direction will be computed to maintain the 
  ##             perspective. There is no guarantee that the exact number
  ##             of cells in the x-direction is x_size
  ##
  ## Output:
  ## - A data frame with the same column names as image_df, but with fewer 
  ##   entries that corresponds to the reduced resolution image.
  ##
  ## Example:
  ##   library(imager)
  ##   library(dplyr)
  ##   fpath <- system.file('extdata/Leonardo_Birds.jpg',package='imager') 
  ##   im <- load.image(fpath)
  ##   im_dat<- as.data.frame(im,wide = "c") %>% rename(R = c.1, G = c.2, B = c.3) %>%
  ##            select(x,y,R,G,B)
  ##   agg_image <- change_resolution(im_dat, 50)
  
  if(!require(sp)) {
    stop("The sp packages must be installed. Run install.packages(\"sp\") and then try again.")
  }
  if(!require(dplyr)) {
    stop("The dplyr packages must be installed. Run install.packages(\"dplyr\") and then try again.")
  }
  
  sp_dat <- image_df 
  gridded(sp_dat) = ~x+y
  
  persp = (gridparameters(sp_dat)$cells.dim[2]/gridparameters(sp_dat)$cells.dim[1])
  y_size = floor(x_size*persp)
  orig_x_size = gridparameters(sp_dat)$cells.dim[1]
  orig_y_size = gridparameters(sp_dat)$cells.dim[2]
  
  x_res = ceiling(orig_x_size/x_size)
  y_res = ceiling(orig_y_size/y_size)
  
  gt = GridTopology(c(0.5,0.5), c(x_res, y_res),
                    c(floor(orig_x_size/x_res), floor(orig_y_size/y_res)))
  SG = SpatialGrid(gt)
  agg = aggregate(sp_dat, SG, function(x) names(which.max(table(x)))[1] )
  agg@grid@cellsize <- c(1,1)
  df <- agg %>% as.data.frame %>% rename(x = s1, y = s2)  %>% select(colnames(image_df))
  
  return(df)
  
}
