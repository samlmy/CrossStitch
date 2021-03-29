# CrossStitch
The R functions take a JPEG or PNG file and create a cross-stitch pattern that will include thread colour. The project utilizes k-means clustering to help you to decide how many thread color you should use for you cross stitich and you can avoid using similar color threads that are not necessary.

By giving the image file and a list of numbers, function process_image will output the information other functions need. 
Function scree_plot visualize the information which indicate the optimizing number of thread colors to use for the cross-stitch pattern. 
Function colour_strips simply finds the most similar DMC colors to the original color in the picture.
The last step is to produce your cross-stitch pattern with make_pattern. You will need to give the funtion the output from process_image, the number of thread colors you eventually decided, and the size of the total number of possible stitches in the horizontal direction.
