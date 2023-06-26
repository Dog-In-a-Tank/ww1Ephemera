library(magick)

a = image_read("m1.png")
b = image_read("m2.png")
c = image_read("m3.png")
d = image_read("m4.png")
combined <-c( a, b, c ,d)
x <- image_montage(combined,tile = "2x2",geometry = "800")

print(x)
