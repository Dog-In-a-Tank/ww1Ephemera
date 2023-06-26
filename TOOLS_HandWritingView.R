library(magick)

path = "./HAMPTON - CHESHIRE-270223/041.jpeg"
img <- image_read(path)

a <-image_modulate(img, brightness = 60, saturation = 100, hue = 90)
b <-image_quantize(img, max = 5, colorspace = "rgb")
c <-image_contrast(img, sharpen = 100)
d <-image_equalize(img)
e <-image_despeckle(a, times = 2L)
f <-img%>%image_negate()
g <- image_canny(img, geometry = "0x1+5%+30%")
h <-img%>%image_fill("yellow", fuzz = 15)
combined <-c( a, b, c ,d,e,f,g,h )
print(h)

x <- image_montage(combined,tile = "4x2",geometry = "1800")
print(x)


     















print(g)
image_write(a, path = 'out/a.png', format = 'png')
image_write(b, path = 'out/b.png', format = 'png')
image_write(c, path = 'out/c.png', format = 'png')
image_write(d, path = 'out/d.png', format = 'png')
image_write(e, path = 'out/e.png', format = 'png')
image_write(f, path = 'out/f.png', format = 'png')
image_write(g, path = 'out/g.png', format = 'png')
image_write(h, path = 'out/h.png', format = 'png')


combined <-c( a, b, c ,d,e,f,g,h )
x <- image_montage(combined,tile = "4x2",geometry = "1800")
image_write(x, path = 'out/x.png', format = 'png')            

