#генерация на заданное количество максимально различные друг от друга цвета. Можно также подкрутить яркость


generate_hue_rainbow <- function(n, start = sample(1:360, 1))
{
  h <- sapply (c(0:(n - 1)), function(x)
  {
    hue <- round(start + x * (360 / (n)))
    if (hue >= 360)
      hue <- hue - 360
    hue
  })
  h
}

hsl_to_rgb <- function(h, s, l) {
  h <- h / 360
  r <- g <- b <- 0.0
  if (s == 0) {
    r <- g <- b <- l
  } else {
    hue_to_rgb <- function(p, q, t) {
      if (t < 0) {
        t <- t + 1.0
      }
      if (t > 1) {
        t <- t - 1.0
      }
      if (t < 1 / 6) {
        return(p + (q - p) * 6.0 * t)
      }
      if (t < 1 / 2) {
        return(q)
      }
      if (t < 2 / 3) {
        return(p + ((q - p) * ((2 / 3) - t) * 6))
      }
      return(p)
    }
    q <- ifelse(l < 0.5, l * (1.0 + s), l + s - (l * s))
    p <- 2.0 * l - q
    r <- hue_to_rgb(p, q, h + 1 / 3)
    g <- hue_to_rgb(p, q, h)
    b <- hue_to_rgb(p, q, h - 1 / 3)
  }
  return(rgb(r, g, b))
}

generate_rgb_rainbow <-
  function(n,
           s = 0.6,
           l = 0.5,
           random_order = T,
           start = F)
  {
    if (start != F)
      colors <-
        generate_hue_rainbow(n, start)
    else
      colors <- generate_hue_rainbow(n)
    colors <- sapply(colors, function(x)
      hsl_to_rgb(h = x, s = s, l = l))
    if (random_order)
      colors <- sample(colors, n)
    colors
  }

generate_rgb_rainbow(5, random_order = F, start=2)

#s - saturation, l - luminocity, h - hue
