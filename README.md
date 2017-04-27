Intall from Github:

```r
devtools::install_github("Jean-Romain/RingsDetection", dependencies=TRUE)
```

Usage:

```r
data(oakprofile)
result = with(oakprofile, ring_detection_UI(rad_pos, density))
```

![](https://raw.githubusercontent.com/Jean-Romain/RingsDetection/master/img/screen-v0.10.png)
