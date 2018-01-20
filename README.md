
uslides
-------

Rmarkdown template for pretty university-themed beamer (pdf) presentations.

Installation
------------

``` r
## install devtools if it's not already installed
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")

## install uslides from github
devtools::install_github("mkearney/uslides")
```

Requirements
------------

-   The fonts [Roboto Condensed](https://fonts.google.com/specimen/Roboto+Condensed), [Roboto Bold](https://fonts.google.com/specimen/Roboto), and [Inconsolata](https://fonts.google.com/specimen/Inconsolata) must be downloaded from [fonts.google.com](https://fonts.google.com/).
-   [\\LaTeX](https://www.latex-project.org/get/) (and the `tex_engine` is `xelatex` if that matters)

Example
-------

See an [example Rmd file](tools/readme/example.Rmd) and its [pdf output](tools/readme/example.pdf). Screen shots of the example are included below.

### uslides .Rmd template

<center>
<img src="tools/readme/ss4.png" width="70%" />
</center>

<style>
img {box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);}
</style>
 

### Title slide

<center>
<img src="tools/readme/ss1.png" width="70%" />
</center>
 

### Multi-level bullet points

<center>
<img src="tools/readme/ss2.png" width="70%" />
</center>
 

### Display R code and output

<center>
<img src="tools/readme/ss3.png" width="70%" />
</center>
