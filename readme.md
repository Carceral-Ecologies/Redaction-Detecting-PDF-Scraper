# Redaction Detecting PDF Text Scraper

Structured text extraction from image-only PDFs using R and the ExtractTable API, detecting and inserting redactions within the text.

This program is built in particular for the format of the LAPD Air Support Division daily log format. It cannot be used with other formats of files without significant modification, although the code structure could be used as useful reference for building a similar PDF scraper for another file format.

## Running

### Quick start

This is a brief overview of getting the program running. For more info, see the below sections.
 
 - Install [R and RStudio](https://posit.co/download/rstudio-desktop/)
 - Install the pdftools, magick, tidyverse, and tesseract R packages using `install.packages()` in RStudio console
 - Install [ImageMagick](https://imagemagick.org/script/download.php) on your command line
 - Acquire an [ExtractTable](https://extracttable.com/) "Extra" API key and put it in the `extracttable_api_key` variable
 - Set the config options at the top of `src/main.R` and run the whole file
 - Copy your `logs` variable to the name you'd like (e.g., `logs <- logs_sept_20`)
 - Set the config options at the top of `src/toCSV.R` and run the whole file

### Dependencies

This is an R program; running it first requires installing [R and RStudio](https://posit.co/download/rstudio-desktop/).

After opening RStudio, open the console and install the required dependencies:

-   [pdftools](https://cran.r-project.org/web/packages/pdftools/index.html) to get PDF metadata
-   [magick](https://cran.r-project.org/web/packages/magick/vignettes/intro.html) for image manipulation (primarily cropping)
-   [tidyverse](https://www.tidyverse.org/) for common R syntax
-   [tesseract](https://cran.r-project.org/web/packages/tesseract/index.html) for local OCR processing

This can be accomplished by running the following command in the console:

```         
install.packages(c('pdftools', 'magick', 'tidyverse', 'tesseract'))
```

For in-text redaction detection (detecting black boxes), this R script uses the executable version of ImageMagick as it has more features than the R plugin. As such, you must have [ImageMagick installed](https://imagemagick.org/script/download.php)
 - You can verify whether ImageMagick is installed by running `magick -version` in your terminal. This should result in an info screen about the version installed, not a "command not found" error.
 - On macOS, the easiest method of installation is to install [Homebrew](https://brew.sh/) and then run `brew install magick` in a terminal.

Finally, this program uses an online web service/API called [ExtractTable](https://extracttable.com/) to recognize and extract structured table data from images, with built-in table detection and higher text recognition accuracy than locally. Acquire an API key for "Extra" credits -- you will need roughly 1.3x as many credits as pages of PDF you plan to extract data from.

### Configuration

## Design

### File structure

### Quantifying redactions
