# WHO Head Circumference Tracker

Browser-based version of the supplied Shiny app for ages 0–5 years. Built with Shinylive for free GitHub Pages hosting.

The app reads the two supplied reference CSV files and calculates the growth curves, Z-scores and percentiles using the original R code. Measurements are held in the current session, not saved to a database. Refreshing closes that session.

## Build locally

```r
install.packages("shinylive")
shinylive::export(".", "site")
```

Run this from the folder containing `app.R` and both CSV files. Serve `site` over HTTP for testing; opening `index.html` directly as a file does not work.

## Hosting

GitHub Pages serves the exported static website. The R calculations run in the visitor's browser. The public repository and exported website expose the app source and bundled reference CSVs.

Do not commit patient records or credentials. This repository contains reference data only.
