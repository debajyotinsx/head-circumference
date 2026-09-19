# WHO Head Circumference Tracker

The app calculates the growth curves, Z-scores and percentiles using WHO reference data. Measurements are held in the current session, not saved to a database. Refreshing closes that session.

## Build locally

```r
install.packages("shinylive")
shinylive::export(".", "site")
```

Run this from the folder containing `app.R` and both CSV files. Serve `site` over HTTP for testing; opening `index.html` directly as a file does not work.


