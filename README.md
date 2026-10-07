# MOSSI: Modelling Britain's electricity generation costs 2010 to 2025

The Merit Order Stack model and datasets used to estimate the short-run marginal cost (SRMC) of generating Britain's electricity from 2010 to 2025. The code sets up a fleet of power stations, dispatches them against half-hourly demand, and compares the resulting costs with observed day-ahead electricity prices.

*Report citation to be added later.*

## Download and run

You need [R](https://cran.r-project.org/) or [RStudio](https://posit.co/download/rstudio-desktop/) if you prefer it.  No other dependencies are required.

After you have downloaded or cloned this repository, you can run the code in three ways:

**With RStudio:**
1. Open `MOSSI.Rproj` in RStudio.
2. Run this in the R console:

   ```r
   source("model_gb_power_system.r", encoding = "UTF-8")
   ```

**With RScript:**

Open a terminal in the extracted folder and run:

```sh
Rscript --vanilla model_gb_power_system.r
```

Or for a short demonstration:

```sh
Rscript --vanilla model_gb_power_system.r --quick
```


**With R GUI Console:**

Run the line of code below in the console, changing the path to where you downloaded the package.

```r
source("/path/to/download/model_gb_power_system.r", encoding = "UTF-8")
```

On Windows, use forward slashes in R paths, for example `C:/Users/istaffell/Downloads/MOSSI/model_gb_power_system.r`



The default run covers 1 January 2010 to 31 December 2025. When it finishes, open `outputs/figures.pdf` to see the charts. Results and summary data are saved alongside it. Allow a few minutes for the full run to compute.

If you want a short first run to check things work, edit `model_gb_power_system.r` and set `quick_run = TRUE`. This runs a single month and writes to `outputs/quick-run/`. Set it back to `FALSE` to reproduce the full analysis.
