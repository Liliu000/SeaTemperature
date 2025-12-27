# SeaTemp

Analysis of Mediterranean sea temperature data from Estartit observation point (2000-2017).

**All data is included in the package** - no external files needed!

  ## Installation
  ```r
# From GitHub
devtools::install_github("yourusername/SeaTemp")

# Or from local file
install.packages("SeaTemp_0.1.0.tar.gz", repos = NULL, type = "source")
```

## Quick Start
```r
library(SeaTemp)

# Data is already loaded - just use it!
data(sea.deep)
head(sea.deep)

# Calculate month-to-month changes
plot_delta_by_year(years = c(2000, 2010, 2017))
plot_delta_mean(depths = c(0, -80))

# Calculate and plot anomalies
plot_anom_by_year(years = c(2000, 2010, 2017))
plot_anom_mean(depths = c(0, -80))

# Project future trends
trends <- fit_anom_linear_by_depth()
print(trends)

projections <- project_anom_linear(years_future = 2018:2030)
plot_anom_projection()
```

## Included Data

- **sea.deep**: Monthly temperatures at 4 depths (2000-2017)
- **baseline_1974_1999**: Climatological means (1974-1999)

## Functions

### Temperature Changes
- `calculate_month_deltas()` - Month-to-month changes
- `calculate_delta_mean()` - Average changes across years
- `plot_delta_by_year()` - Visualize by year
- `plot_delta_mean()` - Visualize averages

### Anomalies
- `calculate_anomalies()` - Anomalies vs baseline
- `calculate_anomaly_mean()` - Average anomalies
- `plot_anom_by_year()` - Visualize by year
- `plot_anom_mean()` - Visualize averages

### Projections (Optional)
- `fit_anom_linear_by_depth()` - Linear trends
- `project_anom_linear()` - Future projections
- `plot_anom_projection()` - Visualize projections

## Citation
Carranza, P., Inglada, A., & Martínez, L. (2025). SeaTemp: Analysis of
Mediterranean Sea Temperature Data. R package version 0.1.0.

## Data Source

Institut d'Estadística de Catalunya (IDESCAT)  
https://www.idescat.cat/pub/?id=aec&n=218&t=2000

## License

MIT License

## Authors

Pere Carranza, Ares Inglada, Liliu Martínez
