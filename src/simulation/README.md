Evaluation metrics used in the simulation

| Metric Name       | Description                      | R Formula                                                   |
|-------------------|----------------------------------|-------------------------------------------------------------|
| `Er_mean`         | Mean Error                       | `mean(error, na.rm = TRUE)`                                 |
| `Er_mean_rel`     | Mean Relative Error              | `mean(error, na.rm = TRUE) / mean(reference, na.rm = TRUE)` |
| `flux_mean`       | Mean of Reference                | `mean(reference, na.rm = TRUE)`                             |
| `flux_sum`        | Sum of Reference                 | `sum(reference, na.rm = TRUE)`                              |
| `flux_sd`         | Standard Deviation of Reference  | `sd(reference, na.rm = TRUE)`                               |
| `Er_sum`          | Sum of Error                     | `sum(error, na.rm = TRUE)`                                  |
| `MAE`             | Mean Absolute Error              | `mean(abs(error), na.rm = TRUE)`                            |
| `MAE_rel`         | Mean Absolute Error Relative     | `mean(abs(error) / abs(reference), na.rm = TRUE)`           |
| `Er_sd`           | Standard Deviation of Error      | `sd(error, na.rm = TRUE)`                                   |
| `Er_rmse`         | Root Mean Square Error           | `sqrt(mean(error^2, na.rm = TRUE))`                         |
| `Er_intercept`    | Intercept from Linear Regression | `summary(lm(error ~ reference))$coefficients[1,1]`          |
| `Median_er_ratio` | Median of Error Ratio            | `median((reference - error) / reference, na.rm = TRUE)`     |
| `Er_slope`        | Slope from Linear Regression     | `summary(lm(error ~ reference))$coefficients[2,1]`          |
| `Er_slope1`       | Slope Coefficient through (0,0)  | `summary(lm(error ~ reference - 1))$coefficients[1,1]`      |
| `Er_slope1.p`     | P-value for `Er_slope1`          | `summary(lm(error ~ reference - 1))$coefficients[1,4]`      |
| `Er_intercept.p`  | P-value for `Er_intercept`       | `summary(lm(error ~ reference))$coefficients[1,4]`          |
| `Er_slope.p`      | P-value for `Er_slope`           | `summary(lm(error ~ reference))$coefficients[2,4]`          |
| `Er_intercept.se` | Standard Error of Intercept      | `summary(lm(error ~ reference))$coefficients[1,2]`          |
| `Er_slope.se`     | Standard Error of Slope          | `summary(lm(error ~ reference))$coefficients[2,2]`          |


## Metadata for perf_eval_all.csv


1. **Metric**: The name of the statistical metric used to evaluate the
performance or error. Examples include mean error, mean relative error, standard
deviation, etc.

2. **QEA**: Represents the results from the "QEA" method. 

3. **QEA0**, **REA-BTS**, **REA-LDB-BTS**, **REA-LDB-BCST**, **REA-LDB-BW**,
**REA-DB1-BTS**, **REA-DB1-BCST**, **REA-DB1-BW**: represent
different variants of relaxed eddy accumulation. Each column holds the metric values
calculated for that particular method.

4. **QEA0_rel**, **REA-BTS_rel**, **REA-LDB-BTS_rel**, **REA-LDB-BCST_rel**,
**REA-LDB-BW_rel**, **REA-DB1-BTS_rel**, **REA-DB1-BCST_rel**,
**REA-DB1-BW_rel**: These columns show the relative performance of each method
compared to the "QEA" method. They represent the ratio of 
improvement or degradation of the metric relative to the "QEA" method.

5. **avg_improvement**: This column represents the average improvement across
selected methods ("REA-LDB-BCST", "REA-LDB-BW", "REA-DB1-BW") for each
metric. It's calculated as the mean of the relative performance columns for
these methods.

6. **scalar**: Indicates the scalar for the metrics, such as "co2", "h2o", "ts".

