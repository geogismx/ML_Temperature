# ML_Temperature

Specific machine learning methods were developed and linked with target‐oriented validation and then applied to convert LST to Tair. Spatial variables in retrieving Tair, such as solar radiation and vegetation indices, were used in estimation of Tair, whereas MODIS LST products were mainly focused on temporal variation in surface air temperature.

![ML](https://github.com/geogismx/ML_Temperature/blob/master/ML.png)

We perform a target‐oriented validation which validates the model with a view towards spatial mapping. To this end, we repeatedly leave the complete time series of one or more data loggers out, and use them as test data during CV. This study will use the following two steps to identify and avoid overfitting.
1. To compare Machine Learning methods with different validation strategies using 10‐fold Leave‐Location‐Time‐Out (LLTO), Leave‐Location‐Out (LLO) and Leave‐Time‐Out (LTO).
2. Using the best fitting model with suitable validation strategies to estimate monthly Tair products based on 10‐fold LLTO Cross‐Validation.

![ML_Rplot](https://github.com/geogismx/ML_Temperature/blob/master/Rplot_RMSE_FL.png)

Monthly Tmean based on RF model in 2003–2013

![ML_Temp](https://github.com/geogismx/ML_Temperature/blob/master/temperature.png)
