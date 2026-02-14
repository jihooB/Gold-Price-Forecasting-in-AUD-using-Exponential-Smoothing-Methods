Gold Price Forecasting in AUD using Exponential Smoothing Methods

This project aims to establish an optimal statistical model to forecast the price of gold in Australian Dollars (AUD) over a 12-month horizon. Using historical data since 1978, the research involves several key analytical stages:

Exploratory Data Analysis (EDA): Visualizing long-term trends, significant price accelerations (such as the 2005 ETF launch and the 2019 COVID-19 pandemic), and seasonal peaks occurring in the first quarter of each year due to cultural demands in China and India.

Data Pre-processing: Applying Box-Cox transformations to stabilize the high volatility of gold prices and linearize the trend for more accurate modeling.

Model Development: Comparing multiple forecasting models, including a Seasonal Naïve benchmark, manual ETS (AAA) and ETS (AAN) models, and an automated ETS (A, Ad, N) selection.

Model Selection: Evaluating models through residual diagnostics and time-series cross-validation. While the automatic model showed lower error measures, the ETS (AAA) model was selected as the Champion Model for its superior short-term accuracy and ability to account for seasonality.

Forecasting: Generating a 12-month point forecast and confidence intervals to assist stakeholders in budget planning and risk management, predicting an overall upward trend in gold prices through early 2026.
