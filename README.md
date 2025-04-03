# Traffic and EV Charging Demand Forecasting using Time Series and Deep Learning

This project explores the intersection of traffic forecasting and electric vehicle (EV) charging demand using time series analysis, deep learning, and hierarchical modeling techniques. The focus is on leveraging real-world traffic data and EV usage patterns to build intelligent systems for **path planning**, **demand prediction**, and **infrastructure optimization**.

---

##  Project Objectives

### 1. **Path Planning Based on Traffic Flow Forecasting**
- Forecast hourly traffic volume using statistical and deep learning models.
- Enable congestion-aware routing by predicting future flow.

### 2. **Forecasting EV Charging Demand at Different Aggregation Levels**
- Use traffic and EV datasets to model charging behavior.
- Predict charging demand at the station, city, or regional level using advanced time series methods.

---

## Datasets

| Dataset | Description | Source |
|--------|-------------|--------|
| **PeMS (District 3)** | 3 months of hourly traffic data (flow, speed, occupancy) | [PeMS Website](https://pems.dot.ca.gov) |

---

##  Methods & Techniques

- **Exploratory Data Analysis (EDA)**
- **Time-Based Feature Engineering**
  - Hour of day, day of week, holiday indicators, rolling averages, lag features
- **Statistical Forecasting Models**
  - SARIMA, Exponential Smoothing, Naive models
- **Hierarchical Time Series Forecasting**
  - Forecasts at multiple levels (sensor, road segment, district)
- **Deep Learning**
  - LSTM, GRU, Transformer models
- **Graph-Based Forecasting**
  - Future work using GNNs for networked time series
- **Spectral Clustering & Aggregation Techniques** for EV demand

---

## Visualizations

The repository includes plots to show:
- Hourly, daily, weekly traffic flow trends
- Clustered time series groups using Coefficient of Variation
- Forecasting performance comparisons

---

## Key Readings & References

Some of the core resources guiding the project:
- [DeepHGNN: GNN Forecasting for Hierarchical Time Series](https://arxiv.org/pdf/2405.18693)
- [Probabilistic Forecast of EV Charging Demand](https://energyinformatics.springeropen.com/articles/10.1186/s42162-024-00319-1)
- [Traffic Flow Forecasting with DNNs](https://arxiv.org/pdf/1703.07015)
- [PeMS User Guide](https://pems.dot.ca.gov/Papers/PeMS_Intro_User_Guide_v6.pdf)

---

## Contributions & Feedback

If you have suggestions, papers to recommend, or are interested in collaborating—feel free to open an issue or reach out!





