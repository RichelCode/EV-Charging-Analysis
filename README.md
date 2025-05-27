#  Traffic and EV Charging Demand Forecasting using Time Series and Deep Learning

This project explores the intersection of traffic forecasting and electric vehicle (EV) charging demand using time series analysis, deep learning, and hierarchical modeling techniques. The focus is on leveraging real-world traffic data and EV usage patterns to build intelligent systems for **path planning**, **demand prediction**, and **infrastructure optimization**.

---

## Project Objectives

### 1. **Path Planning Based on Traffic Flow Forecasting**
- Forecast hourly traffic volume using statistical and deep learning models.
- Enable congestion-aware routing by predicting future flow dynamics.

### 2. **Forecasting EV Charging Demand at Different Aggregation Levels**
- Use traffic and EV datasets to model charging behavior.
- Predict demand at station, city, or regional levels using advanced time series and graph-based methods.

---

##  Datasets

| Dataset | Description | Source |
|--------|-------------|--------|
| **PeMS (District 3)** | 3 months of hourly traffic data (flow, speed, occupancy) | [PeMS Website](https://pems.dot.ca.gov) |

---

##  Methods & Techniques

###  Forecasting Models

We compare **9 models** across four categories:

| Category                    | Models |
|----------------------------|--------|
| **Baseline Models**        | Linear Regression, LSTM-only |
| **Published Hybrid Model** | CNN-GRU-LSTM (reproduced from literature) |
| **Graph-based Models**     | DCRNN-only, STGCN-only, GraphWaveNet-only |
| **Proposed Hybrid Models** | DCRNN-GRU-LSTM, STGCN-GRU-LSTM, GraphWaveNet-GRU-LSTM |

---

### Model Highlights

- **DCRNN**: Captures spatial diffusion using directed graphs + temporal learning.
- **STGCN**: Combines Chebyshev graph convolutions with temporal convolutions.
- **GraphWaveNet**: Learns graph structure dynamically using adaptive adjacency + dilated convolutions.
- **Hybrid Models**: Fuse graph-based spatial layers with GRU + LSTM for robust spatio-temporal forecasting.

---

##  Forecasting Horizon

Each model is trained to forecast:

- The next **24 hours**
- The next **72 hours**

This allows both short-term and medium-term traffic prediction, useful for planning and EV charging coordination.

---

## Evaluation Metrics

We evaluate all models using the same test data and the following metrics:

- **MAPE (Mean Absolute Percentage Error)** – interpretable percentage error.
- **RMSE (Root Mean Squared Error)** – penalizes large errors.
- **MAE (Mean Absolute Error)** – stable and interpretable.

---

## Model Selection Criteria

The final model will be selected based on:

1. **Accuracy**: Lowest RMSE, MAE, and MAPE.
2. **Spatial Consistency**: Stable station-level performance.
3. **Generalization**: Minimal overfitting (low val-test gap).
4. **Efficiency** *(if tied)*: Preference given to faster or lighter model for integration into the EV simulation phase.

---

##  Visualizations

The repository includes:

- Time series plots of hourly, daily, weekly trends
- Clustered traffic groups (Coefficient of Variation)
- Station-by-station forecasting error plots
- Predicted vs actual plots for all models

---

##  Contributions & Feedback

If you have suggestions, resources to share, or want to collaborate—please feel free to [open an issue](https://github.com/) or contact me directly!

---

