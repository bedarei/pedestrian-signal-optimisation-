## Pedestrian Signal Optimisation — Discrete Event Simulation

A discrete-event simulation (DES) built in R to model and optimise 
pedestrian crossing signal timing at Pearse Street Station, Dublin.

### The Problem
The current signal gives pedestrians only 17 seconds of green time 
per 99-second cycle. During peak hours (lecture start/end times at 
Trinity College Dublin), this creates queues of 26+ people and average 
wait times of nearly 70 seconds — well above the recommended 30-second 
threshold (Living Streets, 2012).

### Approach
- Modelled the crossing as a queuing system with non-homogeneous Poisson arrivals
- Built a full event-driven DES engine from scratch in R (no simulation libraries)
- Ran 100 stochastic replications per scenario to account for randomness
- Tested green durations from 17s to 60s to find the optimal configuration

### Results

| Metric           | Baseline (17s green) | Optimised (26s green) |
|------------------|----------------------|-----------------------|
| Avg wait time    | 69.7s                | 29.8s ↓ 57%           |
| Avg queue length | 26.7                 | 12.35 ↓ 54%           |
| Utilisation (ρ)  | 1.000                | 0.654                 |

Improvement validated via two-sample t-test (p < 0.05).

### How to Run
Open `Simulation_t-test.R` in RStudio and run — no external packages required.

### Files
- `Simulation_t-test.R` — Simulation and statistical analysis
- `Report.pdf` — Project report including methodology and recommendations

### My Contributions
This was a group project (5 members). My role covered:
- Event logic design and DES architecture
- Results analysis and discussion
- Data visualisation and figures throughout the report
