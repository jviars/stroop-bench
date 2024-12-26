# Stroop-Bench

A psychological data visualization tool for analyzing cognitive bias data, specifically focusing on the Stroop Effect and Confirmation Bias.

## Getting Started

### Prerequisites
- R (version 3.6.0 or higher)
- RStudio (recommended)

### Required R Packages
The app will automatically install these if needed:
- shiny
- shinydashboard
- ggplot2
- DT
- tidyr
- reshape2

### Installation
1. Download `stroop_bench.R`
2. Open in RStudio
3. Click "Run App"

## Features

### 1. Stroop Effect Analysis
- Upload participant data
- Visualize response time differences
- Calculate statistical significance
- Determine effect size (Cohen's d)
- View detailed data tables

### 2. Confirmation Bias Analysis
- Upload belief and evidence rating data
- Calculate bias scores
- Visualize rating patterns
- Perform correlation analysis
- View detailed data tables

### 3. Data Generation
- Generate sample datasets
- Adjust sample size and bias strength
- Download generated data for testing
- Preview data before download

## Data Format Requirements

### Stroop Effect Data
CSV file must include columns:
- participant: Participant ID
- condition: "congruent" or "incongruent"
- response_time: Response time in milliseconds

### Confirmation Bias Data
CSV file must include columns:
- participant: Participant ID
- initial_belief: Initial belief rating (1-7 scale)
- evidence_type: "supporting" or "opposing"
- evidence_rating: Evidence rating (1-7 scale)

## Images
<img width="1366" alt="Screenshot 2024-12-25 at 10 14 08 PM" src="https://github.com/user-attachments/assets/e31ae477-64ff-4784-8994-011c77183410" />
<img width="1366" alt="Screenshot 2024-12-25 at 10 13 28 PM" src="https://github.com/user-attachments/assets/b37a866a-5884-42fc-a5b5-a107cf47e44a" />
<img width="1364" alt="Screenshot 2024-12-25 at 10 14 41 PM" src="https://github.com/user-attachments/assets/2b9d1889-e194-4000-aa6f-10244c52b2a6" />
<img width="1364" alt="Screenshot 2024-12-25 at 10 15 01 PM" src="https://github.com/user-attachments/assets/4d491f04-ba75-4ded-822b-e0ad89fd1d06" />



## Key Terms and Concepts

### Statistical Concepts
- **Cohen's d**: A measure of effect size that indicates the standardized difference between two means. Generally:
  - 0.2 = small effect
  - 0.5 = medium effect
  - 0.8 = large effect

- **T-test**: A statistical test used to determine if there is a significant difference between the means of two groups. The p-value indicates the probability that the observed difference occurred by chance.

### Psychological Concepts

- **Cognitive Bias**: A systematic pattern of deviation from norm or rationality in judgment. These mental shortcuts can lead to errors in memory, perception, and decision-making.

- **Stroop Effect**: A demonstration of interference in the reaction time of a task. When the name of a color (e.g., "blue") is printed in a different color (e.g., red), naming the physical color takes longer and is more prone to errors.
  - Congruent: Word matches color (e.g., "red" written in red)
  - Incongruent: Word differs from color (e.g., "red" written in blue)

- **Confirmation Bias**: The tendency to search for, interpret, and recall information in a way that confirms one's preexisting beliefs. Measured by:
  - Initial beliefs: Starting opinions on a topic
  - Evidence ratings: How convincing people find supporting vs opposing evidence

### Visualizations

- **Violin Plot** (Stroop Analysis): Shows the distribution of response times for congruent and incongruent conditions.
  - Width indicates frequency at each response time
  - Internal box plot shows median and quartiles
  - Wider sections represent more common values

- **Scatter Plot** (Confirmation Bias): Shows relationship between initial beliefs and evidence ratings.
  - Points represent individual ratings
  - Trend lines show overall patterns
  - Separation between supporting/opposing lines indicates bias strength

## Contact

Created by jviars for psychology data visualization.

## License

This project is licensed under the MIT License - see the LICENSE file for details.
