# 📊 Regression Model Selection in R

This project presents a comparative analysis of four linear regression strategies to identify the best predictive model for a given dataset. It was developed as part of a university course project in Data Science (Master's Degree in Computer Engineering at University of Salerno), and demonstrates the ability to apply advanced regression techniques using R.

> This demonstrates proficiency in R programming, statistical analysis, and reproducible research workflows.

📂 This project is part of a series of regression analysis case studies. For another similar project, see [linear-regression-from-scratch-R](https://github.com/francescopiocirillo/linear-regression-from-scratch-R).

## 📌 Objective

Given a dataset with 100 observations, one dependent variable `Y`, and 25 potential predictors `X1, X2, ..., X25`, the goal is to:

1. Identify linear models that minimize the Mean Squared Error (MSE) using different model selection techniques.
2. Evaluate the out-of-sample (test set) performance of each model and determine the most effective strategy.

## 🌍 Language Note

All code comments and the report are written in Italian, as this project was originally developed in an academic setting in Italy. Nonetheless, the structure, organization, and methodology follow international best practices in data science and statistical modeling.

## 🛠️ Techniques Applied

Four model selection techniques were implemented and compared:

- **Best Subset Selection (BSS)** using Bayesian Information Criterion (BIC)
- **Backward Stepwise Selection** with 5-fold Cross Validation
- **Ridge Regression** with optimal λ via cross-validation
- **Lasso Regression** with optimal λ via cross-validation

## 📈 Methodology Overview

- The dataset was split into training (70%) and test (30%) sets.
- Preliminary analysis included:
  - Correlation analysis between predictors and the response.
  - Multicollinearity assessment using VIF (Variance Inflation Factor).
- Each model selection strategy was implemented on the training set.
- MSE was computed on the test set to compare model performance.
- Visualizations were created for:
  - Correlation matrices.
  - Variable selection paths for Ridge and Lasso.
  - BIC and cross-validation error curves.

## 🧪 Results

- **Best performing strategy:** Best Subset Selection (BIC-based), which selected 9 predictors.
- **Lowest test MSE:** Achieved by the BSS model.
- Lasso and Ridge performed well but did not outperform BSS, likely due to the presence of only a few strongly predictive variables and weak multicollinearity in the dataset.


## 📂 Repository Structure

```
📦 regression-model-comparison-R/
├── README.md
├── 📁 analysis/
│   └── script.R
├── 📁 data/
│   └── RegressionDSDA250130.csv
├── 📁 instructions/
│   ├── project_brief_ENGLISH.pdf
│   └── project_brief_ITALIAN.pdf
├── 📁 report/
│   └── report.pdf
├── .gitignore
└── LICENSE
```

## 📊 Technologies Used

- **Language:** R
- **Packages:** `leaps`, `glmnet`, `car`, `corrplot`, `boot`, `plotmo`

## 🎓 About the Project

This project was created as part of the *Data Science & Data Analysis* course (2024/2025) for the Master’s Degree in Computer Engineering at the **University of Salerno**.

## 📬 Contacts

✉️ Got feedback or want to contribute? Feel free to open an Issue or submit a Pull Request!

## 📈 SEO Tags

```
regression, model-selection, linear-regression, ridge-regression, lasso, best-subset-selection, stepwise-selection, cross-validation, R, data-science, statistics, predictive-modeling, machine-learning, feature-selection, multicollinearity, MSE, OLS, glmnet, BIC, AIC
```

## 📄 License

This project is licensed under the **MIT License**, a permissive open-source license that allows anyone to use, modify, and distribute the software freely, as long as credit is given and the original license is included.

> In plain terms: **use it, build on it, just don’t blame us if something breaks**.

> ⭐ Like what you see? Consider giving the project a star!

