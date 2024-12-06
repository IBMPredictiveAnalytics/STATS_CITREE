# Conditional and Model-Based Trees in SPSS

## Introduction
This procedure estimates classification and regression trees. Unlike the CHAID, CART, and QUEST algorithms available in the SPSS TREES procedure, **conditional inference trees** use statistical significance tests with multiple testing corrections to find splits. These trees are more stable, less prone to overfitting, and generalize better to new data.

Trees can grow large and difficult to interpret. This procedure simplifies tree visualization by displaying subtree outlines and suppressing unnecessary details, improving readability and display efficiency.

### Key Features:
- Two types of trees: **Conditional Inference Trees (CIT)** and **Model-Based Trees (MOB)**.
- No R knowledge required; fully integrated into SPSS procedures.
- Comprehensive support for tree visualization, subtree selection, and predictive modeling.
- Statistical methods implemented based on the work of **Torsten Hothorn**, **Kurt Hornik**, and **Achim Zeileis**.

## Conditional Inference Trees (CIT)
Conditional Inference Trees use a unified framework for conditional inference or permutation tests. Key aspects include:
1. **Significance-Based Splitting**: Splits are determined by adjusted p-values (e.g., Bonferroni or univariate).
2. **Stopping Criteria**: Based on `mincriterion` (e.g., a p-value threshold like 0.05).
3. **Bias Reduction**: Avoids selection bias by using univariate p-values.

This approach eliminates the need for post-pruning or cross-validation. It ensures right-sized trees by statistically determining splits based on data characteristics.

### Comparison with Traditional Trees
- **CIT**: Uses significance tests for splits.
- **Traditional Trees (e.g., CART)**: Maximizes impurity measures like Gini or entropy.

## Model-Based Trees (MOB)
Model-Based Trees extend regression and logistic regression models by segmenting data into subpopulations with statistically distinct relationships.

### Key Steps:
1. Fit a model (e.g., regression) to observations at the current node.
2. Test for parameter instability across partitioning variables.
3. Split the data based on the variable with the most significant instability.
4. Repeat the process for child nodes.

Supported models:
- **moblinear**: Linear regression.
- **moblogit**: Logistic regression (requires scale-level dependent variables).

## Using the Procedure
### Estimation
- Select target (dependent) variables.
- Choose partitioning variables (categorical or scale).
- Choose a tree type: `ctree`, `moblinear`, or `moblogistic`.

### Prediction
- Save and reuse models for predictions.
- Output prediction results in a new dataset with customizable result types (e.g., response values, class probabilities, or node numbers).

### Display Options
- Full tree or subtrees can be visualized.
- Adjust plot size, font, and details for readability.

## Statistical Options
### Ctree Models:
- Test types: `Bonferroni`, `univariate`, etc.
- Parameters like `mincriterion`, `maxdepth`, and `minsplit` for fine control.

### MOB Models:
- Adjust significance thresholds, tree depth, and post-pruning criteria (`AIC` or `BIC`).

## Vignettes
The Vignettes tab links to detailed documentation by the original R module authors. These resources provide advanced insights into the statistical algorithms.

## Example Output
The procedure can display:
- Full trees and subtrees.
- Summary statistics (e.g., means or modes at terminal nodes).
- Structural change test tables for all possible splits.

For example, the Titanic dataset is used to demonstrate the ctree model, where case weights adjust the passenger count to 2201.

## Saving and Sharing Models
Estimated models can be saved for later use or shared as R data files.

## Acknowledgments
This implementation builds upon the R modules created by:
- **Torsten Hothorn**
- **Kurt Hornik**
- **Achim Zeileis**

Their work ensures the statistical rigor and reliability of the conditional and model-based trees.

---

For detailed usage instructions and examples, consult the **Vignettes tab** within SPSS or visit the authors' documentation.

## Output Explanation
This ctree model output comes from the data on passengers on the Titanic, which sank in 1912 in the north Atlantic ocean after colliding with an iceberg.  The dataset has only 24 records, but the case weights bring the passenger count up to 2201.

<img width="456" alt="image" src="https://github.com/user-attachments/assets/01cad3f6-e681-4fe1-bfa9-9a4ca1698cbb">

## Figure 1: Tree of Survival Probability
- The tree display shows the breakdown of passengers by booking class, sex, and adult/child (Age).  The formula shows the dependent and independent variables.  Variables to the left of the ~ are dependent, and those to the right are independent.
- The tree is shown in an outline format.
- The number in [ ] identifies the node and the subtree starting at that position.  These numbers will appear in the tree plot and can be used for specifying subtree outlines and plot to display.
- The statistics for a categorical dependent variable in the err field show the percentage of cases classified incorrectly.  For a continuous node, the sum of squared errors would be shown.
- Terminal nodes are at the lowest level of the tree.  Inner nodes show the branches as the tree is traversed.
Here is the plot corresponding to the tree above.

<img width="488" alt="image" src="https://github.com/user-attachments/assets/8b0facf1-49e9-4292-8f17-a972105fc0f3">

 
## Figure 2 Plot of survival probability
It may be useful to plot subtrees 2 (third class) and 7 (class one) to accommodate the large plot. Alternatively, the plot can be specified to be larger.  The small plots in the terminal nodes show the proportion of the dependent variable, i.e., the survivor proportion.  The chart type in the terminal nodes depends on the properties of the dependent variable.
Here are the results for a moblinear model.  Using the employee data.sav file shipped with SPSS Statistics, it estimates the effect of education, treated as a scale variable, on salary taking account of job category, gender, and birth date (bdate), 
 
<img width="391" alt="image" src="https://github.com/user-attachments/assets/e028e39f-c7e0-4824-88b1-94eae0fe99b5">

## Figure 3 Tree of MOB estimates of education effect on salary
- The equation shows that the dependent variable, salary, is regressed on educ and partitioned on jobcat, bdate, and gender.  The variables to the right of the vertical bar define the partition while the variable to the left defines the regression equation.
- The equation variable coefficients are shown for each partition where they differ.  For managers, the educ coefficient is not significantly different for any subgroup; custodial has a negative coefficient different from the other groups, and for clerical, males have a bigger educ effect than females.
- The birth date variable value is shown as the SPSS numerical value of the date.  While it would not affect the tree calculation, transforming birth date into age in years before running the procedure would make the results more readable.
The plot of the tree also shows the date as its numerical value.  It includes small scatterplots of each subgroup with points, and a fit line.  The font size and plot size were increased to show everything without overlap.  The x axis covers the range of the educ variable for the entire sample.  
In summary, the story in this tree is that education has a large positive effect for managers as seen in the slope of the fit line, a small but negative effect for custodial staff, and, for clerical staff, an in-between effect on salary.  For females, the effect is larger for younger females.  
 
<img width="488" alt="image" src="https://github.com/user-attachments/assets/08e694fa-2777-4d6e-9dd6-cc518578ed05">

## Figure 4  Plot of education effect on salary
Here is an example with two categorical dependent variables, minority and gender with educ as a factor for partitioning
 
<img width="472" alt="image" src="https://github.com/user-attachments/assets/3b4b9912-1fb4-4669-a97b-d88921f6544a">

## Figure 5 gender and jobcat with educ for splits
The first row of plots shows how the gender proportions vary with the educ breakdown, which is less than or equal to 12 versus greater than 12, which is further broken down.  The second row shows how the proportions vary for gender.
This display shows boxplots for the terminal nodes using a tree estimating sepal length, which is a scale variable, using the iris dataset. The maximum tree depth was set to two.

<img width="432" alt="image" src="https://github.com/user-attachments/assets/e6ba373f-0137-43fa-8583-9dfd90ddf112">
Figure 6 Boxplots in the terminal nodes



