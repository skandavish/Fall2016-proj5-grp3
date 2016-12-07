# ADS Final Project: 

![image](https://github.com/TZstatsADS/Fall2016-proj5-grp3/blob/master/figs/santander%20logo.gif)

Term: Fall 2016

+ Team Name: The Santander Data Group
+ Projec title: [Santander Product Recommendation](https://www.kaggle.com/c/santander-product-recommendation)
+ Project Data: [Santander Data: download here](https://www.kaggle.com/c/santander-product-recommendation/data)
+ Project Description

To support needs for a range of financial decisions, Santander Bank offers a lending hand to their customers through personalized product recommendations. Our goal is to predict which products their existing customers will use in the next month based on their past behavior and that of similar customers.

+ Team members
	+ Chenxi Huang (ch3129) (Celia)
	+ Jingdan Zhao (jz2678)
	+ Zachary (Zach) Chen (zc2317)
	+ Skanda Vishwanath (sv2481)
	+ William Raikes (wrr2109)
	
	
+ Contribution statement 

The whole team agreed on the study.
CH, JZ, ZC and WR discussed and designed the model evaluation protocol. 
CH, JZ, ZC and WR carried out the computation for model evaluation. 
CH prepared the presentation

Chenxi cleaned the data. 
Chenxi explored feature engineering and feature selection.
Chenxi trained mlr::rFerns and mlr::RandomForestSRC for predictions and explanations purposes. 
Chenxi prepared the presentation and is the presenter. 
Chenxi organized group meetings, Github files and project descriptions.
Chenxi also explored and researched on multi-label classification, naive bayes, multinomial and ML-KNN.
Chenxi (with William) built baseline model and evaluated the results.
Chenxi (with William) wrote the project report and organized the Github.

William coded XGBoost & Random Forest models.  Also created baseline data.  And supported efforts in data exploration, feature engineering, model building and selection, and reporting.

Jingdan built SVM and Random Forest models. Jingdan built cross-validation results of SVM and random forest. 

Skanda created the clean analytical dataset using a lean code. Skanda also created new features that explained the month on month cariation thus making it possible to roll up the data by ensuring no loss in information. Skanda tried boosting, random forest and multinomial logistic regression for predictions.

Zachary built decision tree models with a specific focus on Santander savings account. Zachary developed and visualized single decision trees, prune trees, and random forest trees for predictions and explanations purposes. Zachary also built prediction results and uploaded them on github. Zachary also participated in team meetings and shared and contributed his ideas to the team.  

All team members contributed to the GitHub repository. 
All team members approve our work presented in our GitHub repository including this contribution statement.


+ Project summary: The objective of this project is to apply a predictive algorithm to determine which products a consumer will have at a given time period. After processing the data, and imputing missing values, we included extra features that were originally included as outcome variables. These features showed which products a consumer had at a given month, and along with the demographics, were used to predict the user's products in the subsequent month. Another, less accurate method, just used the demographics of all the users, and produced a model predicting the subsequent month product selection. Many predictive models were employed, and by using cross-validation, several of the models were high performers. The final model selected was the XGBoost with the extra features, as it had the lowest error rate. It should be noted that a baseline model was also employed, and resulted in a decent performance as well. The baseline model was just an average likelihood of a certain product obtained by any given consumer. The written report is linked below, as well as the final presentation. Best error rate is **0.3%**.


**Final CV Results Comparison**

![image](https://github.com/TZstatsADS/Fall2016-proj5-grp3/blob/master/figs/finalresults.png)


+ **####### we wrote a compherensive project report. Please click here for more details! #######**
+ **################################### Don't miss it! #################**

[**FINAL WRITTEN REPORT**](https://github.com/TZstatsADS/Fall2016-proj5-grp3/blob/master/project%205%20report_chenxi.pdf)

[**POWERPOINT PRESENTATION (ppt version)**](https://github.com/TZstatsADS/Fall2016-proj5-grp3/blob/master/doc/proj5grp3_presentation_chenxi.pptx)

[**POWERPOINT PRESENTATION (pdf version, github-reading friendly)**](https://github.com/TZstatsADS/Fall2016-proj5-grp3/blob/master/proj5grp3_presentation_chenxi.pdf)

** *presentation slides prepared by Chenxi. Project report written by Chenxi and William* **




+ ################################ References ######################################

[MLKNN publication](http://cs.nju.edu.cn/zhouzh/zhouzh.files/publication/pr07.pdf) 

[MLPUGS tutorial](https://cran.r-project.org/web/packages/MLPUGS/vignettes/tutorial.html)

[mlR package](https://mlr-org.github.io/mlr-tutorial/release/html/multilabel/index.html#predict)

[mldr package](https://cran.r-project.org/web/packages/mldr/vignettes/mldr.pdf)

This folder is orgarnized as follows.


+ ################################ Thank you! ##################################
```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
