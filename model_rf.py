import pandas as pd
import numpy as np
from sklearn.metrics import roc_curve, auc
import matplotlib.pyplot as plt
from sklearn import metrics
from sklearn.model_selection import learning_curve

x_train = pd.read_csv("x_train.csv")
y_train = pd.read_csv("y_train.cvs")
x_test = pd.read_csv("x_test.csv")
y_test = pd.read_csv("y_test.csv")


from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import GridSearchCV
# Parameter Tuning: n_estimators
# optimal n_estimators=60
param_test1 = {'n_estimators':range(10,71,10)}
gsearch1 = GridSearchCV(estimator = RandomForestClassifier(min_samples_split=100,
                                 min_samples_leaf=20,max_depth=8,max_features='sqrt' ,random_state=10), 
                      param_grid = param_test1, scoring='roc_auc',cv=5)
gsearch1.fit(x_train,y_train)
gsearch1.best_params_, gsearch1.best_score_

# Parameter Tuning: max_depth min_samples_split
# optimal max_depth=13 min_samples_split=50
param_test2 = {'max_depth':range(3,14,2), 'min_samples_split':range(50,201,20)}
gsearch2 = GridSearchCV(estimator = RandomForestClassifier(n_estimators= 60, 
                                 min_samples_leaf=20,max_features='sqrt' ,oob_score=True, random_state=10),
  param_grid = param_test2, scoring='roc_auc',iid=False, cv=5)
gsearch2.fit(x_train,y_train)
gsearch2.best_params_, gsearch2.best_score_

# Parameter Tuning: min_samples_split min_samples_leaf 
# optimal min_samples_leaf=10 min_samples_split=80
param_test3 = {'min_samples_split':range(80,150,20), 'min_samples_leaf':range(10,60,10)}
gsearch3 = GridSearchCV(estimator = RandomForestClassifier(n_estimators= 60, max_depth=13,
                                 min_samples_split=50, max_features='sqrt' ,oob_score=True, random_state=10),
  param_grid = param_test3, scoring='roc_auc',iid=False, cv=5)
gsearch3.fit(x_train,y_train)
gsearch3.best_params_, gsearch3.best_score_

# Parameter Tuning: max_features 
# optimal max_features=11
test=[]
for i in range(43):
   clf = RandomForestClassifier(n_estimators= 60, 
                            max_depth=13, 
                            min_samples_split=80,
                            min_samples_leaf=10,
                            max_features=i+1,
                            oob_score=True, 
                            random_state=10)
   clf.fit(x_train,y_train)
   score=clf.score(x_test,y_test)
   test.append(score)
import matplotlib.pyplot as plt
plt.figure(figsize=(10,4),dpi=80)
plt.plot(range(1,44),test)
plt.title('max_features')
plt.show()
np.argmax(test)

#case1：Learning Curve
clf = RandomForestClassifier(n_estimators= 60, 
                         max_depth=13, 
                         min_samples_split=80,
                         min_samples_leaf=10,
                         max_features=11,
                         oob_score=True, 
                         random_state=10)
train_sizes,train_scores,test_scores=learning_curve(estimator=clf,X=x_train,y=y_train,train_sizes=np.linspace(0.009,1.0,10),cv=10,n_jobs=1)
train_mean = np.mean(train_scores,axis=1)
train_std = np.std(train_scores,axis=1)
test_mean =np.mean(test_scores,axis=1)
test_std = np.std(test_scores,axis=1)
train_sizes_log = np.log(train_sizes)
plt.plot(train_sizes_log,train_mean,color='blue',marker='o',markersize=5,label='training accuracy')
plt.fill_between(train_sizes_log,train_mean+train_std,train_mean-train_std,alpha=0.15,color='blue')
plt.plot(train_sizes_log,test_mean,color='green',linestyle='--',marker='s',markersize=5,label='test accuracy')
plt.fill_between(train_sizes_log,test_mean+test_std,test_mean-test_std,alpha=0.15,color='green')
plt.grid()
plt.xlabel('Log(Number of training samples)')
plt.ylabel('Percent Correctly Classified Instances')
plt.legend(loc='lower right')
plt.title("Learning Curve(Random Forest)",{'fontsize':'large','fontweight':'bold'})
plt.savefig("image\RandomForest_LearningCurve.png")
plt.show()

#case2: confusion matrix
clf = RandomForestClassifier(n_estimators= 60, 
                         max_depth=13, 
                         min_samples_split=80,
                         min_samples_leaf=10,
                         max_features=11,
                         oob_score=True, 
                         random_state=10)
clf.fit(x_train,y_train)
y_test, y_pred = y_test, clf.predict(x_test)
print(metrics.classification_report(y_test,y_pred,digits=4))
print(metrics.confusion_matrix(y_test,y_pred))

#case3: ROC curve
y_pred_proba = clf.predict_proba(x_test) # return probabilities
prob_rf = y_pred_proba[:,1]
def plot_roc(labels, predict_prob):
    false_positive_rate,true_positive_rate,thresholds=roc_curve(labels, predict_prob)
    roc_auc=auc(false_positive_rate, true_positive_rate)
    plt.title('ROC(Random Forest)')
    plt.plot(false_positive_rate, true_positive_rate,'b',label='AUC = %0.4f'% roc_auc)
    plt.legend(loc='lower right')
    plt.plot([0,1],[0,1],'r--')
    plt.ylabel('TPR')
    plt.xlabel('FPR')
    plt.savefig("image\RandomForest_ROC.png")
    plt.show()
plot_roc(y_test,y_pred_proba[:,1])

# case4: LIFT curve
import scikitplot as skplt
skplt.metrics.plot_cumulative_gain(y_test, y_pred_proba)
plt.title("Cumulative Gain Curve(Random Forest)")
plt.savefig("image\RandomForest_LiftCurve.png")
plt.show()


# Feature importance ranking
feat_labels = ['Age', 'ser_life', ...] # list variable names
importances = clf.feature_importances_
indices = np.argsort(importances)[::-1]
for f in range(x_train.shape[1]):
   print("%2d) %-*s %f" % (f + 1, 30, feat_labels[indices[f]], importances[indices[f]]))
