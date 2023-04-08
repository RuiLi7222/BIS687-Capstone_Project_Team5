import pandas as pd
import numpy as np
from sklearn.metrics import roc_curve, auc
import matplotlib.pyplot as plt
from sklearn import tree
from sklearn.pipeline import Pipeline
from sklearn import metrics
from sklearn.model_selection import learning_curve
from sklearn.feature_selection import SelectKBest,f_classif

x_train = pd.read_csv("x_train.csv")
y_train = pd.read_csv("y_train.cvs")
x_test = pd.read_csv("x_test.csv")
y_test = pd.read_csv("y_test.csv")

# Parameter Tuning: selectkbest
# select k best variables. 
# optimal k=37
test=[]
for i in range(43): # select i variables. need to change 43 to other numbers. 
   pipe_dt=Pipeline([("kbest", SelectKBest(f_classif,k=i+1)),('clf2',tree.DecisionTreeClassifier(max_depth=9,criterion='entropy'))])
   pipe_dt.fit(x_train,y_train)
   score=pipe_dt.score(x_test,y_test)
   test.append(score)
plt.figure(figsize=(10,4),dpi=80)
plt.plot(range(1,44),test)
plt.title('SelectKBest k')
plt.show()
np.argmax(test)

# Parameter Tuning: max_depth
# optimal max_depth=10
test=[]
for i in range(20):
   pipe_dt=Pipeline([("kbest", SelectKBest(f_classif,k=37)),('clf2',tree.DecisionTreeClassifier(max_depth=i+1,criterion='entropy'))])
   pipe_dt.fit(x_train,y_train)
   score=pipe_dt.score(x_test,y_test)
   test.append(score)
import matplotlib.pyplot as plt
plt.figure(figsize=(10,4),dpi=80)
plt.plot(range(1,21),test)
plt.title('max_depth')
plt.show()
np.argmax(test)

#case1：Learning Curve
pipe_dt=Pipeline([("kbest", SelectKBest(f_classif,k=37)),('clf2',tree.DecisionTreeClassifier(max_depth=10,criterion='gini'))])
train_sizes,train_scores,test_scores=learning_curve(estimator=pipe_dt,X=x_train,y=y_train,train_sizes=np.linspace(0.009,1.0,20),cv=10,n_jobs=1)
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
plt.title("Learning Curve(Decision Tree)",{'fontsize':'large','fontweight':'bold'})
plt.savefig("image\DecisionTree_LearningCurve.png")
plt.show()

#case2: confusion matrix
pipe_dt=Pipeline([("kbest", SelectKBest(f_classif,k=37)),('clf2',tree.DecisionTreeClassifier(max_depth=10,criterion='gini'))])
pipe_dt.fit(x_train,y_train)
y_test, y_pred = y_test, pipe_dt.predict(x_test)
print(metrics.classification_report(y_test,y_pred,digits=4))
print(metrics.confusion_matrix(y_test,y_pred))

#case3: ROC curve
y_pred_proba = pipe_dt.predict_proba(x_test) # return probability
prob_dt = y_pred_proba[:,1]
def plot_roc(labels, predict_prob):
    false_positive_rate,true_positive_rate,thresholds=roc_curve(labels, predict_prob)
    roc_auc=auc(false_positive_rate, true_positive_rate)
    plt.title('ROC(Decision Tree)')
    plt.plot(false_positive_rate, true_positive_rate,'b',label='AUC = %0.4f'% roc_auc)
    plt.legend(loc='lower right')
    plt.plot([0,1],[0,1],'r--')
    plt.ylabel('TPR')
    plt.xlabel('FPR')
    plt.savefig("image\DecisionTree_ROC.png")
    plt.show()
plot_roc(y_test,y_pred_proba[:,1])


# Tree Visualization
import pydotplus 
# Create decision tree classifer object
clf2 = tree.DecisionTreeClassifier(max_leaf_nodes=8,criterion='gini')
# Train model
model = clf2.fit(X=x_train, y=y_train)
# Create DOT data
name_list = ['Age', 'ser_life', ...] # list variable names
dot_data = tree.export_graphviz(clf2, out_file=None,class_names=['NO VALUE','VALUE'],feature_names = name_list,filled=True,rounded=True)
# Draw graph
graph = pydotplus.graph_from_dot_data(dot_data)  
# Show graph
graph.write_png("image\DecisionTree_Viz.png")


