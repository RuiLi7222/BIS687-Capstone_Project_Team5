import pandas as pd
from sklearn.model_selection import train_test_split

df = pd.read_csv("xxx.csv")
x = df.drop(['value'],axis=1)
y = df.loc[:,'value']
x_train, x_test, y_train, y_test= train_test_split(x, y, test_size = 0.2)

# SMOTE If dataset is very imbalanced
from imblearn.over_sampling import SMOTE
smo = SMOTE(random_state=55)
x_train, y_train = smo.fit_sample(x_train, y_train)

from sklearn.utils import shuffle
x_train, y_train = shuffle(x_train, y_train)

x_train.to_csv("x_train.csv")
y_train.to_csv("y_train.cvs")
x_test.to_csv("x_test.csv")
y_test.to_csv("y_test.csv")