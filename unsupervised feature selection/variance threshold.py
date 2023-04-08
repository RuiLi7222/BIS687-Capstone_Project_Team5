#!/usr/bin/env python 
# -*- coding:utf-8 -*-

from sklearn.feature_selection import VarianceThreshold

sel = VarianceThreshold(threshold=0.05)
df_selection = sel.fit_transform(df)
df_selection.shape

# remaining features' names
sel.get_feature_names_out(df.columns)

#remaining dataframe
df_reduced_1 = sel.transform(df)

# dropped features
in_indeces = sel.get_support(range(284))
all_indeces = np.array(range(284))
out_indeces = [x for x in all_indeces if x not in in_indeces]
df.columns[out_indeces]