#!/usr/bin/env python 
# -*- coding:utf-8 -*-
#correlations
cor = df.corr()
cor

# Using Pearson Correlation, plot a sample correlation heatmap
plt.figure(figsize=(12,10))
example = np.matrix(cor)[0:14,0:14]
mask = np.triu(np.ones_like(example, dtype=bool)) #mask upper triangle
cmap = sns.diverging_palette(230, 20, as_cmap=True) #color palette
sns.heatmap(example, annot=True, mask = mask,cmap=cmap)
plt.show()

# drop when correlation > 0.95
cor_matrix = cor.abs()
upper_tri = cor_matrix.where(np.triu(np.ones(cor_matrix.shape),k=1).astype(np.bool))
to_drop = [column for column in upper_tri.columns if any(upper_tri[column] > 0.95)]
print(to_drop)

