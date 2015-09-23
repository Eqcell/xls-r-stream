# -*- coding: utf-8 -*-
"""
# Calculate linear formulas on dataframes in Python based on R code
# 
"""

import pandas as pd
import numpy as np

"""
# data
credit     = c(1000, 1200, 0)
credit_rog = c(1,     1.2,   1.35)
df = data.frame(credit, credit_rog)
res        = c(1000, 1200, 1620)
"""
credit = [1000, 1200, 0]
credit_rog = [1, 1.2, 1.35]
res = [1000, 1200, 1620]


"""
# calculation 2
T = length(credit)
for (t in 2:T)
   {credit[t] = credit[t-1]*credit_rog[t]}
stopifnot(credit == res)
"""

T = len(credit)
for t in range(1,T):    
    credit[t] = credit[t-1]*credit_rog[t]
assert credit == res
 

"""
# calculation 1
credit     = cumprod(credit_rog) * credit[1]
stopifnot(credit == res)
"""
a_credit = np.array(credit)
a_credit_rog = np.array(credit_rog)
a_res = np.array(res)

a_credit = a_credit[0] * np.cumprod(a_credit_rog)
assert (a_credit == a_res).all() == True


"""
# calculation 3
rm(credit, credit_rog)
T = dim(df)[1]
for (t in 2:T)
   {df$credit[t] = df$credit[t-1]*df$credit_rog[t]}
stopifnot(df$credit == res)
"""

df = pd.DataFrame({'credit':credit, 'credit_rog':credit_rog})
df.credit = df.credit[0] * df.credit_rog.cumprod()
assert (df.credit == res).all() == True

for t in range(1,T):    
    df.credit[t] = df.credit[t-1] * df.credit_rog[t]
assert (df.credit == res).all() == True

#see also df.eval()

"""
# calulation 4
T = dim(df)[1]
attach(df)
for (t in 2:T)
   {credit[t] = credit[t-1]*credit_rog[t]}
detach(df)
stopifnot(credit == res)
"""

# PROBLEM: in python pandas cannot do attach() detach() on dataframe
#          bt in this 
