# Calculate linear formulas on dataframes in R
# 23.09.2015

# data
credit     = c(1000,  1200,  0)
credit_rog = c(1,     1.2,   1.35)
df = data.frame(credit, credit_rog)
res        = c(1000,  1200,  1620)

# calculation 1
credit     = cumprod(credit_rog) * credit[1]
stopifnot(credit == res)

# calculation 2
T = length(credit)
for (t in 2:T)
   {credit[t] = credit[t-1]*credit_rog[t]}
stopifnot(credit == res)

# calculation 3
rm(credit, credit_rog)
T = dim(df)[1]
for (t in 2:T)
   {df$credit[t] = df$credit[t-1]*df$credit_rog[t]}
stopifnot(df$credit == res)

# calulation 4
T = dim(df)[1]
attach(df)
for (t in 2:T)
   {credit[t] = credit[t-1]*credit_rog[t]}
detach(df)
stopifnot(credit == res)
