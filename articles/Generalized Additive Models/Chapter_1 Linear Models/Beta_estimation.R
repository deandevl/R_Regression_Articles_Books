library(gamair)
library(data.table)


# Reference text: "Generalized Additive Models" by Simon Wood
# Section 1.3.1 page 12


# set X and Y matrices
data("sperm.comp1", package = "gamair")

intercept_dt <- data.table(Intercept = 1)

sperm1_dt <- data.table::setDT(sperm.comp1)
data.table::setnames(sperm1_dt,c("time.ipc","prop.partner"), c("time_ipc","prop_partner"))

X_dt <- sperm1_dt[, .(time_ipc, prop_partner)]
X_dt <- cbind(intercept_dt, X_dt)

X <- as.matrix(X_dt)
Y <- as.matrix(sperm1_dt[, .(count)])

# decompose X
qr_lst <- base::qr(X)

# the rank
rank <- qr_lst$rank

# the Q factor
Q <- base::qr.Q(qr_lst)
Q

# the R factor
R <- base::qr.R(qr_lst)
R

# reconstruct X
X_re <- base::qr.X(qr_lst)
X_re


# Q * Y
QY <- base::qr.qy(qr_lst, Y)
QY

# Q(transpose) * Y
tQY <- base::qr.qty(qr_lst, Y)
tQY

# R inverse
R_inv <- solve(R)
R_inv

# check inverse
I_check <- R_inv %*% R
I_check

f <- tQY[1:3]
f

beta_hat <- R_inv %*% f
beta_hat
