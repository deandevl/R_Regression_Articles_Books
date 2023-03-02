library(mgcv)
library(MASS)

data(mcycle)
str(mcycle)

mcycle_mod_gam <- mgcv::gam(formula = accel ~ s(times), data = mcycle)

basis_mat <- mgcv::model.matrix.gam(mcycle_mod_gam)
coef_mat <- as.matrix(stats::coef(mcycle_mod_gam))

vals_mat <- matrix(data = NA, nrow = nrow(basis_mat), ncol = ncol(basis_mat) - 1)

for(i in 2:ncol(basis_mat)){
  vals <- basis_mat[,i] * coef_mat[i,1] +coef_mat[1,1]
  vals_mat[,i-1] <- vals
}

vals_df <- as.data.frame(vals_mat)
names(vals_df) <- colnames(basis_mat)[2:ncol(basis_mat)]


