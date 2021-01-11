cf_60 <- set_cf(dlnm.main.gam_multimob3, config, analysis_df, retail_basis, -60)
A <- make_cfplot(cf_60, 'New York City, New York County, NY', -60) +
  ggtitle("Counterfactual -60")

cf_50 <- set_cf(dlnm.main.gam_multimob3, config, analysis_df, retail_basis, -50)
B <- make_cfplot(cf_50, 'New York City, New York County, NY', -50) +
  ggtitle("Counterfactual -50")

cf_40 <- set_cf(dlnm.main.gam_multimob3, config, analysis_df, retail_basis, -40)
C <- make_cfplot(cf_40, 'New York City, New York County, NY', -40) +
  ggtitle("Counterfactual -40")

cf_30 <- set_cf(dlnm.main.gam_multimob3, config, analysis_df, retail_basis, -30)
D <- make_cfplot(cf_30, 'New York City, New York County, NY', -30) +
  ggtitle("Counterfactual -30")

cf_25 <- set_cf(dlnm.main.gam_multimob3, config, analysis_df, retail_basis, -25)
E <- make_cfplot(cf_25, 'New York City, New York County, NY', -25) +
  ggtitle("Counterfactual -25")

library(cowplot)
plot_grid(A, B, C, D, E, ncol=1)


A <- make_cfplot(cf_60, 'Houston, Galveston County, TX', -60) +
  ggtitle("Counterfactual -60")

B <- make_cfplot(cf_50, 'Houston, Galveston County, TX', -50) +
  ggtitle("Counterfactual -50")

C <- make_cfplot(cf_40, 'Houston, Galveston County, TX', -40) +
  ggtitle("Counterfactual -40")

D <- make_cfplot(cf_30, 'Houston, Galveston County, TX', -30) +
  ggtitle("Counterfactual -30")

E <- make_cfplot(cf_25, 'Houston, Galveston County, TX', -25) +
  ggtitle("Counterfactual -25")

cf_0 <- set_cf(dlnm.main.gam_multimob3, config, analysis_df, retail_basis, 0)
F <- make_cfplot(cf_0, 'Houston, Galveston County, TX', 0) +
  ggtitle("Counterfactual 0")

library(cowplot)
plot_grid(A, B, E, F, ncol=1)
