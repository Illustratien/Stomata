pacman::p_load(dplyr,ompr,ompr.roi,ROI.plugin.glpk)

dist_fun <- function(i, j, distance) {
  vapply(seq_along(i), function(k) distance[i[k], j[k]], numeric(1L))
}

optim_d<- function(df){
  
  distance <- df %>%
    dplyr::select(stomata.cx,stomata.cy) %>% 
    stats::dist() %>%
    as.matrix(diag = TRUE, upper = TRUE)
  
  n <- nrow(df)
  model <- MIPModel() %>%
    # we create a variable that is 1 iff we travel from point i to j
    add_variable(x[i, j], i = 1:n, j = 1:n, 
                 type = "integer", lb = 0, ub = 1) %>%
    
    # a helper variable for the MTZ formulation of the tsp
    add_variable(u[i], i = 1:n, lb = 1, ub = n) %>% 
    
    # minimize travel distance
    set_objective(sum_expr(dist_fun(i, j, distance) * x[i, j], i = 1:n, j = 1:n), "min") %>%
    
    # you cannot go to the same point
    set_bounds(x[i, i], ub = 0, i = 1:n) %>%
    
    # leave each point
    add_constraint(sum_expr(x[i, j], j = 1:n) == 1, i = 1:n) %>%
    #
    # visit each point
    add_constraint(sum_expr(x[i, j], i = 1:n) == 1, j = 1:n) %>%
    
    # ensure no subtours (arc constraints)
    add_constraint(u[i] >= 2, i = 2:n) %>% 
    add_constraint(u[i] - u[j] + 1 <= (n - 1) * (1 - x[i, j]), i = 2:n, j = 2:n)
  
  result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))
  # total distance
  data.frame(pic_name=df$pic_name[1],
             route=result$objective_value)
}
