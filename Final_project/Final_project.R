pacman::p_load(tidyverse, magrittr, data.table, skimr, R.utils)
setwd("/Users/hiram")
apts = fread("housing_data_2016_2017.csv")

#Missing & Error
substring <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
apts$zip <- substring(apts$full_address_or_zip_code,5)
apt <- subset(apts, select = c("zip","cats_allowed","common_charges","coop_condo","dining_room_type","dogs_allowed","fuel_type","kitchen_type","maintenance_cost","num_bedrooms","num_floors_in_building","num_full_bathrooms","num_half_bathrooms","num_total_rooms","sale_price","sq_footage","total_taxes","walk_score"))
apt[2,1]=11354
apt = data.frame(apt)
apt$num_floors_in_building[is.na(apt$num_floors_in_building)] <- median(apt$num_floors_in_building, na.rm = T)  
apt$num_bedrooms[is.na(apt$num_bedrooms)] <- median(apt$num_bedrooms, na.rm = T)  
apt$num_half_bathrooms[is.na(apt$num_half_bathrooms)] <- median(apt$num_half_bathrooms, na.rm = T)  
apt$sq_footage[is.na(apt$sq_footage)] <- mean(apt$sq_footage, na.rm = T)  
apt$num_total_rooms[is.na(apt$num_total_rooms)] <- median(apt$num_total_rooms, na.rm = T) 
#impute(apt$fuel_type, "oil")
#impute(apt$kitchen_type, "eat in")
#impute(apt$dining_room_type, "condo")
apt$sale_price =gsub("\\$", "", apt$sale_price)
apt$maintenance_cost =gsub("\\$", "", apt$maintenance_cost)
apt$total_taxes =gsub("\\$", "", apt$total_taxes)
apt$common_charges =gsub("\\$", "", apt$common_charges)
apt$sale_price =gsub("\\,", "", apt$sale_price)
apt$maintenance_cost =gsub("\\,", "", apt$maintenance_cost)
apt$total_taxes =gsub("\\,", "", apt$total_taxes)
apt$common_charges =gsub("\\,", "", apt$common_charges)
apt$sale_price <- as.numeric(apt$sale_price)
apt$maintenance_cost <- as.numeric(apt$maintenance_cost)
apt$total_taxes <- as.numeric(apt$total_taxes)
apt$common_charges <- as.numeric(apt$common_charges)
apt$sale_price[is.na(apt$sale_price)] <- mean(apt$sale_price, na.rm = T)  
apt$maintenance_cost[is.na(apt$maintenance_cost)] <- mean(apt$maintenance_cost, na.rm = T)  
apt$total_taxes[is.na(apt$total_taxes)] <- mean(apt$total_taxes, na.rm = T)  
apt$common_charges[is.na(apt$common_charges)] <- mean(apt$common_charges, na.rm = T)  
skim(apt)
view(apt)

#Regression Tree
if (!pacman::p_isinstalled(YARF)){
  pacman::p_install_gh("kapelner/YARF/YARFJARs", ref = "dev")
  pacman::p_install_gh("kapelner/YARF/YARF", ref = "dev", force = TRUE)
}
options(java.parameters = "-Xmx4000m")
pacman::p_load(YARF)
test_prop = 0.2
train_indices = sample(1 : nrow(apt), round((1 - test_prop) * nrow(apt)))
apt_train = apt[train_indices, ]
y_train = apt_train$sale_price
X_train = apt_train
X_train$medv = NULL
n_train = nrow(X_train)
tree_mod = YARFCART(X_train, y_train, calculate_oob_error = TRUE)
get_tree_num_nodes_leaves_max_depths(tree_mod)
nrow(apt_train) / get_tree_num_nodes_leaves_max_depths(tree_mod)$num_leaves
illustrate_trees(tree_mod, max_depth = 5, length_in_px_per_half_split = 30, open_file = TRUE)

#Linear Model
pacman::p_load(ggplot2)
simple_linear_model = lm(sale_price ~ num_bedrooms + num_half_bathrooms + num_full_bathrooms + maintenance_cost + total_taxes + sq_footage + cats_allowed + dogs_allowed + coop_condo + common_charges, data = apt)
coef(simple_linear_model)
summary(simple_linear_model)$r.squared
summary(simple_linear_model)$sigma

simple_linear_model1 = lm(sale_price ~ num_bedrooms + num_half_bathrooms + num_full_bathrooms + maintenance_cost + total_taxes + sq_footage + cats_allowed + dogs_allowed + coop_condo + common_charges + num_floors_in_building, data = apt)
coef(simple_linear_model1)
summary(simple_linear_model1)$r.squared
summary(simple_linear_model1)$sigma

simple_linear_model2 = lm(sale_price ~ num_bedrooms + num_half_bathrooms + num_full_bathrooms + maintenance_cost + total_taxes + sq_footage + cats_allowed + dogs_allowed + coop_condo + common_charges + num_floors_in_building + fuel_type, data = apt)
coef(simple_linear_model2)
summary(simple_linear_model2)$r.squared
summary(simple_linear_model2)$sigma

#Random Forest
num_trees = 500
train_size = 2000
training_indices = sample(1 : nrow(apt), train_size)
apt_train = apt[training_indices, ]
y_train = apt_train$sale_price
X_train = apt_train
X_train$sale_price = NULL
mod_bag = YARFBAG(X_train, y_train, num_trees = num_trees, calculate_oob_error = TRUE)
mod_rf = YARF(X_train, y_train, num_trees = num_trees, calculate_oob_error = TRUE)

tree_mod = YARFCART(X_train, y_train)
y_hat_train = predict(tree_mod, X_train)
y_hat_test = predict(tree_mod, X_test)
table(y_train, y_hat_train)
oos_confusion_table = table(y_test, y_hat_test)
oos_confusion_table

test_indices = sample(setdiff(1 : nrow(apt), training_indices), 100)
apt_test = apt[test_indices, ]
y_test = apt_test$sale_price
X_test = apt_test
X_test$sale_price = NULL
y_hat_test_bag = predict(mod_bag, X_test)
y_hat_test_rf = predict(mod_rf, X_test)
oos_conf_table_bag = table(y_test, y_hat_test_bag)
oos_conf_table_rf = table(y_test, y_hat_test_rf)
oos_conf_table_bag
oos_conf_table_rf
miscl_err_bag = mean(y_test != y_hat_test_bag)
miscl_err_rf = mean(y_test != y_hat_test_rf)
miscl_err_bag
miscl_err_rf
cat("gain: ", (miscl_err_bag - miscl_err_rf) / miscl_err_bag * 100, "%\n")

#Note
ggplot(apt, aes(x = sale_price, colour = num_bedrooms))+
  geom_freqpoly(binwidth = 50)

