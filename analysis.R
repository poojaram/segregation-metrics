library('ggplot2')
source('metric_functions.R')

my_files = read_all_files()

interaction_results = lapply(my_files, compute_interaction)
isolation_results = lapply(my_files, compute_isolation)
dissimilarity_results = lapply(my_files, compute_dissimilarity)

correlation_results = lapply(my_files, compute_correlation)

new_results = lapply(my_files, compute_new)

isolation_df = model_data(isolation_results)
interaction_df = model_data(interaction_results)
dissimilarity_df = model_data(dissimilarity_results)

correlation_df = model_data(correlation_results)

new_df = model_data(new_results)

isolation_hist = hist(isolation_df)
interaction_hist = hist(interaction_df)
dissimilarity_hist = hist(dissimilarity_df)

correlation_hist = hist(correlation_df)

new_hist = hist(new_df)

iso_table = to_df(isolation_df)
int_table = to_df(interaction_df)
dissimilarity_table = to_df(dissimilarity_df)

correlation_table = to_df(correlation_df)

new_table = to_df(new_df)

iso_plot <- ggplot(data = iso_table, aes(x=City, y = Value)) + geom_bar(stat = "identity", fill="steelblue")+ theme(text = element_text(size=20),axis.text.x=element_text(angle=45, hjust=1))
int_plot = ggplot(data = int_table, aes(x=City, y = Value)) + geom_bar(stat = "identity", fill="steelblue") + theme(text = element_text(size=20),axis.text.x=element_text(angle=45, hjust=1))
dissimilarity_plot = ggplot(data = dissimilarity_table, aes(x=City, y = Value)) + geom_bar(stat = "identity", fill="steelblue") + theme(text = element_text(size=20),axis.text.x=element_text(angle=45, hjust=1))

correlation_plot = ggplot(data = correlation_table, aes(x=City, y = Value)) + geom_bar(stat = "identity", fill="steelblue") + theme(text = element_text(size=20),axis.text.x=element_text(angle=45, hjust=1))

new_plot = ggplot(data = new_table, aes(x=City, y = Value)) + geom_bar(stat = "identity", fill="steelblue") + theme(text = element_text(size=20),axis.text.x=element_text(angle=45, hjust=1))

