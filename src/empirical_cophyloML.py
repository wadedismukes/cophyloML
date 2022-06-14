import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

import tensorflow as tf
from tensorflow	import keras
from tensorflow.keras.models import Sequential
from tensorflow.keras.callbacks import EarlyStopping
from tensorflow.keras import layers

from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.preprocessing import Normalizer
from sklearn.metrics import r2_score, median_absolute_error, mean_squared_error

# Do extensions code below
# if you decide to do the Matplotlib extension, you must save your 
# plot in the directory by uncommenting the line of code below

# fig.savefig('static/images/my_plots.png')
# 0 , 10 columns -> features
# labels -> 11 -> host speciations 0  
# labels -> 12 -> symbiont speciations 1 
# labels -> 13 -> cospeciations 2
# labels -> 14 -> host switches 3
# labels -> 15 -> dispersals 4
def run_model(df, empirical_set):
  # preprocessing
  ## separate into features and labels
  features = df.iloc[:,0:11]
  labels = df.iloc[:,11:15]
  ## split data into training and test sets
  features_training_set, features_test_set, labels_train_set, labels_test_set = train_test_split(features, labels, test_size = 0.3, random_state = 42)
  sc = StandardScaler()
  features_test_set.iloc[2999,:] = empirical_set.iloc[0,1:12]
  features_training_set = features_training_set.to_numpy()
  features_test_set = features_test_set.to_numpy()
  labels_train_set = labels_train_set.to_numpy()
  labels_test_set = labels_test_set.to_numpy()
  ## scale these since they are in different number systems
  scaled_features_training = pd.DataFrame(sc.fit_transform(features_training_set)).to_numpy()
  scaled_features_test = pd.DataFrame(sc.transform(features_test_set)).to_numpy()
  #scaled_empirical = pd.DataFrame(sc.fit_transform(empirical_set)).to_numpy()
  
  model = Sequential()
  input = layers.InputLayer(input_shape = len(features.columns) )
  model.add(input)
  model.add(layers.Dense(64, activation = "relu"))
  model.add(layers.Dense(64, activation = "relu"))
  model.add(layers.Dense(64, activation = "relu"))
  model.add(layers.Dense(64, activation = "relu"))
  model.add(layers.Dense(4))
    
    
  opt = keras.optimizers.Adam(learning_rate = 0.001)
  model.compile(loss = 'mse', metrics = ['mae'], optimizer = opt)
    
    
  stop=EarlyStopping(monitor='loss', mode='min', verbose=1, patience=40)
    
  
  history = model.fit(scaled_features_training, 
                      labels_train_set, 
                      epochs = 200, 
                      batch_size = 256, 
                      verbose = 1, 
                      validation_split = 0.33, 
                      callbacks=[stop])
  
  res_mse, res_mae = model.evaluate(scaled_features_test, labels_test_set)
  #print(res_mse, res_mae)
  predicted_values = model.predict(scaled_features_test) 
  predictions = predicted_values
  empirical_pred = predictions[2999, :]
  # importance = model.coef_[0]
  return(history, predicted_values, labels_test_set, empirical_pred)
#print(r2_score(labels_test_set, predicted_values)) 

#print("Prediction: {}".format(tf.keras(predictions, axis=1)))#
#print("    Labels: {}".format(labels))






### function calls below here
empirical_data = pd.read_csv("empirical_data.csv")

empirical_data.drop("Unnamed: 0", axis = 1)

empirical_set = empirical_data
outfile_base = '/Users/waded/Documents/cophyloML/empirical_test_csp'
csp_rates = ["0.1", "0.2"]
hs_rates = ["0.5", "0.8"]
histories = []
pred_values = []
labs = []
empirical_preds = []
for c in csp_rates:
  for hs in hs_rates:
    out_file = outfile_base + c + "_hs" + hs + ".csv" 
    df = pd.read_csv(out_file)
    history, predicted_vals, test_labs, empirical_pred = run_model(df, empirical_data)
    empirical_preds.append(empirical_pred)
    histories.append(history)
    pred_values.append(predicted_vals)
    labs.append(test_labs)
    
# 
# outfile_base = '/Users/waded/Documents/cophyloML/vignettes/modelloss_ext'
# count = 0
# for e in ext_rates:
#   for c in csp_rates:
#     for hs in hs_rates:
#       out_file = outfile_base + e + "_csp" + c + "_hs" + hs + ".png"
#       plot_model_loss(history=histories[count], outfile=out_file)
#       count += 1
# 
# 
# outfile_base = '/Users/waded/Documents/cophyloML/vignettes/cospeciation_accuracy_ext'
# count = 0
# for e in ext_rates:
#   for c in csp_rates:
#     for hs in hs_rates:
#       out_file = outfile_base + e + "_csp" + c + "_hs" + hs + ".png"
#       plot_cospeciation_accuracy(labels_test_set=labs[count], pred_val=pred_values[count] , outfile=out_file)
#       count += 1
#       
#       
# outfile_base = '/Users/waded/Documents/cophyloML/vignettes/hostswitch_accuracy_ext'
# count = 0
# for e in ext_rates:
#   for c in csp_rates:
#     for hs in hs_rates:
#       out_file = outfile_base + e + "_csp" + c + "_hs" + hs + ".png"
#       plot_hostswitch_accuracy(labels_test_set=labs[count], pred_val=pred_values[count] , outfile=out_file)
#       count += 1
# 
# count = 0
# r2_vals = []
# hostswitch = []
# cosp = []
# extinc = []
# mae_vals = []
# mse_scores = []
# for e in ext_rates:
#   for c in csp_rates:
#     for hs in hs_rates:
#       hostswitch.append(hs)
#       cosp.append(c)
#       extinc.append(e)
#       r2_vals.append(r2_score(labs[count], pred_values[count]))
#       mae_vals.append(median_absolute_error(labs[count], pred_values[count]))
#       mse_scores.append(mean_squared_error(labs[count], pred_values[count]))
#       count += 1
#       
#       
#   
# print(r2_vals)
# print(mae_vals)
# print(mse_scores)
# d = {'Host switching rate': hostswitch,
#       'Cospeciation rate': cosp,
#       'Extinction rate': extinc,
#       'R^2': r2_vals,
#       'MAE': mae_vals,
#       'MSE': mse_scores}
# r2_df = pd.DataFrame(d)
# r2_df.to_csv("r2_vals.csv")
# 
# 
# 
# 
# host_spec_labs = []
# host_spec_pred = [] 
# symb_spec_labs = []
# symb_spec_pred = []
# cosp_labs = []
# cosp_pred = []
# hs_labs = []
# hs_pred = []
# i = 0
# for e in ext_rates:
#   for c in csp_rates:
#     for hs in hs_rates:
#       for j in range(0,len(labs[i][:,0])):
#         host_spec_labs.append(labs[i][j,0])
#         host_spec_pred.append(pred_values[i][j,0])
#         symb_spec_labs.append(labs[i][j,1])
#         symb_spec_pred.append(pred_values[i][j,1])
#         cosp_labs.append(labs[i][j,2])
#         cosp_pred.append(pred_values[i][j,2])
#         hs_labs.append(labs[i][j,3])
#         hs_pred.append(pred_values[i][j,3])
#       i += 1
# 
# 
# data = {'Host Speciation (true)': host_spec_labs,
#       'Host Speciation (predicted)': host_spec_pred,
#       'Symbiont Speciation (true)': symb_spec_labs,
#       'Symbiont Speciation (predicted)': symb_spec_pred,
#       'Cospeciation (true)': cosp_labs,
#       'Cospeciation (predicted)': cosp_pred,
#       'Host-switching (true)': hs_labs,
#       'Host-switching (predicted)': hs_pred}
# comparison_df = pd.DataFrame(data)
# comparison_df.to_csv("pred_vs_true.csv")
### plotting

def plot_model_loss(history, outfile):
  fig = plt.figure()
  ax1 = fig.add_subplot(2,1,1)
  ax1.plot(history.history['mae'])
  ax1.plot(history.history['val_mae'])
  ax1.set_title('model mae')
  ax1.set_ylabel('MAE')
  ax1.set_xlabel('epoch')
  ax1.legend(['train', 'validation'], loc = 'upper left')
  
  ax2 = fig.add_subplot(2,1,2)
  ax2.plot(history.history['loss'])
  ax2.plot(history.history['val_loss'])
  ax2.set_title('model loss')
  ax2.set_ylabel('loss')
  ax2.set_xlabel('epoch')
  ax2.legend(['train', 'validation'], loc='upper left')
  
  fig.tight_layout()
  fig.savefig(outfile)

def plot_cospeciation_accuracy(labels_test_set, pred_val, outfile):
    fig = plt.figure()
    a = plt.axes(aspect='equal')
    plt.scatter(labels_test_set[:,2], pred_val[:,2])
    plt.xlabel('True Values [Cospeciations]')
    plt.ylabel('Predictions [Cospeciations]')
    lims = [0, 50]
    plt.xlim(lims)
    plt.ylim(lims)
    _ = plt.plot(lims, lims)
    
    fig.tight_layout()
    fig.savefig(outfile)

def plot_dispersal_accuracy(labels_test_set, pred_val, outfile):
    fig = plt.figure()
    a = plt.axes(aspect='equal')
    plt.scatter(labels_test_set[:,4], pred_val[:,4])
    plt.xlabel('True Values [Dispersals]')
    plt.ylabel('Predictions [Dispersals]')
    lims = [0, 50]
    plt.xlim(lims)
    plt.ylim(lims)
    _ = plt.plot(lims, lims)
    fig.tight_layout()
    fig.savefig(outfile)

def plot_hostswitch_accuracy(labels_test_set, pred_val, outfile):
    fig = plt.figure()
    a = plt.axes(aspect='equal')
    plt.scatter(labels_test_set[:,3], pred_val[:,3])
    plt.xlabel('True Values [Host Switches]')
    plt.ylabel('Predictions [Host Switches]')
    lims = [0, 50]
    plt.xlim(lims)
    plt.ylim(lims)
    _ = plt.plot(lims, lims)
    fig.tight_layout()
    fig.savefig(outfile)
