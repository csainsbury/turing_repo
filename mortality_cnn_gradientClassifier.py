from numpy.random import seed
seed(1)
from tensorflow import set_random_seed
set_random_seed(2)

from keras.layers import Input, Embedding, Reshape, merge, Dropout, Dense, LSTM, core, Activation
from keras.layers import TimeDistributed, Flatten, concatenate, Bidirectional, Concatenate, Conv1D, MaxPooling1D, Conv2D

from keras.engine import Model
from keras.models import Sequential

from keras import layers, optimizers

import numpy as np
import pandas as pd

from sklearn.preprocessing import StandardScaler
from sklearn import preprocessing

import my_callbacks

sc = StandardScaler()

'''
data prep
'''
# load main data input from R
hba1c_train_val_Set = pd.read_csv('./inputFiles/python_mainInput.csv')
hba1c_trainValSet = hba1c_train_val_Set.values

hba1cMask_train_val_Set = pd.read_csv('./inputFiles/python_mainMask.csv')
hba1cMask_trainValSet = hba1cMask_train_val_Set.values

#sbp_train_val_Set = pd.read_csv('./inputFiles/alldata_2param_sbp_T1.csv')
#sbp_trainValSet = sbp_train_val_Set.values

# stack to 3D array
trainValSet = np.dstack([hba1c_trainValSet, hba1cMask_trainValSet])

#trainValSet = trainValSet[0:200, :]
ncolOfSet = trainValSet.shape[1]

X_trainVal = trainValSet[:, 0:(ncolOfSet - 1), :]

# load y values from file
y_trainVal_Set = pd.read_csv('./inputFiles/python_gradient.csv')
y_trainVal_Set = y_trainVal_Set.values

# remove ID col
y_trainVal = y_trainVal_Set[:, 1]

# generate categorical y
y_trainVal = y_trainVal<0

#X_test_initial = testSet[:, 0:49]
#y_test_initial = testSet[:, 49]

#y_test = y_test_initial

# kfold
kfold_splits = 4
from sklearn.model_selection import StratifiedKFold
skf = StratifiedKFold(n_splits=kfold_splits, shuffle=True)
cvscores = []
aucs = []
losses = []

for index, (train_indices, val_indices) in enumerate(skf.split(X_trainVal, y_trainVal)):
    print "Training on fold " + str(index+1) + "/" + str(kfold_splits) + "..."
    # Generate batches from indices
    X_train, X_val = X_trainVal[train_indices], X_trainVal[val_indices]
    y_train, y_val = y_trainVal[train_indices], y_trainVal[val_indices]
    # save out files with linkids for output analysis
    np.savetxt("./pythonOutput/X_train_" + str(index) +  "_.csv", X_train[:, :, 0], delimiter=",")
    np.savetxt("./pythonOutput/y_train_" + str(index) +  "_.csv", y_train, delimiter=",")
    np.savetxt("./pythonOutput/X_val_" + str(index) +  "_.csv", X_val[:, :, 0], delimiter=",")
    np.savetxt("./pythonOutput/y_val_" + str(index) +  "_.csv", y_val, delimiter=",")
    # remove first col (LinkId)
    X_train = X_train[:, 1:(X_train.shape[1])]
    print(X_train.shape)
    X_val_IDs = X_val[:, 0] # save IDs for saving out later
    X_val = X_val[:, 1:(X_val.shape[1])]
    print(X_val.shape)
#   from sklearn.preprocessing import StandardScaler
    scalers = {}
    for i in range(X_train.shape[1]):
        scalers[i] = StandardScaler()
        X_train[:, i, :] = scalers[i].fit_transform(X_train[:, i, :])
        X_val[:, i, :] = scalers[i].fit_transform(X_val[:, i, :])
#
## RNN setup
    LSTM_layer1 =  16 # 64 # 32 # 256
#
    dropout_n = 0.2
#
    n_epochs = 30 # 168
    n_batch_size = 256 # 64
#
    LSTMinputDim = (2)
    numericTS_set = Input(shape = (len(X_train[0]), 2), name = 'numericTS_set')
    output_merge = numericTS_set
#
    output_merge = Conv1D(filters=22, kernel_size=6 , strides = 3, activation='relu')(output_merge)
    output_merge = MaxPooling1D(pool_size=2)(output_merge)
#
    output_merge = LSTM(return_sequences=False, units=LSTM_layer1, dropout = 0, recurrent_dropout = 0.2)(output_merge)
#
    output_merge = Dropout(dropout_n)(output_merge)
#    output_merge = Flatten()(output_merge)
#
    output_merge = Dense(8)(output_merge)
    output_merge = Dropout(dropout_n)(output_merge)
#
    main_output = Dense(1, activation = 'sigmoid')(output_merge)
#
    model = Model(inputs=numericTS_set, outputs=main_output)
    #adam = optimizers.Adam(lr=0.001, beta_1=0.9, beta_2=0.999, epsilon=1e-08, decay=0.0)
    adam = optimizers.Adam(lr=0.0001, beta_1=0.9, beta_2=0.999, epsilon=1e-08, decay=0.0)
#
    #model.compile(optimizer='adam', loss='binary_crossentropy', loss_weights=[0.2, 0.4])
    #adam = optimizers.Adam
    model.compile(optimizer = adam, loss='binary_crossentropy', metrics=['accuracy'])
#
    class_weight = {0: 1.,
                    1: 4., # 1: 20.,
                    2: 1.
                    }
# prepare callback
    histories = my_callbacks.Histories()
#
    history = model.fit(X_train, y_train, epochs=n_epochs, batch_size=n_batch_size, validation_data = ([X_val, y_val]), class_weight=class_weight, callbacks = [histories])
#
    scores = model.evaluate(X_val, y_val, verbose=0)
    print("%s: %.2f%%" % (model.metrics_names[1], scores[1]*100))
    cvscores.append(scores[1] * 100)
#
    auc_values = histories.aucs
    aucs.append(auc_values)
#
    loss_values = histories.losses
    losses.append(loss_values)
#
    y_pred_kfoldValidationSet_asNumber = model.predict([X_val])
    y_preds = y_pred_kfoldValidationSet_asNumber[:, 0]
    val_output = np.column_stack((X_val_IDs, y_preds))
    np.savetxt("./pythonOutput/y_pred_asNumber_2param_1param_fold_" + str(index) +  "_.csv", val_output, delimiter=",")


# calculate average AUC per epoch
auc_array = np.asarray(aucs)
print(auc_array)
np.savetxt("./pythonOutput/aucs_array.csv", auc_array, delimiter=",")
average_aucs = np.mean(auc_array, axis = 0)
print("average_aucs")
print(average_aucs)
np.savetxt("./pythonOutput/average_aucs.csv", average_aucs, delimiter=",")

# calculate average loss per epoch
loss_array = np.asarray(losses)
print(loss_array)
average_losses = np.mean(loss_array, axis = 0)

# plot losses
import matplotlib
matplotlib.use('Agg') # ensure that matplotlib doesn't try and call a display

import matplotlib.pyplot as plt
loss = history.history['loss']
val_loss = history.history['val_loss']
#acc = history.history['acc']
#val_acc = history.history['val_acc']
epochs = range(1, len(loss) + 1)

plt.plot(epochs, loss, 'bo', label = 'Training loss')
plt.plot(epochs, val_loss, 'b', label = 'Validation loss')
plt.legend()
plt.savefig('./pythonOutput/loss_valLoss_2param.png', dpi = 300)
plt.clf()

# plot AUROC - use the averaged AUC per epoch value
auc_p = average_aucs
auc_p_losses = average_losses

# plot epoch vs auroc
epochs = range(1, len(auc_p) + 1)
plt.plot(epochs, auc_p, 'bo', label = 'epoch vs average auc per epoch')
plt.legend()
plt.savefig('./pythonOutput/auc_plot_2param.png', dpi = 300)
plt.clf()

# plot epoch vs auroc
epochs = range(1, len(auc_p) + 1)
plt.plot(epochs, auc_p_losses, 'bo', label = 'epoch vs average loss per epoch')
plt.legend()
plt.savefig('./pythonOutput/auc_loss_plot_2param.png', dpi = 300)
plt.clf()

# plot loss vs auc
plt.plot(auc_p_losses, auc_p, 'bo', label = 'loss vs auc')
plt.legend()
plt.savefig('./pythonOutput/aucVSloss_plot_2param.png', dpi = 300)
plt.clf()
