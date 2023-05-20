import os
import numpy as np
import pandas as pd
import json
from sklearn.model_selection import train_test_split as _split

from warnings import simplefilter
from sklearn.exceptions import ConvergenceWarning

current_folder = os.path.dirname(os.path.abspath(__file__))

simplefilter("ignore", category=ConvergenceWarning)


class Dataset:
    X_train = None
    y_train = None
    X_valid = None
    y_valid = None
    X_test = None
    y_test = None

    def __init__(self, data, labels, test_ratio=0.3, valid_ratio=0.0, random_seed=4622, shuffle=True):
        self.X = data
        self.y = labels
        if test_ratio > 0.0:
            self.X_train, self.X_test, self.y_train, self.y_test = _split(data, labels,
                                                                          shuffle=shuffle,
                                                                          test_size=test_ratio,
                                                                          random_state=random_seed)
        else:
            self.X_train, self.y_train = data, labels
        if valid_ratio > 0.0:
            self.X_train, self.X_valid, self.y_train, self.y_valid = _split(self.X_train, self.y_train,
                                                                            shuffle=shuffle,
                                                                            test_size=valid_ratio,
                                                                            random_state=random_seed)


class Concrete(Dataset):
    def __init__(self):
        rawdata = pd.read_csv('data/concrete_data.csv').to_numpy()
        super(Concrete, self).__init__(rawdata[:, :-1], rawdata[:, -1], test_ratio=0.2, random_seed=4622)


class BinaryPrices(Dataset):

    def __init__(self):
        rawdata = pd.read_csv('data/kc_house_data.csv').drop(['id', 'date', 'yr_renovated', 'zipcode', 'lat', 'long',
                                                              'sqft_living15', 'sqft_lot15'],
                                                             axis=1).to_numpy()
        np.random.seed(4622)
        rawdata = rawdata[np.random.choice(len(rawdata), 2000, replace=False)]
        median = np.median(rawdata[:, 0])
        rawdata[:, 0] = (rawdata[:, 0] <= median) * 1
        super(BinaryPrices, self).__init__(rawdata[:, 1:], rawdata[:, 0], test_ratio=0.2)
