import pickle
import numpy as np
import os
import scipy.sparse as sp
import torch
from scipy.sparse import linalg
from torch.autograd import Variable
from helper import *
import sys


def normal_std(x):
    return x.std() * np.sqrt((len(x) - 1.)/(len(x)))


class DataLoaderM(object):
    def __init__(self, xs, ys, batch_size, pad_with_last_sample=True, is_windows=False):
        """
        :param xs:
        :param ys:
        :param batch_size:
        :param pad_with_last_sample: pad with the last sample to make number of samples divisible to batch_size.
        """
        self.batch_size = batch_size
        self.current_ind = 0

        if pad_with_last_sample:
            num_padding = (batch_size - (len(xs) % batch_size)) % batch_size # 求补足的个数
            x_padding = np.repeat(xs[-1:], num_padding, axis=0)
            y_padding = np.repeat(ys[-1:], num_padding, axis=0)
            xs = np.concatenate([xs, x_padding], axis=0)
            ys = np.concatenate([ys, y_padding], axis=0)
        self.size = len(xs)
        self.num_batch = int(self.size // self.batch_size)
        self.xs = xs
        self.ys = ys

    def shuffle(self):
        permutation = np.random.permutation(self.size)
        xs, ys = self.xs[permutation], self.ys[permutation]
        self.xs = xs
        self.ys = ys

    def get_iterator(self):
        self.current_ind = 0

        def _wrapper():
            while self.current_ind < self.num_batch:
                start_ind = self.batch_size * self.current_ind
                end_ind = min(self.size, self.batch_size * (self.current_ind + 1))
                x_i = self.xs[start_ind: end_ind, ...]
                y_i = self.ys[start_ind: end_ind, ...]
                yield x_i, y_i
                self.current_ind += 1

        return _wrapper()


def masked_mae(preds, labels, null_val=np.nan):
    if np.isnan(null_val):
        mask = ~torch.isnan(labels)
    else:
        mask = (labels!=null_val)
    mask = mask.float()
    mask /= torch.mean((mask))
    mask = torch.where(torch.isnan(mask), torch.zeros_like(mask), mask)
    loss = torch.abs(preds-labels)
    loss = loss * mask
    loss = torch.where(torch.isnan(loss), torch.zeros_like(loss), loss)
    return torch.mean(loss)

def masked_rmse(preds, labels, null_val=np.nan):
    return torch.sqrt(masked_mse(preds=preds, labels=labels, null_val=null_val))

def masked_mse(preds, labels, null_val=np.nan):
    if np.isnan(null_val):
        mask = ~torch.isnan(labels)
    else:
        mask = (labels!=null_val)
    mask = mask.float()
    mask /= torch.mean((mask))
    mask = torch.where(torch.isnan(mask), torch.zeros_like(mask), mask)
    loss = (preds-labels)**2
    loss = loss * mask
    loss = torch.where(torch.isnan(loss), torch.zeros_like(loss), loss)
    return torch.mean(loss)

def masked_mape(preds, labels, null_val=np.nan):
    if np.isnan(null_val):
        mask = ~torch.isnan(labels)
    else:
        mask = (labels!=null_val)
    mask = mask.float()
    mask /= torch.mean((mask))
    mask = torch.where(torch.isnan(mask), torch.zeros_like(mask), mask)
    loss = torch.abs(preds-labels)/labels
    loss = loss * mask
    loss = torch.where(torch.isnan(loss), torch.zeros_like(loss), loss)
    # loss = torch.nan_to_num(loss)
    return torch.mean(loss)

def metric_avg(pred, real, single=False):
    mae = []
    mape = []
    rmse = []
    for i in range(pred.shape[1]):
        mae.append(masked_mae(pred[:, i, :], real[:,  i, :]).item())
        rmse.append(masked_rmse(pred[:, i, :], real[:, i, :]).item())
        mape.append(masked_mape(pred[:, i, :], real[:, i, :], null_val=0.0).item())
    return np.mean(mae), np.mean(mape), np.mean(rmse)

def metrics_ori(pred, real):
    mae = masked_mae(pred, real).item()
    mape = masked_mape(pred, real).item()
    rmse = masked_rmse(pred, real, 0.0).item()
    return mae, mape, rmse

def metric(pred, real, single_flag=False, single_var=False):
    mae = masked_mae(pred,real, 0.0).item()
    mape = masked_mape(pred,real, 0.0).item()
    if single_flag:
        rmse = masked_rmse_mean(pred, real, 0.0).item()
    elif single_var:
        rmse = masked_rmse(pred, real, 0.0).item()
    else:
        rmse = masked_stat_rmse_mean(pred, real, 0.0).item()

    return mae,mape,rmse