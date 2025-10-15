import torch.optim as optim
import math
import util
import numpy as np
import torch


class DoubleTrainer():
    def __init__(self, model, lrate, wdecay, clip, step_size, seq_out_len, device, predA=True):
        self.model = model
        self.model.to(device)
        self.optimizer = optim.Adam(self.model.parameters(), lr=lrate, weight_decay=wdecay)
        self.loss = util.masked_mae
        self.clip = clip
        self.step = step_size
        self.iter = 1
        self.task_level = 1
        self.seq_out_len = seq_out_len
        self.predA = predA

    def train(self, input, real_val, idx, data_name):
        self.model.train()
        self.optimizer.zero_grad()
        
        output = self.model(input)
        real = real_val 
        predict = output
        predict = torch.squeeze(predict, (3, 4))
        predict = predict.transpose(2, 1)
        if self.iter % self.step == 0 and self.task_level <= self.seq_out_len:
            self.task_level += 1
        loss = self.loss(predict, real)
        loss.backward()
        if self.clip is not None:
            torch.nn.utils.clip_grad_norm_(self.model.parameters(), self.clip)
        self.optimizer.step()
        mae = []
        mape = []
        rmse = []
        for i in range(predict.shape[1]):
            mae.append(util.masked_mae(predict[:, i, :], real[:, i, :]).item())
            rmse.append(util.masked_rmse(predict[:, i, :], real[:, i, :]).item())
            mape.append(util.masked_mape(predict[:, i, :], real[:, i, :], null_val=0.0).item())
        self.iter += 1
        return loss.item(), np.mean(mae), np.mean(mape), np.mean(rmse)

    def eval(self, input, real_val, data_name):
        self.model.eval()
        output = self.model(input)
        real = real_val
        predict = output
        mae = []
        mape = []
        rmse = []
        for i in range(predict.shape[1]):
            mae.append(util.masked_mae(predict[:, i, :], real[:, i, :]).item())
            rmse.append(util.masked_rmse(predict[:, i, :], real[:, i, :]).item())
            mape.append(util.masked_mape(predict[:, i, :], real[:, i, :], null_val=0.0).item())
        return np.mean(mae), np.mean(mape), np.mean(rmse)
        
    def evalbest(self, input, real_val, data_name):
        self.model.eval()
        output = self.model(input)
        real = real_val
        predict = output
        mae = []
        mape = []
        rmse = []
        for i in range(predict.shape[1]):
            mae.append(util.masked_mae(predict[:, i, :], real[:, i, :]).item())
            rmse.append(util.masked_rmse(predict[:, i, :], real[:, i, :]).item())
            mape.append(util.masked_mape(predict[:, i, :], real[:, i, :], null_val=0.0).item())
        return np.mean(mae), np.mean(mape), np.mean(rmse), predict

class Optim(object):
    def _makeOptimizer(self):
        if self.method == 'sgd':
            self.optimizer = optim.SGD(self.params, lr=self.lr, weight_decay=self.lr_decay)
        elif self.method == 'adagrad':
            self.optimizer = optim.Adagrad(self.params, lr=self.lr, weight_decay=self.lr_decay)
        elif self.method == 'adadelta':
            self.optimizer = optim.Adadelta(self.params, lr=self.lr, weight_decay=self.lr_decay)
        elif self.method == 'adam':
            self.optimizer = optim.Adam(self.params, lr=self.lr, weight_decay=self.lr_decay)
        else:
            raise RuntimeError("Invalid optim method: " + self.method)

    def __init__(self, params, method, lr, clip, lr_decay=1, start_decay_at=None):
        self.params = params  # careful: params may be a generator
        self.last_ppl = None
        self.lr = lr
        self.clip = clip
        self.method = method
        self.lr_decay = lr_decay
        self.start_decay_at = start_decay_at
        self.start_decay = False

        self._makeOptimizer()

    def step(self):
        # Compute gradients norm.
        grad_norm = 0
        if self.clip is not None:
            torch.nn.utils.clip_grad_norm_(self.params, self.clip)
        self.optimizer.step()
        return grad_norm

    # decay learning rate if val perf does not improve or we hit the start_decay_at limit
    def updateLearningRate(self, ppl, epoch):
        if self.start_decay_at is not None and epoch >= self.start_decay_at:
            self.start_decay = True
        if self.last_ppl is not None and ppl > self.last_ppl:
            self.start_decay = True

        if self.start_decay:
            self.lr = self.lr * self.lr_decay
            print("Decaying learning rate to %g" % self.lr)
        #only decay for one epoch
        self.start_decay = False

        self.last_ppl = ppl

        self._makeOptimizer()
