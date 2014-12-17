#!/usr/bin/python

import getopt
import sys, os
from optparse import OptionParser
from datetime import datetime
from csv import DictReader
from math import exp, log, sqrt


##############################################################################
# Options #################################################################
##############################################################################


parser = OptionParser()
parser.add_option("-a", "--alpha", dest="alpha",default=0.1,type="float",action="store",
                  help="alpha value for the model: learning rate")
parser.add_option("-b", "--beta",dest="beta", default=1,type="float",action="store",
                  help="beta:smoothing parameter for adaptive learning rate ")
parser.add_option("-1", "--L1",dest="L1", default=12,type="float",action="store",
                  help="L1 regularization, larger value means more regularized")
parser.add_option("-2", "--L2",dest="L2", default=12,type="float",action="store",
                  help="# L2 regularization, larger value means more regularized")

(options, args) = parser.parse_args()


#print "alpha:" + str(options.alpha)
#print "beta:" + str(options.beta)
#print "L1:" + str(options.L1)
#print "L2:" + str(options.L2)
#print "outputfile Name:" + options.outputfile

##############################################################################
# parameters #################################################################
##############################################################################

# A, paths
train = '/Users/taposhdr/workspace/decision_science/kaggle/ctr_prediction/data/train'               # path to training file
test = '/Users/taposhdr/workspace/decision_science/kaggle/ctr_prediction/data/test'                 # path to testing file
submissionpath = '/Users/taposhdr/workspace/decision_science/kaggle/ctr_prediction/data/'  # path of to be outputted submission file


# B, model
alpha = options.alpha  # learning rate
beta = options.beta   # smoothing parameter for adaptive learning rate
L1 = options.L1     # L1 regularization, larger value means more regularized
L2 = options.L2     # L2 regularization, larger value means more regularized
outputfile = "submission-file" + "-alpha-"+str(alpha)+"-beta-"+str(beta)+"-L1-"+str(L1)+"-L2-"+str(L2)+".csv"
submission= submissionpath + outputfile



#print str(alpha)
#print str(beta)
#print str(L1)
#print str(L2)
#print submission

# C, feature/hash trick
D = 2 ** 22              # number of weights to use
do_interactions = False  # whether to enable poly2 feature interactions

# D, training/validation
epoch = 1      # learn training data for N passes
holdout = 29  # use every N training instance for holdout validation


##############################################################################
# class, function, generator definitions #####################################
##############################################################################

# each class below is a learning algorithm

class logistic_regression(object):
    ''' Classical logistic regression

        This class (algorithm) is not used in this code, it is here
        for a quick reference in hope to make the following (more complex)
        algorithm more understandable.
    '''

    def __init__(self, alpha, D, interaction=False):
        # parameters
        self.alpha = alpha

        # model
        self.w = [0.] * D

    def predict(self, x):
        # parameters
        alpha = self.alpha

        # model
        w = self.w

        # wTx is the inner product of w and x
        wTx = sum(w[i] for i in x)

        # bounded sigmoid function, this is the probability of being clicked
        return 1. / (1. + exp(-max(min(wTx, 35.), -35.)))

    def update(self, x, p, y):
        # parameter
        alpha = self.alpha

        # model
        w = self.w

        # gradient under logloss
        g = p - y

        # update w
        for i in x:
            w[i] += g * alpha


class ftrl_proximal(object):
    ''' Our main algorithm: Follow the regularized leader - proximal

        In short,
        this is an adaptive-learning-rate sparse logistic-regression with
        efficient L1-L2-regularization

        Reference:
        http://www.eecs.tufts.edu/~dsculley/papers/ad-click-prediction.pdf
    '''

    def __init__(self, alpha, beta, L1, L2, D, interaction=False):
        # parameters
        self.alpha = alpha
        self.beta = beta
        self.L1 = L1
        self.L2 = L2

        # feature related parameters
        self.D = D
        self.interaction = interaction

        # model
        # n: squared sum of past gradients
        # z: weights
        # w: lazy weights
        self.n = [0.] * D
        self.z = [0.] * D
        self.w = [0.] * D  # use this for execution speed up
        # self.w = {}  # use this for memory usage reduction

    def _indices(self, x):
        ''' A helper generator that yields the indices in x

            The purpose of this generator is to make the following
            code a bit cleaner when doing feature interaction.
        '''

        for i in x:
            yield i

        if self.interaction:
            D = self.D
            L = len(x)
            for i in xrange(1, L):  # skip bias term, so we start at 1
                for j in xrange(i+1, L):
                    yield (i * j) % D

    def predict(self, x):
        ''' Get probability estimation on x

            INPUT:
                x: features

            OUTPUT:
                probability of p(y = 1 | x; w)
        '''

        # parameters
        alpha = self.alpha
        beta = self.beta
        L1 = self.L1
        L2 = self.L2

        # model
        n = self.n
        z = self.z
        w = self.w  # use this for execution speed up
        # w = {}  # use this for memory usage reduction

        # wTx is the inner product of w and x
        wTx = 0.
        for i in self._indices(x):
            sign = -1. if z[i] < 0 else 1.  # get sign of z[i]

            # build w on the fly using z and n, hence the name - lazy weights -
            if sign * z[i] <= L1:
                # w[i] vanishes due to L1 regularization
                w[i] = 0.
            else:
                # apply prediction time L1, L2 regularization to z and get w
                w[i] = (sign * L1 - z[i]) / ((beta + sqrt(n[i])) / alpha + L2)

            wTx += w[i]

        self.w = w

        # bounded sigmoid function, this is the probability estimation
        return 1. / (1. + exp(-max(min(wTx, 35.), -35.)))

    def update(self, x, p, y):
        ''' Update model using x, p, y

            INPUT:
                x: feature, a list of indices
                p: click probability prediction of our model
                y: answer

            MODIFIES:
                self.n: increase by squared gradient
                self.z: weights
        '''

        # parameter
        alpha = self.alpha

        # model
        n = self.n
        z = self.z
        w = self.w  # no need to change this, it won't gain anything

        # gradient under logloss
        g = p - y

        # update z and n
        for i in self._indices(x):
            sigma = (sqrt(n[i] + g * g) - sqrt(n[i])) / alpha
            z[i] += g - sigma * w[i]
            n[i] += g * g


def logloss(p, y):
    ''' FUNCTION: Bounded logloss

        INPUT:
            p: our prediction
            y: real answer

        OUTPUT:
            logarithmic loss of p given y
    '''

    p = max(min(p, 1. - 10e-15), 10e-15)
    return -log(p) if y == 1. else -log(1. - p)


def data(path, D):
    ''' GENERATOR: Apply hash-trick to the original csv row
                   and for simplicity, we one-hot-encode everything

        INPUT:
            path: path to training or testing file
            D: the max index that we can hash to

        YIELDS:
            ID: id of the instance, mainly useless
            x: a list of hashed and one-hot-encoded 'indices'
               we only need the index since all values are either 0 or 1
            y: y = 1 if we have a click, else we have y = 0
    '''

    for t, row in enumerate(DictReader(open(path))):
        # process id
        ID = row['id']
        del row['id']

        # process clicks
        y = 0.
        if 'click' in row:
            if row['click'] == '1':
                y = 1.
            del row['click']

        # turn hour really into hour, it was originally YYMMDDHH
        row['hour'] = row['hour'][6:]

        # build x
        x = [0]  # 0 is the index of the bias term
        for key in sorted(row):  # sort is for preserving feature ordering
            value = row[key]

            # one-hot encode everything with hash trick
            index = abs(hash(key + '_' + value)) % D
            x.append(index)

        yield t, ID, x, y


##############################################################################
# start training #############################################################
##############################################################################

start = datetime.now()

# initialize ourselves a learner
learner = ftrl_proximal(alpha, beta, L1, L2, D, interaction=do_interactions)

# start training
for e in xrange(epoch):
    loss = 0.
    count = 0

    for t, ID, x, y in data(train, D):  # data is a generator
        #  t: just a instance counter
        # ID: id provided in original data
        #  x: features
        #  y: label (click)

        # step 1, get prediction from learner
        p = learner.predict(x)

        if t % holdout == 0:
            # step 2-1, calculate holdout validation loss
            #           we do not train with the holdout data so that our
            #           validation loss is an accurate estimation of
            #           the out-of-sample error
            loss += logloss(p, y)
            count += 1
        else:
            # step 2-2, update learner with label (click) information
            learner.update(x, p, y)

        if t % 2500000 == 0 and t > 1:
            print(' %s\tencountered: %d\tcurrent logloss: %f' % (
                datetime.now(), t, loss/count))

    print('Holdout logloss: %f,' % (loss/count) + ",Filename: " + outputfile )


##############################################################################
# start testing, and build Kaggle's submission file ##########################
##############################################################################

with open(submission, 'w') as outfile:
    outfile.write('id,click\n')
    for t, ID, x, y in data(test, D):
        p = learner.predict(x)
        outfile.write('%s,%s\n' % (ID, str(p)))
