#!/usr/bin/env python
# -*- coding: utf-8 -*-
# @Date    : 2018-11-09 12:17:18
# @Author  : EricYichen (ericyichen@outlook.com)
# @Link    : ${link}
# @Version : $Id$

import pandas as pd
import numpy as np
from pandas_datareader import data

def get_data():
    symbols = pd.read_csv('~/Desktop/MAFN/Hedge Fund/presentation2/symbols.csv')
    df = pd.DataFrame()
    start = '2010-01-01'
    end = '2018-01-01'
    for symbol in symbols:
        try:
            mydata = data.DataReader(symbol, 'yahoo', start, end)
            path = '~/Desktop/MAFN/hedge fund/presentation2/data/' + symbol + '.csv'
            mydata.to_csv(path)
            df[symbol] = mydata['Close']
        except:
            continue
    return df

def train_test_split(df):
    return df[:'2017-01-01'], df['2017-01-01':]

def get_pairs(train):
    s = train.corr().abs().unstack().drop_duplicates()
    corr = s.sort_values()
    corr.to_csv('~/Desktop/MAFN/Hedge Fund/presentation2/correlation.csv')
    pairs = list(corr.index)[-200:-1]
    ans = []
    for pair in pairs:
        _, p_value, _ = coint(df[[pair[0]]], df[[pair[1]]])
        if p_value < 0.05:
            ans.append(pair)
    selected = corr[ans]
    selected.to_csv('highcorrelationandcointegrated.csv', header = None)
    return list(selected.index)

def calculate_signal(train, test, pairs):
    for pair in pairs:
        df = pd.DataFrame(index = test.index)
        ols = LinearRegression().fit(train[[pair[0]]], train[[pair[1]]])
        train_spread = train[[pair[1]]] - ols.predict(train[[pair[0]]])
        mean = np.mean(np.array(train_spread))
        std = np.std(np.array(train_spread))
        test_spread = test[[pair[1]]] - ols.predict(test[[pair[0]]])
        sig = (test_spread - mean) / std
        df['spread'] = test_spread
        df['signal'] = sig
        df[pair[0]] = test[pair[0]]
        df[pair[1]] = test[pair[1]]
        df['beta'] = ols.coef_[0][0]
        d = '~/Desktop/MAFN/Hedge fund/presentation2/data/' + ''.join(pair) + '.csv'
        df.to_csv(d)



def back_test(pair):
    d = '~/Desktop/MAFN/Hedge fund/presentation2/data/' + ''.join(pair) + '.csv'
    df1 = pd.read_csv(d)
    # df1.drop(labels=['Unnamed: 0'], axis=1, inplace=True)
    # df1.rename(columns={'trade.sig': 'trade_sig', 'hedge': 'spread'}, inplace=True)

    zEntries = np.arange(start=0.5,stop=2.5,step=0.1)
    exitZscore = 0

    cum_rets = pd.DataFrame()
    for z in zEntries:
        df1['long entry'] = ((df1.signal < -1.0 * z) & (df1.signal.shift(1) > -1.0 * z))
        df1['long exit'] = ((df1.signal > - exitZscore) & (df1.signal.shift(1) < - exitZscore))
        df1['num units long'] = np.nan
        df1.loc[df1['long entry'], 'num units long'] = 1
        df1.loc[df1['long exit'], 'num units long'] = 0
        df1['num units long'][0] = 0
        df1['num units long'] = df1['num units long'].fillna(method='pad')

        # set up num units short
        df1['short entry'] = ((df1.signal > z) & (df1.signal.shift(1) < z))
        df1['short exit'] = ((df1.signal < exitZscore) & (df1.signal.shift(1) > exitZscore))
        df1.loc[df1['short entry'], 'num units short'] = -1
        df1.loc[df1['short exit'], 'num units short'] = 0
        df1['num units short'][0] = 0
        df1['num units short'] = df1['num units short'].fillna(method='pad')

        df1['numUnits'] = df1['num units long'] + df1['num units short']
        df1['spread pct ch'] = (df1['spread'] - df1['spread'].shift(1)) / (
            (df1[pair[0]] * np.abs(df1['beta'])) + df1[pair[1]])
        df1['port rets'] = df1['spread pct ch'] * df1['numUnits'].shift(1)

        df1['cum rets'] = df1['port rets'].cumsum()
        df1['cum rets'] = df1['cum rets'] + 1

        if cum_rets.empty:
            cum_rets = pd.DataFrame(df1['cum rets'])
            cum_rets.rename(columns={'cum rets': 'cumRets_{}'.format(round(z,2))}, inplace=True)
        else:
            cum_rets = cum_rets.join(pd.DataFrame(df1['cum rets']), how="outer")
            cum_rets.rename(columns={'cum rets': 'cumRets_{}'.format(round(z,2))}, inplace=True)

    print("\nBACKTEST %s AND %s:\n" % (pair[0], pair[1]))
    try:
        print("2018 :\n")
        for i in cum_rets.columns:
            print(
                "Z-Score Entry {}: ".format(i[8:11]),
                str((cum_rets[i][len(cum_rets)-1] - cum_rets[i][1])*100)[0:4], "\b%"
            )
        maxCAGR = 0.0
        bestZ = ""
        for i in cum_rets.columns:
            # i = float(i[8:11])
            if float((cum_rets[i][len(cum_rets)-1] - cum_rets[i][1])*100) > float(maxCAGR):
                maxCAGR = (cum_rets[i][len(cum_rets)-1] - cum_rets[i][1])*100
                bestZ = i
        print("\nBest 1-yr return {} with {} ZScore Entry".format(
            str((cum_rets[bestZ][len(cum_rets)-1] - cum_rets[bestZ][1])*100)[0:4] + "%",
            bestZ[8:11])
        )
        print("1-yr Return {} with 1.0 ZScore Entry".format(
            str((cum_rets["cumRets_1.0"][len(cum_rets)-1] - cum_rets["cumRets_1.0"][1])*100)[0:4] + "%"
        ))
    except KeyError:
        print("KeyError")

    cum_rets.index = pd.to_datetime(df1['Date'])
    plt.plot(cum_rets[bestZ])
    plt.title("1 Year {} against {} Returns with {} Z".format(pair[0], pair[1], bestZ[8:11]))
    plt.savefig("{} {}.png".format(pair[0], pair[1]))
    plt.show()
    cumRets_1 = cum_rets['cumRets_1.0']
    cum_rets.drop(labels=[i for i in cum_rets.columns if i != bestZ], inplace=True, axis=1)
    if bestZ != "cumRets_1.0":
        cum_rets = cum_rets.join(pd.DataFrame(cumRets_1))
    cum_rets = cum_rets.join(df1['Date'])
    try:
        cum_rets.to_csv("%s %s Cumulative_Returns.csv" % (pair[0], pair[1]))
        print("Cumulative Returns CSV created.")
    except:
        print("Unable to save CSV.")


def process():
    dat = get_data()
    train, test = test_train_split(dat)
    pairs = get_pairs(train)
    calculate_signal(train, test, pairs)
    for pair in pairs:
        back_test(pair)

if __name__ == '__main__':
    process()




# def get_redundant_pairs(df):
#     '''Get diagonal and lower triangular pairs of correlation matrix'''
#     pairs_to_drop = set()
#     cols = df.columns
#     for i in range(0, df.shape[1]):
#         for j in range(0, i+1):
#             pairs_to_drop.add((cols[i], cols[j]))
#     return pairs_to_drop

# def get_top_abs_correlations(df, n=5):
#     au_corr = df.corr().abs().unstack()
#     labels_to_drop = get_redundant_pairs(df)
#     au_corr = au_corr.drop(labels=labels_to_drop).sort_values(ascending=False)
#     return au_corr[0:n]