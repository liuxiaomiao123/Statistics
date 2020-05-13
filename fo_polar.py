# 注意要将mean值缩短到小数点后5位，否则会溢出

#coding: utf-8
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.collections import LineCollection


def result_pic(result):
    """
    雷达图的绘制
    :param result: 分类数据
    :return: 雷达图
    """
    # 解析出类别标签和种类
    labels = ['PCC', 'vmPFC', 'AG_R', 'AG_L', 'hipp_L','hipp_R','dlPFC_R','dlPFC_L','IPS_R','IPS_L','dACC','insula_R','insula_L','amygdala_R','amygdala_L','striatum_R','striatum_L']
    #labels = ['PCC', 'vmPFC', 'AG_R', 'AG_L', 'dlPFC_R', 'dlPFC_L', 'IPS_R', 'IPS_L',
           #   'dACC', 'insula_R', 'insula_L']
    kinds = list(result.iloc[:, 0])

    # 由于在雷达图中，要保证数据闭合，这里就再添加L列，并转换为 np.ndarray
    result = pd.concat([result, result[['PCC']]], axis=1)
   # result = pd.concat([result, result[['insula_L']]], axis=1)
    centers = np.array(result.iloc[:, 1:])

    # 分割圆周长，并让其闭合
    n = len(labels)
    angle = np.linspace(0, 2 * np.pi, n, endpoint=False)
    angle = np.concatenate((angle, [angle[0]]))

    # 绘图
    fig = plt.figure()
    ax = fig.add_subplot(111, polar=True)  # 参数polar, 以极坐标的形式绘制图形

    # 画线
    for i in range(len(kinds)):
         ax.plot(angle, centers[i], linewidth=2, label=kinds[i])
      #  lc = LineCollection(angle, linewidths=2, color=color)
      #  ax.add_collection(lc)
        # ax.fill(angle, centers[i])  # 填充底色

    # 添加属性标签
    ax.set_thetagrids(angle * 180 / np.pi, labels,fontsize =10)
    ax.set_rlim(-0.7,0)
    plt.title('')
    plt.legend(loc='lower right')
    plt.savefig('D://brainbnu//brain_software//ShareFolders//CHS_project//Final2//HMM_result_9_7//k=10//k=10_1//mean//10.png')
    plt.show()



if __name__ == '__main__':
    result = pd.read_csv('D:/brainbnu/brain_software/ShareFolders/CHS_project/Final2/HMM_result_9_7/k=10/k=10_1/mean/mean.csv', sep=',')
    result_pic(result)


