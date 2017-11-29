# codeing: utf-8
from ccgen_classdef import *

"""関数群"""

def ReadDiagrams(infile):
    """ファイルからダイアグラムを読み取ってリストに保存
    ファイル形式：
    L1:m11,m12,m13/m21,m22,m23/m31,m32,m33/m41,m42,m43/m5/pw
    L2: ....
    """

    f = open(infile, "r")
    D = []
    for line in f:
        tmp = line.replace(" ","").replace("\n","").split("/")
        T1 = tmp[0].split()
        T2 = tmp[1].split()
        T3 = tmp[2].split()
        T4 = tmp[3].split()
        T5 = tmp[4].split()
        pw = tmp[5]
        D.append([T1,T2,T3,T4,T5,pw])
    f.close()
    return D
    

def SetupTensor(T):
    """13-integer description(string)からTensorインスタンスを生成"""

    tensor = Tensor()
    T = T[0].split(",")

    if len(T) == 1:    # interaction vertex or lambda (DM case)
        T = int(T[0])
        if T == 1:
            tensor.setup(1,0,1,0,0,0,"G")
        elif T == 2:
            tensor.setup(0,1,0,1,0,0,"G")
        elif T == 3:
            tensor.setup(1,1,0,0,0,0,"G")
        elif T == 4:
            tensor.setup(2,0,2,0,0,0,"G")
        elif T == 5:
            tensor.setup(0,2,0,2,0,0,"G")
        elif T == 6:
            tensor.setup(1,1,1,1,0,0,"G")
        elif T == 7:
            tensor.setup(2,1,1,0,0,0,"G")
        elif T == 8:
            tensor.setup(1,2,0,1,0,0,"G")
        elif T == 9:
            tensor.setup(1,0,2,1,0,0,"G")
        elif T == 10:
            tensor.setup(0,1,1,2,0,0,"G")
        elif T == 11:
            tensor.setup(2,2,0,0,0,0,"G")
        elif T == 12:
            tensor.setup(0,0,1,1,0,0,"G")
        elif T == 13:
            tensor.setup(0,0,2,2,0,0,"G")
#        elif T < 0:
#            # Lambda amplitude in response density-matrix
    elif len(T) == 3:  # T or lambda amplitude
        for i in range(3):
            T[i] = int(T[i])
        if T[0] < 0: typ = "L"
        else: typ = "T"
        nC = T[2]
        nK = T[1] - T[2]
        nA = abs(T[0]) - nC
        nI = abs(T[0]) - nK
        tensor.setup(nA, nI, nC, nK, 0, 0, typ)

    return tensor

    
def SetupDiagrams(D):
    """読み込んだダイアグラムリストからDiagramインスタンスのリストを作成"""

    Diagrams = []
    for diag13 in D:
        T1 = SetupTensor(diag13[0])
        T2 = SetupTensor(diag13[1])
        T3 = SetupTensor(diag13[2])
        T4 = SetupTensor(diag13[3])
        T5 = SetupTensor(diag13[4])
        pw = int(diag13[5])
        diagram = Diagram(T1, T2, T3, T4, T5, pw)
        Diagrams.append(diagram)

    return Diagrams    


def FactorizeDiagrams(Diagrams):
    """DiagramインスタンスのリストからDiagramGroupインスタンスのリストを作成"""

    DiagramGroups = []

    # 一意なT1をリストアップ
    T1set = [Diagrams[0].T1]
    for diagram in Diagrams:
        if not (diagram.T1 in T1set): T1set.append(diagram.T1)

    # DiagramGroupインスタンスのリストを作成
    DGroups = []
    for T1 in T1set:
        DList = []
        for diagram in Diagrams:
            if diagram.T1 == T1: DList.append(diagram)
        DGroup = DiagramGroup(DList) # 初期化
        DGroup.setup()               # group内でfactorization
        DGroups.append(DGroup)
    
    return DGroups


def InitArray(name, size):
    """配列初期化コードの生成"""

    
