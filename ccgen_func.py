# coding: utf-8
from ccgen_classdef import *

"""関数群"""

def readDiagrams(infile):
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
    """1-もしくは3-integer description（文字列形式）からTensorインスタンスを生成"""

    tensor = Tensor()
    T = T[0].split(",")

    if len(T) == 1:    # interaction vertex or lambda (DM case)
        T = int(T[0])
        if T == 1:
            tensor.setup(1,0,1,0,0,0,"G")
        elif T == 2:
            tensor.setup(0,1,0,1,0,0,"G")
        elif T == 3:
            tensor.setup(0,0,1,1,0,0,"G")
        elif T == 4:
            tensor.setup(2,0,2,0,0,0,"G")
        elif T == 5:
            tensor.setup(0,2,0,2,0,0,"G")
        elif T == 6:
            tensor.setup(1,1,1,1,0,0,"G")
        elif T == 7:
            tensor.setup(1,0,2,1,0,0,"G")
        elif T == 8:
            tensor.setup(0,1,1,2,0,0,"G")
        elif T == 9:
            tensor.setup(2,1,1,0,0,0,"G")
        elif T == 10:
            tensor.setup(1,2,0,1,0,0,"G")
        elif T == 11:
            tensor.setup(0,0,2,2,0,0,"G")
        elif T == 12:
            tensor.setup(1,1,0,0,0,0,"G")
        elif T == 13:
            tensor.setup(2,2,0,0,0,0,"G")
#        elif T < 0:
#            # Lambda amplitude in response density-matrix
    elif len(T) == 3:  # T or lambda amplitude
        for i in range(3):
            T[i] = int(T[i])
        if T[0] < 0: typ = "L"
        elif T[0] == 0: typ = ""
        else: typ = "T"
        nC = T[2]
        nK = T[1] - T[2]
        nA = abs(T[0]) - nC
        nI = abs(T[0]) - nK
        tensor.setup(nA, nI, nC, nK, 0, 0, typ)

    return tensor

    
def setupDiagrams(D):
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



def factorizeDiagrams(Diagrams):
    """Diagramsインスタンスのリストをfactorize"""

    # 一意なテンソルをディクショナリ形式で抽出
    Tdict = uniqueTensors(Diagrams)

#    # 確認のため出力
#    for T1 in Tdict[("root",)]:
#        print(T1.show())
#        for T2 in Tdict[("root",T1)]:
#            print("---"+T2.show())
#            for T3 in Tdict[("root",T1,T2)]:
#                print("---"*2+T3.show())
#                for T4 in Tdict[("root",T1,T2,T3)]:
#                    print("---"*3+T4.show())
#                    for T5 in Tdict[("root",T1,T2,T3,T4)]:
#                        print("---"*4+T5.show())

    # 上と同じことを再帰的に実行（練習用）
    tmp(Tdict, ["root"], 0)


    # Tdictを使って中間体をディクショナリ形式で定義
    InterDict = defineIntermediates(Diagrams, Tdict)

    # 確認のため出力
#    InterDict[("root",)].display()

    return InterDict



def tmp(Tdict, key, indent):
    """Tdictのツリー構造を再帰的に出力"""

    if len(key) > 5: return
    for T in Tdict[tuple(key)]:
        print("---"*indent, T.show())
        key.append(T)
        tmp(Tdict, key, indent+1)
        key.pop()



def uniqueTensors(Diagrams):
    """一意なテンソルのディクショナリを返す"""

    Tdict = {}

    Tdict[("root",)] = []
    for diagram in Diagrams:
        if not diagram.T1 in Tdict[("root",)]: Tdict[("root",)].append(diagram.T1)

    for T1 in Tdict[("root",)]:
        Tdict[("root",T1)] = []
        for diagram in Diagrams:
            if diagram.T1 == T1:
                if not diagram.T2 in Tdict[("root",T1)]:
                    Tdict[("root",T1)].append(diagram.T2)

    for T1 in Tdict[("root",)]:
        for T2 in Tdict[("root",T1)]:
            Tdict[("root",T1,T2)] = []
            for diagram in Diagrams:
                if diagram.T1 == T1 and diagram.T2 == T2:
                    if not diagram.T3 in Tdict[("root",T1,T2)]:
                        Tdict[("root",T1,T2)].append(diagram.T3)

    for T1 in Tdict[("root",)]:
        for T2 in Tdict[("root",T1)]:
            for T3 in Tdict[("root",T1,T2)]:
                Tdict[("root",T1,T2,T3)] = []
                for diagram in Diagrams:
                    if diagram.T1 == T1 and diagram.T2 == T2 and diagram.T3 == T3:
                        if not diagram.T4 in Tdict[("root",T1,T2,T3)]:
                            Tdict[("root",T1,T2,T3)].append(diagram.T4)

    for T1 in Tdict[("root",)]:
        for T2 in Tdict[("root",T1)]:
            for T3 in Tdict[("root",T1,T2)]:
                for T4 in Tdict[("root",T1,T2,T3)]:
                    Tdict[("root",T1,T2,T3,T4)] = []
                    for diagram in Diagrams:
                        if diagram.T1 == T1 and diagram.T2 == T2 and diagram.T3 == T3 and diagram.T4 == T4:
                            if not diagram.T5 in Tdict[("root",T1,T2,T3,T4)]:
                                Tdict[("root",T1,T2,T3,T4)].append(diagram.T5)

    return Tdict


def defineIntermediates(Diagrams, Tdict):
    """ダイグラムリストと一意なテンソルのディクショナリから、中間体を定義"""

    InterDict = {}

    I1list = []
    for T1 in Tdict[("root",)]:
        I2list = []
        for T2 in Tdict[("root",T1)]:
            I3list = []
            for T3 in Tdict[("root",T1,T2)]:
                I4list = []
                for T4 in Tdict[("root",T1,T2,T3)]:
                    I5list = []
                    for T5 in Tdict[("root",T1,T2,T3,T4)]:
                        tmp = Intermediate(None, T5)
                        InterDict[("root",T1,T2,T3,T4,T5)] = tmp
                        I5list.append(tmp)
                    tmp = Intermediate(I5list, T4)
                    InterDict[("root",T1,T2,T3,T4)] = tmp
                    I4list.append(tmp)
                tmp = Intermediate(I4list, T3)
                InterDict[("root",T1,T2,T3)] = tmp
                I3list.append(tmp)
            tmp = Intermediate(I3list, T2)
            InterDict[("root",T1,T2)] = tmp
            I2list.append(tmp)
        tmp = Intermediate(I2list, T1)
        InterDict[("root",T1)] = tmp
        I1list.append(tmp)
    InterDict[("root",)] = Intermediate(I1list, None)
    

    return InterDict
    


def generateCode(InterDict, fout):
    """ソースコード生成"""

    # 一時ファイルを定義
    fout_tmp1 = open("header", "w")
    fout_tmp2 = open("variables", "w")
    fout_tmp3 = open("executions", "w")
    tmpfileList = [fout_tmp1, fout_tmp2, fout_tmp3]

    # ヘッダ（コメント、使用モジュール）
#    genSrcHeader(fout_tmp1)

    # メインコードを一時ファイルに書き出し（変数のリストも作成）
    vList = genSrcExec(InterDict, ["root"], fout_tmp3)

    # 変数定義を一時ファイルに書き出し
#    genSrcVar(vList, fout_tmp2)

    # 一時ファイルを統合


    # フッタ



def genSrcExec(interDict, key, fout):
    """メイン実行ソースを再帰的に生成"""

    level = len(key)
    myself = interDict[tuple(key)]  # current intermediate
    children = myself.children      # children of current intermediate

    if children == None:
        return
    else:
        name = "V"+str(level)
#        initArray(myself, name, fout, level)
        for child in children:
            key.append(child.mytensor)
            genSrcExec(interDict, key, fout)
            key.pop()
            contraction(child, fout, level)

    return


    
def initArray(inter, name, fout, level):
    """配列初期化コードの生成"""

    fout.write("---"*level+"Initialize "+name+"("+inter.to_string()+")"+"\n")

    # 配列の動的確保とNTChemの配列ゼロクリアルーチン（配列の名前とサイズが必要）


def contraction(inter, fout, level):
    """Contractionコードの呼び出し"""

    V1 =   "V"+str(level)+"("+inter.typ+")" #inter.to_string()
    T1 = inter.mytensor
    if inter.children == None:   # 下位の中間体が無い場合はmytensorを読み込む
        task = " += "+T1.show()
    else:
        V2 =  "V"+str(level+1) + "(" + inter.children[0].to_string() + ")"
        if T1.zero(): # mytensorがゼロの場合は下位中間体をそのままコピー
            task = " += "+V2
        else: # mytensorと下位中間体をcontract            
            task = " += "+V2+"*"+T1.show()
                
    fout.write("---"*level+V1+task+"\n")
    


def contraction_kernel(V1, V2, T):
    """Contraction: V += V2 * T"""

    # とりあえずエネルギー方程式（GT + Tタイプ）

    # decide if rearrangement is needed
    
    # if needed: call rearrange routine
    # get name of (rearranged) intermediate file

    # contract

    # open subroutine file
    
    # It loop
    # At loop
    # initialize F
    # J loop
    # B loop
    # make F from T
    # B loop end
    # J loop end
    # Io loop
    # Ao loop
    # V1 += V2 * F
    # Ao loop end
    # Io loop end
    # At loop end
    # It loop end

    # close subroutine file


#def contraction(T1, T2, V):
#    """contraction: V += T1 * T2"""
#
#    # contractionのタイプごとに個別関数を実行
#    typ1 = T1.typ
#    typ2 = T2.typ
#
#    if   typ1 == "T" and (typ2 == "I" or typ2 == "IT"):
#        Contraction1(T1, T2, V)
#    elif typ1 == "" and typ2 == "":
#        #
#    elif typ1 == "" and typ2 == "":
#        #
#    elif typ1 == "" and typ2 == "":
#        #
#    elif typ1 == "" and typ2 == "":
#        #
#    elif typ1 == "" and typ2 == "":
#        #
#
#
#
#def Contraction1(T, W, V):
#    """T + IT type contraction"""
#
#    # W~ intermediate
#    nB = W.nC - V.nC
#    nJ = W.nK - V.nK
#    W_ = Tensor()
#    W_.setup(W.nA, W.nI, W.nC-nB, W.nK-nJ, nB, nJ, W.typ)
#    
#    # rearrange W
#    Rearrange(W, W_)
#
#    # contraction
#
#
#
#    
#
#def Contraction2(T1, T2, V):
#    """ type contraction"""
#
#    
#def Contraction3(T1, T2, V):
#    """ type contraction"""
#
#    
#def Contraction4(T1, T2, V):
#    """ type contraction"""
#
#    
#def rearrange(T1, T2):
#    """Rearrange tensor"""
#
#    if nI > 0: # write DO AddI = 1, LenStringOcc
#    if nA > 0: # write DO AddA = 1, LenStringVir
#
