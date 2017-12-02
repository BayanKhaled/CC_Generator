# codeing: utf-8
import sys
from ccgen_func import *
from ccgen_classdef import *

# CC code generator #

"""メインプログラム"""

# 最大励起レベル、射影先manifoldの粒子/hole数をコマンド引数から取得
args = sys.argv
NMax = int(args[1])
ManifoldLv = int(args[2])
if NMax < ManifoldLv:
    sys.exit("Error: manifold level cannot be larger than NMax")

# 生成ソースファイル名の作成
mainsrc = "cc_"
if NMax == 1: mainsrc += "ccs_"    
elif NMax == 2: mainsrc += "ccsd_"
elif NMax == 3: mainsrc += "ccsdt_"
elif NMax == 4: mainsrc += "ccsdq_"

if ManifoldLv == 1: mainsrc += "t1_autogen.f90"
elif ManifoldLv == 2: mainsrc += "t2_autogen.f90"
elif ManifoldLv == 3: mainsrc += "t3_autogen.f90"
elif ManifoldLv == 4: mainsrc += "t4_autogen.f90"


# メインソースファイルを開く
fout = open(mainsrc, "w")

# ソート済みダイアグラムのリストをファイルから読み込む
D = ReadDiagrams("diagrams.sort")

# 読み込んだダイアグラムからインスタンス作成
Diagrams = SetupDiagrams(D)

# ダイグラムをfactorize
DGroups = FactorizeDiagrams(Diagrams)

# main loop #
for DGroup in DGroups:
    T1 = DGroup.T1
    InitArray("V2", DGroup.V2size)
    for T2 in DGroup.T2List:
        InitArray("V3", DGroup.V3size[T1,T2])
        for T3 in DGroup.T3Dict[T2]:
            InitArray("V4", DGroup.V4size[T1,T2,T3])
            for T4 in DGroup.T4Dict[(T2,T3)]:
                T5 = DGroup.T5Dict[(T2,T3,T4)]
                Contraction(T5, T4, V4)
            Contraction(T4, T3, V3)
        Contraction(T3, T2, V2)
    Contraction(T2, T1, V1)


# close main source file
fout.close()
