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
D = readDiagrams("diagrams.sort")

# 読み込んだダイアグラムからインスタンス作成
Diagrams = setupDiagrams(D)

## （仮）ゼロテンソルを削除して左詰めにする
#for diagram in Diagrams:
#    diagram.compact()

# ダイアグラムをfactorize
InterDict = factorizeDiagrams(Diagrams)

# ソースコード生成
generateCode(InterDict, fout)

# close main source file
fout.close()
