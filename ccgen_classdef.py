# coding: utf-8

class Tensor:
    """Tensor class"""

    def __init__(self):
        self.nA = 0
        self.nI = 0
        self.nC = 0
        self.nK = 0
        self.nB = 0
        self.nJ = 0
        self.typ = ""

        
    def setup(self, nA, nI, nC, nK, nB, nJ, typ):
        self.nA = nA
        self.nI = nI
        self.nC = nC
        self.nK = nK
        self.nB = nB
        self.nJ = nJ
        self.typ = typ

        
    def none(self):
        """6つのindexが全てゼロならnone（何もなし）"""
        if (nA == 0 and nI == 0 and nC == 0 and nK == 0 and nB == 0 and nJ == 0):
            return True
        else:
            return False

        
    def __eq__(self, T):
        """Tensorの==演算子"""

        if (self.nA == T.nA and self.nI == T.nI and self.nC == T.nC and self.nK == T.nK and self.nB == T.nB and self.nJ == T.nJ and self.typ == T.typ):
            return True
        else:
            return False


    def __hash__(self):
        """hash関数（Tensorインスタンスをディクショナリのキーに使うため）"""
        return 1


    def to_string(self):
        """出力用に文字列に変換"""
        return self.typ+str(self.nA)+","+str(self.nI)+","+str(self.nC)+","+str(self.nK)


    def show(self):
        """テンソルを出力"""
        return self.to_string()


class Diagram:
    """Diagram class"""

    def __init__(self, T1, T2, T3, T4, T5, weight):
        self.T1 = T1
        self.T2 = T2
        self.T3 = T3
        self.T4 = T4
        self.T5 = T5
#        self.weight = weight      # parity (sign) is involved

        
    def compact(self):
        """000テンソルを削除し、左詰めにする"""
        if (self.T4.none) :
            self.T4 = self.T5
        if (self.T3.none):
            self.T3 = self.T4
            self.T4 = self.T5
        if (self.T2.none):
            self.T2 = self.T3
            self.T3 = self.T4
            self.T4 = self.T5
        if (self.T1.none):
            self.T1 = self.T2
            self.T2 = self.T3
            self.T3 = self.T4
            self.T4 = self.T5

        return self


#    def set_weight_and_parity(self):
#        """等価なTensor対の数から重み因子を求め、メンバ変数にセット
#        Parityを求め、メンバ変数にセット"""
#
#        # Weight factor
#        weight = 0
#        tmp = [self.T1, self.T2, self.T3, self.T4, self.T5]
#        for T1 in tmp:
#            if T1.none: continue
#            for T2 in tmp[:tmp.index(T1)]:
#                if T1 == T2: weight += 1
#
#        self.weight = weight
#
#        # Parity
#        parity = 0
#
#        self.pariry = parity
                

    def show(self):
        """ダイアグラムを出力"""
        return self.T1.to_string()+"/"+self.T2.to_string()+"/"+ \
    self.T3.to_string()+"/"+self.T4.to_string()+"/"+self.T5.to_string()
        


class DiagramGroup:
    """Factorized diagram group"""

    def __init__(self, DiagramList):
        self.DiagramList = DiagramList
        self.nDiagram = len(DiagramList)

        
    def __setupIntermediates__(self):
        """ダイアグラム中のゼロテンソルを削除し、左詰め"""
        for diagram in self.DiagramList:
            diagram = diagram.compact()


    def setup(self):
        """ インスタンス内でFactorizationを行い、メンバ変数を決定
        __init__の一部にしてもいいかも"""

        # T1を取得
        self.T1 = self.DiagramList[0].T1

        # 一意なT2のリストを取得
        self.T2List = []
        for diagram in self.DiagramList:
            if not diagram.T2 in self.T2List: self.T2List.append(diagram.T2)

        # 一意なT3のディクショナリを作成
        self.T3Dict = {}
        for T2 in self.T2List:
            self.T3Dict[T2] = []
            for diagram in self.DiagramList:
                if diagram.T2 == T2:
                    if not diagram.T3 in self.T3Dict[T2]:
                        self.T3Dict[T2].append(diagram.T3)

        # 一意なT4のディクショナリを作成
        self.T4Dict = {}
        for T2 in self.T2List:
            for T3 in self.T3Dict[T2]:
                self.T4Dict[(T2,T3)] = []
                for diagram in self.DiagramList:
                    if diagram.T2 == T2 and diagram.T3 == T3:
                        if not diagram.T4 in self.T4Dict[(T2,T3)]:
                            self.T4Dict[(T2,T3)].append(diagram.T4)

        # 一意なT5のディクショナリを作成
        self.T5Dict = {}
        for T2 in self.T2List:
            for T3 in self.T3Dict[T2]:
                for T4 in self.T4Dict[(T2,T3)]:
                    self.T5Dict[(T2,T3,T4)] = []
                    for diagram in self.DiagramList:
                        if diagram.T2 == T2 and diagram.T3 == T3 and diagram.T4 == T4:
                            if not diagram.T5 in self.T5Dict[(T2,T3,T4)]:
                                self.T5Dict[(T2,T3,T4)].append(diagram.T5)

        # 結果確認（デバッグ用）
        print(self.T1.show())
        for T2 in self.T2List:
            print("---"+T2.show())
            for T3 in self.T3Dict[T2]:
                print("------"+T3.show())
                for T4 in self.T4Dict[(T2,T3)]:
                    print("---------"+T4.show())
                    for T5 in self.T5Dict[(T2,T3,T4)]:
                        print("------------"+T5.show())
        
       
