# coding: udf-8

class Tensor:
    """Tensor class"""

    def __init__(self, nA, nI, nC, nK, nB, nJ, typ):
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


class Diagram:
    """Diagram class"""

    def __init__(self, T1, T2, T3, T4, T5, weight):
        self.T1 = T1
        self.T2 = T2
        self.T3 = T3
        self.T4 = T4
        self.T5 = T5
        self.weight = weight      # parity (sign) is involved

    def compact(self):
        """000テンソルを削除し、左詰めにする"""
        if (self.T4.none) self.T4 = self.T5
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


class DiagramGroup:
    """Factorized diagram group"""

    def __init__(self, DiagramList):
        self.DiafgramList = DiagramList
        self.nDiagram = len(DiagramList)

    def __setupIntermediates__(self):
        # ダイアグラム中のゼロテンソルを削除し、左詰め
        for i in range(1, self.nDiagram):
            self.DiagramList(i) = self.DiagramList(i).compact

        # 
        
