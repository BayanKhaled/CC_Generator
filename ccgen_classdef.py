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
        self.name = ""

        
    def setup(self, nA, nI, nC, nK, nB, nJ, typ):
        self.nA = nA
        self.nI = nI
        self.nC = nC
        self.nK = nK
        self.nB = nB
        self.nJ = nJ
        self.typ = typ
        self.name = typ+str(nA)+","+str(nI)+","+str(nC)+","+str(nK)

        
    def zero(self):
        """6つのindexが全てゼロならzero（何もなし）"""
        if (self.nA == 0 and self.nI == 0 and self.nC == 0 and self.nK == 0 and self.nB == 0 and self.nJ == 0):
            return True
        else:
            return False

        
    def __eq__(self, T):
        """Tensorの==演算子"""

        try:
            if (self.nA == T.nA and self.nI == T.nI and self.nC == T.nC and self.nK == T.nK and self.nB == T.nB and self.nJ == T.nJ and self.typ == T.typ):
                return True
            else:
                return False
        except AttributeError:  # Noneと比較された場合を想定
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



class Intermediate:
    """Intermediate class"""

    def __init__(self, children, tensor, level):
        self.children = children   # list of child Intermediate instances
        self.mytensor = tensor     # my Tensor intance
        self.level = level         # level of the intermediate
        self.name = "V"+str(level)
        self.fingerprint = None
        self.contents = []         # 
        self.weight = 1
        self.nA = 0
        self.nI = 0
        self.nC = 0
        self.nK = 0

        # define intermedieate type
        if children == None:
            self.contents.append(tensor.typ)
        else:
            for child in children:
                for c in child.contents:
                    self.contents.append(c)
            if not tensor == None:
                self.contents.append(tensor.typ)
        # ソートして文字列にする
        self.contents.sort()
        self.typ = "".join(self.contents)

#        # 重み因子（パリティ含む)

        
        # intermediate indices (とりあえずエネルギー計算のみ)
        if children == None:
            self.nA = tensor.nA
            self.nI = tensor.nI
            self.nC = tensor.nC
            self.nK = tensor.nK
        elif tensor == None:
            self.nA = self.children[0].nA
            self.nI = self.children[0].nI
            self.nC = self.children[0].nC
            self.nK = self.children[0].nK
        else:
            self.nA = self.children[0].nA + tensor.nA
            self.nI = self.children[0].nI + tensor.nI
            self.nC = self.children[0].nC - tensor.nC
            self.nK = self.children[0].nK - tensor.nK

        print(self.display())
            

    def to_string(self):
        """出力用に文字列に変換"""

        return self.typ+str(self.nA)+","+str(self.nI)+","+str(self.nC)+","+str(self.nK)


    def display(self):
        """確認用の出力関数"""

        string = ""
        if self.children != None:
            for child in self.children:
                string += child.to_string() + ", "

        return "inter. "+self.to_string()+" contains ["+string+"]"
        
                        
                    
class Diagram:
    """Diagram class"""

    def __init__(self, T1, T2, T3, T4, T5, pw):
        self.T1 = T1
        self.T2 = T2
        self.T3 = T3
        self.T4 = T4
        self.T5 = T5
        weight = 0.5**(abs(pw) - 1)
        if pw < 0: weight = -weight
        self.weight = weight      # parity (sign) is involved

        
    def compact(self):
        """000テンソルを削除し、左詰めにする"""
        if (self.T4.zero) :
            self.T4 = self.T5
            self.T5 = None
        if (self.T3.zero):
            self.T3 = self.T4
            self.T4 = self.T5
            self.T5 = None
        if (self.T2.zero):
            self.T2 = self.T3
            self.T3 = self.T4
            self.T4 = self.T5
            self.T5 = None
        if (self.T1.zero):
            self.T1 = self.T2
            self.T2 = self.T3
            self.T3 = self.T4
            self.T4 = self.T5
            self.T5 = None

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
        


