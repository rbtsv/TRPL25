class PlusNode:
    def compute(self): 
        s = self.x.compute()
        for z in self.sum.elements: #(op:[+,-] x:term)* <SumNode>
            if z.op.text == "+":
                s += z.x.compute()
            else:
                s -= z.x.compute()
        return s

class MultNode:
    def compute(self):
        p = self.fact.compute()
        for z in self.prod.elements:
            if z.op.text == "*":
                p *= z.x.compute()
            else:
                p /= z.x.compute()
        return p        

class SumNode:
    pass

class ProdNode:
    pass
    
class PassNode:
    def compute(self):
        return self.x.compute()    
        
class NumNode:
    def compute(self):
        return int(self.text)
        