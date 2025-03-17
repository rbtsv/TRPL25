class PassNode:
    def compute(self):
        return self.x.compute()

class PNode:
    def compute(self):   
        if self.op == "+":
            return self.x.compute() + self.y.compute()
        else: 
            return self.x.compute() - self.y.compute()

class MNode:
    def compute(self):
          return self.x.compute() * self.y.compute()
        
        
class NumNode:
    def compute(self):
        return int(self.text)
        