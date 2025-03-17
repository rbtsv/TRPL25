class PMNode(object):
    def compute(self):
        return self.first.compute() + self.second.compute()

class MDNode(object):
    def compute(self):
        return self.first.compute() * self.second.compute()
        
        
class NumNode(object):
    def compute(self):
        return int(self.text)
        