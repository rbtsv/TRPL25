import arithm
import arithm_nodes


expr = "((1+(10+3)) *5 )".replace(" ", "")
# expr = "".join(expr.split())
print(expr)
tree = arithm.parse(expr, types=arithm_nodes) # ((10+3)*5)
print tree.compute()
