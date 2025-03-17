#!/usr/bin/env python3
import arithm
import arithm_nodes

tree = arithm.parse("1+(1+(1+2*5)*2+3)*2+13".replace(" ", ""), types=arithm_nodes)
print(tree.compute())
