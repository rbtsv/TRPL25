#!/usr/bin/env python3
import arithm
import arithm_nodes

tree = arithm.parse("2-2-1".replace(" ", ""), types=arithm_nodes)
print(tree.compute())
