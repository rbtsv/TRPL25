#!/usr/bin/env python3
import bin_digit
import bin_digits_nodes

bin_str = "1101"
tree = bin_digit.parse(bin_str, types=bin_digits_nodes)
tree.compute()
print(bin_str, tree.val, tree.ord)
