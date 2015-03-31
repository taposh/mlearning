"""
Author: Sari Haj Hussein
"""
from pyds import MassFunction
from pyuds import pyuds

m = MassFunction({
    "a": 0.26,
    "b": 0.26,
    "c": 0.26,
    "ab": 0.07,
    "ac": 0.01,
    "ad": 0.01,
    "bc": 0.01,
    "bd": 0.01,
    "cd": 0.01,
    "abcd": 0.1
})

print(pyuds.GH(m))
print(pyuds.GS(m))
print(pyuds.AU(m, False))

m = MassFunction([
    ({'a'}, 0.26),
    ({'b'}, 0.26),
    ({'c'}, 0.26),
    ({'a', 'b'}, 0.07),
    ({'a', 'c'}, 0.01),
    ({'a', 'd'}, 0.01),
    ({'b', 'c'}, 0.01),
    ({'b', 'd'}, 0.01),
    ({'c', 'd'}, 0.01),
    ({'a', 'b', 'c', 'd'}, 0.1)
])

print(pyuds.GH(m))
print(pyuds.GS(m))
print(pyuds.AU(m, False))