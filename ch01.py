# 归纳式说明
# Definition 1.1.1 A natural number n is in S if and only if
# 1. n = 0, or
# 2. n - 3 in S
def is_in_S111(n: int) -> bool:
    if n == 0:
        return True
    elif n < 0:
        return False
    else:
        return is_in_S111(n-3)


# number-elements : Listof(SchemeVal) -> Listof(List(Int SchemeVal))
# usage: (number-elements lst) -> '((0 v0) (1 v1) ...)
# Auxiliary Procedure
# number-elements-from : Listof(SchemeVal) Int -> Listof(List(Int SchemeVal))
# usage: (number-elements-from '(v0 v1 v2 ...) n) = ((n v0) (n+1 v1) (n+2 v2) ...)
# Listof(SchemeVal) ::= () | (SchemeVal . Listof(SchemeVal))
# lst - the list we are working on. It get smaller at every recursive call.
# n - an abstract context in which we are working - be called context argument or inherited attribute


def number_elements(lst):
    return map(lambda i, v: [i, v], range(len(lst)), lst)


def number_elements(lst):
    result = []
    n = 0
    for v in lst:
        result.append((n, v))
        n = n + 1
    return result


def number_elements_from(lst, n):
    if len(lst) == 0:
        return []
    else:
        return [(n, lst[0]), number_elements_from(lst[1:], n+1)]


def number_elements(lst):
    return number_elements_from(lst, 0)


print(list(number_elements([1, 2, 3])))


def product(sos1, sos2):
    return [(i, j) for i in sos1 for j in sos2]


def filter_in(pred, lst):
    return [i for i in lst if pred(i)]


def list_index(pred, lst):
    for i, v in enumerate(lst):
        if pred(v):
            return i
