# Exercise 1.24
def everyp(pred, lst):
    return all(map(pred, lst))


def test_everyp():
    assert everyp(lambda x: isinstance(x, int), ["a", 1, 2, 3]) is False
    assert everyp(lambda x: x > 6, [7, 8, 9]) is True


def existp(pred, lst):
    return any(map(pred, lst))


def test_existp():
    assert existp(lambda x: isinstance(x, int), ["a", 1, "b"]) is True
    assert existp(lambda x: isinstance(x, int), ["a", "c", "b"]) is False


# Exercise 1.28
def merge(loi1, loi2):
    if len(loi1) == 0:
        return loi2
    elif len(loi2) == 0:
        return loi1
    elif loi1[0] < loi2[0]:
        return [loi1[0]] + (merge(loi1[1:], loi2))
    else:
        return [loi2[0]] + (merge(loi1, loi2[1:]))


def test_merge():
    assert merge([1, 4], [1, 2, 8]) == [1, 1, 2, 4, 8]
    assert merge([35, 62, 81, 90, 91], [3, 83, 85, 90]) == [
        3,
        35,
        62,
        81,
        83,
        85,
        90,
        90,
        91,
    ]


# Exercise 1.29
def qsort(lon):
    if len(lon) == 0:
        return []
    else:
        base = lon[0]
        return (
            qsort([n for n in lon[1:] if n <= base])
            + [base]
            + qsort([n for n in lon[1:] if n > base])
        )


def test_qsort():
    lon = [8, 2, 5, 2, 3]
    assert qsort(lon) == sorted(lon)
