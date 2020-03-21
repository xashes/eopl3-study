# Exercise 1.24
def everyp(pred, lst):
    return all(map(pred, lst))


def test_everyp():
    assert everyp(lambda x: isinstance(x, int), ['a', 1, 2, 3]) is False
    assert everyp(lambda x: x > 6, [7, 8, 9]) is True


def existp(pred, lst):
    return any(map(pred, lst))


def test_existp():
    assert existp(lambda x: isinstance(x, int), ['a', 1, 'b']) is True
    assert existp(lambda x: isinstance(x, int), ['a', 'c', 'b']) is False
