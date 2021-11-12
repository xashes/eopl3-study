from ch01 import *
import pytest

def test_S111():
    assert is_in_S111(0) is True
    assert is_in_S111(1) is False
    assert is_in_S111(7) is False
    assert is_in_S111(9) is True