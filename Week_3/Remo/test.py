FNAME = "../strings.jsonTest"

import q3
from nose.tools import timed, eq_
import time

tests = []

def timeit(f, n, *args, **kwargs):
    t = time.time()
    while n:
        n -= 1
        f(*args, **kwargs)
    return time.time()-t

def setup():
    import json
    global tests
    with open("../strings.jsonTest") as fp:
        tests = json.load(fp)

@timed(3)
def test_verylongstring():
    global tests
    s = "".join(t['Input'][0] for t in tests)
    t = timeit(q3.valid, 10, s)
    print("avg runtime with string of length {}: {:.1e}s".format(len(s), t))

def test_expectedOutput():
    global tests

    def check(i, exp, act):
        eq_(exp, act)

    for i, T in enumerate(tests):
        r = q3.main(T['Input'][0])
        yield check, i, r, T['ExpectedOutput'][0]


