#!/usr/local/bin/python
# -*- coding: utf-8 -*-

import math
from datetime import datetime
from datetime import timedelta

start_time = datetime.now()
def millis():
	dt = datetime.now() - start_time
	ms = (dt.days * 24 * 60 * 60 + dt.seconds) * 1000 + dt.microseconds / 1000.0
	return ms


# ------------------------------

limit = 2000000

numbers = range(2, limit+1)
z = 2
limit_root = math.sqrt(limit)
while z < limit_root:
	# alle Vielfache entfernen
	v = z*2
	while v <= limit:
		numbers[v - 2] = 0
		v += z
	z += 1

primes = list(filter(lambda x: x!=0, numbers))
primes_len = len(primes)
primes_sum = sum(primes)

# ------------------------------


duration = millis()
print("Anzahl: %i" % primes_len)
print("Summe: %i " % primes_sum)
print("Zeit: %ims" % duration)
