def valid(s):
    n = [s.count(x) for x in set(s)]
    counts = list(set(n))
    numcounts = [n.count(x) for x in counts]

    even = len(counts) == 1
    singleDev = len(counts) == 2 and 1 in numcounts    
    removable = singleDev and (counts[numcounts.index(1)] == 1 or abs(counts[0]-counts[1]) == 1)

    return even or removable
    
def main(s):
    result = valid(s)
    return result and "JA" or "NEIN"

if __name__ == "__main__":
    print(main(input()))

