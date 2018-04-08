def lshift(arr, k):
    return arr[k:] + arr[:k]

if __name__ == '__main__':
    n, k = map(int, input().split())
    arr  = list(map(int, input().split()))

    print(*lshift(arr, k))

