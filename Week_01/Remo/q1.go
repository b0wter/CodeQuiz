package main

import "fmt"

func lshift(arr []int, n int, k int) []int {
	return append(arr[k:], arr[:k]...)
}

func main() {
	var n, k = 0, 0

	fmt.Scanf("%d %d", &n, &k)

	arr := make([]int, n)
	for i := 0; i < n; i++ {
		fmt.Scanf("%d", &(arr[i]))
	}

	arr = lshift(arr, n, k)

	for i := 0; i < n; i++ {
		fmt.Printf("%d ", arr[i])
	}
	fmt.Printf("\n")
}
