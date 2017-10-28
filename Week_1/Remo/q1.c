#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void lshift(int arr[], int n, int k)
{
	int tmp;
	int *buffer;

	buffer = (int*) malloc(k * sizeof(int));
	memcpy(buffer, arr, k * sizeof(int));
	memmove(arr, &(arr[k]), (n-k) * sizeof(int));
	memcpy(&(arr[k-1]), buffer, k * sizeof(int));

// ohne buffer:
/*
	while (k--) {
		tmp = arr[0];
		memmove(&(arr[0]), &(arr[1]), (n-1) * sizeof(int));
		arr[n-1] = tmp;
	}
*/
}

void main()
{
	int n, k, i;
	int *arr;
	scanf("%i %i", &n, &k);

	arr = (int*) malloc(n * sizeof(int));
	if (arr == NULL) exit (1);

	for (i=0; i<n; i++)
		scanf("%i", &(arr[i]));

	lshift(arr, n, k);

	for (i=0; i<n; i++)
		printf("%i ", arr[i]);
	printf("\n");

}
