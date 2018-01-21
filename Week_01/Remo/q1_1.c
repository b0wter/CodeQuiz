#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void lshift(int arr[], int n)
{
	int i, tmp = arr[0];

	for (i=0; i < (n-1); i++) 
		arr[i] = arr[i+1];
	arr[n-1] = tmp;	
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

	while (k--)
		lshift(arr, n);

	for (i=0; i<n; i++)
		printf("%i ", arr[i]);
	printf("\n");

}
