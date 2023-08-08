#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
int* func3(int*, int, int);
void zapol(int*, int);
void output(int*, int);
void main()
{
	int b[10][10], c[4][4];
	int *ab, *ac;
	int M = 10, N = 4;
	ab = &b[0][0];
	ac = &c[0][0];
	zapol(ab, M);
	printf("ishodnaya matrica\n");
	output(ab, M);
	func3(ab, M, M);
	printf("\n");
	printf("\n");
	printf("preobrazaovannaya matrica 10*10\n");
	output(ab, M);
	zapol(ab, M);
	printf("\n");
	printf("\n");
	printf("ishodnaya matrica\n");
	output(ab, M);
	func3(ab, M, N);
	printf("\n");
	printf("\n");
	printf("matrica 10*10 s preobrazovannim verhnim levim uglom\n");
	output(ab, M);
	printf("\n");
	printf("\n");
	zapol(ac, N);
	printf("matrica 4*4\n");
	output(ac, N);
	func3(ac, N, N);
	printf("\n");
	printf("\n");
	printf("preobrazaovannaya matrica 4*4\n");
	output(ac, N);
	getchar();
}
int* func3(int *d, int M, int N)
{
	int i, j, k, l, sum;
	int kk;
	sum = 0;
	for (i = 0; i < N; i++)
	{

		for (j = 0; j < N; j++)
		{
			k = i;
			l = j;
			while (l < N)
			{
				kk = k * M + l;
				sum = sum + *(d + kk);
				l++;
			}
			k = i + 1;
			l = j - 1;
			while (k < N && l >= 0)
			{
				kk = k * M + l;
				sum = sum + *(d + kk);
				k++;
				l--;
			}
			k = i + 1;
			l = j + 1;
			while (k < N && l < N)
			{
				kk = k * M + l;
				sum = sum + *(d + kk);
				l++;
			}
			*(d + i * M + j) = sum;
			sum = 0;
		}
	}
	return(d);
}
void output(int *d, int M)
{
	int i, j, kk;
	for (i = 0; i < M; i++)
	{
		for (j = 0; j < M; j++)
		{
			kk = i * M + j;
			printf("%6d", *(d + kk));
		}printf("\n");
	}
}
void zapol(int *a, int M)
{
	int i, j, kk;
	for (i = 0; i < M; i++)
	{
		for (j = 0; j < M; j++)
		{
			kk = i * M + j;
			*(a + kk) = i + j;
		}
	}

}