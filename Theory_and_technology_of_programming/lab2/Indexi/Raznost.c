#define _CRT_SECURE_NO_WARNINGS 
#include <stdio.h>
#include <string.h>
void inputk(int arr[][256], int, int);
void inputf(FILE*, int arr[][256], int, int);
void outputk(int arr[][256], int, int);
int raznost(int arr1[][256], int arr2[][256], int arr3[][256], int, int);
void outputf(FILE*, int arr[][256], int, int);
int main()
{
	int a[256][256], b[256][256], c[256][256];
	int row, col, l;
	char fname1[50], fname2[50], fnamer[50];
	FILE *fe;
	FILE *fp;
	FILE *fk;
	printf("viberite sposob vvoda matric 1-konsol, 2-fail\n");
	scanf("%d", &l);

	printf("Vvedite kolichestvo strok matrici:\n");
	scanf("%d", &row);
	printf("Vvedite kolichestvo stolbcov matrici:\n");
	scanf("%d", &col);
	if (l == 1)
	{
		printf("vvedite 1 matricu\n");
		inputk(a, row, col);
		printf("vvedite 2 matricu\n");
		inputk(b, row, col);
		getchar();
	}
	else
	{
		getchar(); getchar();
		printf("Vvedite imya 1 faila v kotorom nahoditsa 1 matrica\n");
		fgets(fname1, 50, stdin);
		fname1[strlen(fname1) - 1] = '\0';
		fp = fopen(fname1, "r");
		inputf(fp, a, row, col);
		printf("Vvedite imya 2 faila v kotorom nahoditsa 2 matrica\n");
		fgets(fname2, 50, stdin);
		fname2[strlen(fname2) - 1] = '\0';
		fk = fopen(fname2, "r");
		inputf(fk, b, row, col);
	}
	printf("1 matrica\n");
	outputk(a, row, col);
	printf("2 matrica\n");
	outputk(b, row, col);
	raznost(a, b, c, row, col);
	printf("Viberite sposob vivoda rezultata 1-konsol, 2-fail\n");
	scanf("%d", &l);
	getchar();
	if (l == 1)
	{
		printf("rezultat\n");
		outputk(c, row, col);
	}
	else
	{
		printf("Vvedite imya faila\n");
		fgets(fnamer, 50, stdin);
		fnamer[strlen(fnamer) - 1] = '\0';

		fe = fopen(fnamer, "w");
		outputf(fe, c, row, col);
		fclose(fe);
	}
	getchar();
}

void inputk(int re[][256], int n, int m)
{
	int i, j;
	for (i = 0; i < n; i++)
	{
		for (j = 0; j < m; j++)
		{
			scanf("%d", &re[i][j]);
		}
	}
}

void inputf(FILE *fr, int d[][256], int n, int m)
{
	char f;
	int i, j;
	for (i = 0; i < n; i++)
	{
		for (j = 0; j < m; j++)
		{
			fscanf(fr, "%d", &d[i][j]);
		}
		do
		{
			f = fgetc(fr);
		} while (f != '\n');
	}
}

void outputk(int d[][256], int n, int m)
{
	int i, j;
	for (i = 0; i < n; i++)
	{
		for (j = 0; j < m; j++)
		{
			printf("%5d", d[i][j]);
		}printf("\n");
	}

}
int raznost(int a[][256], int b[][256], int c[][256], int n, int m)
{
	int i, j;
	for (i = 0; i < n; i++)
	{
		for (j = 0; j < m; j++)
		{
			c[i][j] = b[i][j] - a[i][j];
		}
	}
	return(c);
}

void outputf(FILE *fe, int d[][256], int n, int m)
{
	int i, j;
	for (i = 0; i < n; i++)
	{
		for (j = 0; j < m; j++)
		{
			fprintf(fe, "%5d", d[i][j]);
		}
		fprintf(fe, "\n");
	}
}