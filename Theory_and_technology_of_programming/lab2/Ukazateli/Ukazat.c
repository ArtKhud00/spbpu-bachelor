#define _CRT_SECURE_NO_WARNINGS 
#include <stdio.h>
#include <string.h>
void inputk(int*, int, int);
void inputf(FILE*, int*, int, int);
void outputk(int*, int, int);
int* raznost(int*, int*, int*, int, int);
void outputf(FILE*, int*, int, int);
int main()
{
	int a[256][256], b[256][256], c[256][256];
	int *aa, *ab, *ac;
	int row, col, l;
	char fname1[50], fname2[50], fnamer[50];
	FILE *fe;
	FILE *fp;
	FILE *fk;
	aa = &a[0][0];
	ab = &b[0][0];
	ac = &c[0][0];
	printf("viberite sposob vvoda matric 1-konsol, 2-fail\n");
	scanf("%d", &l);

	printf("Vvedite kolichestvo strok matrici:\n");
	scanf_s("%d", &row);
	printf("Vvedite kolichestvo stolbcov matrici:\n");
	scanf("%d", &col);
	if (l == 1)
	{
		printf("vvedite 1 matricu\n");
		inputk(aa, row, col);
		printf("vvedite 2 matricu\n");
		inputk(ab, row, col);
		getchar();
	}
	else
	{
		getchar(); getchar();
		printf("Vvedite imya 1 faila v kotorom nahoditsa 1 matrica\n");
		fgets(fname1, 50, stdin);
		fname1[strlen(fname1) - 1] = '\0';
		fp = fopen(fname1, "r");
		inputf(fp, aa, row, col);
		printf("Vvedite imya 2 faila v kotorom nahoditsa 2 matrica\n");
		fgets(fname2, 50, stdin);
		fname2[strlen(fname2) - 1] = '\0';
		fk = fopen(fname2, "r");
		inputf(fk, ab, row, col);
	}
	printf("1 matrica\n");
	outputk(aa, row, col);
	printf("2 matrica\n");
	outputk(ab, row, col);
	raznost(aa, ab, ac, row, col);
	printf("Viberite sposob vivoda rezultata 1-konsol, 2-fail\n");
	scanf("%d", &l);
	getchar();
	if (l == 1)
	{
		printf("rezultat\n");
		outputk(ac, row, col);
	}
	else
	{
		printf("Vvedite imya faila\n");
		fgets(fnamer, 50, stdin);
		fnamer[strlen(fnamer) - 1] = '\0';

		fe = fopen(fnamer, "w");
		outputf(fe, ac, row, col);
		fclose(fe);
	}
	getchar();
}

void inputk(int *re, int n, int m)
{
	int i, j;
	for (i = 0; i < n; i++)
	{
		for (j = 0; j < m; j++)
		{
			scanf_s("%d", &*(re + i * m + j));
		}
	}
}
void inputf(FILE *fr, int *d, int n, int m)
{
	char f;
	int i, j;
	for (i = 0; i < n; i++)
	{
		for (j = 0; j < m; j++)
		{
			fscanf(fr, "%d", &d[i*m + j]);
		}
		do
		{
			f = fgetc(fr);
		} while (f != '\n');
	}
}
void outputk(int *d, int n, int m)
{
	int i, j;
	for (i = 0; i < n; i++)
	{
		for (j = 0; j < m; j++)
		{
			printf("%5d", d[i*m + j]);
		}printf("\n");
	}

}
int* raznost(int *a, int *b, int *c, int n, int m)
{
	int i, j;
	for (i = 0; i < n; i++)
	{
		for (j = 0; j < m; j++)
		{
			*(c + i * m + j) = *(b + i * m + j) - *(a + i * m + j);
		}
	}
	return(c);
}

void outputf(FILE *fe, int *d, int n, int m)
{
	int i, j;
	for (i = 0; i < n; i++)
	{
		for (j = 0; j < m; j++)
		{
			fprintf(fe, "%5d", d[i*m + j]);
		}
		fprintf(fe, "\n");
	}
}