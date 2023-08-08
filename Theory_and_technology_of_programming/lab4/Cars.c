#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <locale.h>

typedef struct persone {
	char firstname[20];
	char lastname[20];
	char car[20];
	char plate[9];
	int year;
} MM;

int inputf(MM array[100], FILE*, FILE*);
int poisk(MM array[100], char array1[9], int, int, int array2[100]);
void sortirovka(MM array[100], int array2[100], int);
void output(MM array[100], int);

int main()
{
	FILE *fp;
	FILE *fe;
	char symb[9];
	int t[100];
	int l, j;
	int k;
	MM infotab[100];
	setlocale(LC_ALL, "Rus");
	fe = fopen("red1.txt", "r");
	fp = fopen("red.txt", "r");
	k = inputf(infotab, fp, fe);
	fclose(fp);
	fclose(fe);
	printf("Введите: \n");
	gets(symb);
	l = strlen(symb);
	j = poisk(infotab, symb, l, k, t);
	if (j > 0)
	{
		sortirovka(infotab, t, j);
		output(infotab, t, j);
	}
	else
	{
		printf("Нет");
	}
	getchar(); getchar();
}

int inputf(struct persone m[], FILE *fp, FILE *fe)
{
	int i = 0;
	while (!feof(fp))
	{
		fscanf(fp, "%s %s %s %s %d ", m[i].firstname, m[i].lastname, m[i].car, m[i].plate, &m[i].year);
		printf("%s %s %s %s %d ", m[i].firstname, m[i].lastname, m[i].car, m[i].plate, m[i].year);
		printf("\n");
		i++;
	}
	while (!feof(fe))
	{
		fscanf(fe, "%s %s %s %s %d ", m[i].firstname, m[i].lastname, m[i].car, m[i].plate, &m[i].year);
		printf("%s %s %s %s %d ", m[i].firstname, m[i].lastname, m[i].car, m[i].plate, m[i].year);
		printf("\n");
		i++;
	}
	return(i);
}

int poisk(struct persone n[], char symb[], int u, int o, int t[])
{
	int i, k, l, j, m;
	int fl;
	m = 9 - u;
	k = 0;
	for (i = 0; i < o; i++)
	{
		l = 0;
		fl = 1;
		while (l <= m && fl == 1)
		{
			fl = 0;
			j = 0;
			while (j < u && fl == 0)
			{
				if (n[i].plate[l + j] == symb[j])
				{
					j++;
				}
				else
				{
					fl = 1;
				}
			}
			if (j == u)
			{
				t[k] = i;
				k++;
			}
			if (fl == 1)
			{
				l++;
			}
		}
	}
	return(k);
}

void sortirovka(struct persone n[], int t[], int y)
{
	int i, z, q, f, d;
	int min, j, l, fl;
	char m, k;
	for (i = 0; i < y; i++)
	{
		z = t[i];
		f = i;
		min = z;
		for (j = i; j < y; j++)
		{
			q = t[j];
			l = 0;
			k = n[q].car[l];
			m = n[min].car[l];
			fl = 0;
			while (k != '\0'&& m != '\0'&& fl == 0)
			{
				if (k == m)
				{
					l++;
					k = n[q].car[l];
					m = n[min].car[l];
				}
				if (k < m)
				{
					min = q;
					f = j;
					fl = 1;
				}
				else
				{
					fl = 1;
				}
			}
		}
		d = t[i];
		t[i] = t[f];
		t[f] = d;
	}
}

void output(struct persone n[], int t[], int y)
{
	int i, z;
	for (i = 0; i < y; i++)
	{
		z = t[i];
		printf("\n %8s\t %s\t %d\t %9s\t %s\t", n[z].car, n[z].plate,
			n[z].year, n[z].firstname, n[z].lastname);
	}
}
