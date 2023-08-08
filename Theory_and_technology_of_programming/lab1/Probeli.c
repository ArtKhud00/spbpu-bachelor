#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
void inputk(char*);
void input1(FILE*, char*);
char* alltrim(char*);
void outputk(char*);
void outputf(FILE*, char*);
int main()
{
	char a1[256];
	int i, l, m;
	char *fname = (char *)malloc(256);
	char *fname1 = (char *)malloc(256);
	printf("Sposob vvoda 1-konsol, 2-file \n");
	scanf_s(" %d", &l);
	getchar();
	if (l == 1)
	{
		printf("Vvedite stroku\n");
		inputk(a1);
	}
	else {
		printf("Vvedite imya faila\n");
		fgets(fname, 256, stdin);
		fname[strlen(fname) - 1] = '\0';
		FILE *fe;
		fe = fopen(fname, "r");
		if (fe == NULL)
		{
			printf("Ne udalos otkrit file\n");
			getchar();
			return 1;
		}
		else
		{
			input1(fe, a1);
			i = 0;
			printf("\nStroka iz faila\n");
			do
			{
				printf("%c", a1[i]);
				i++;
			} while (a1[i] != '\n'&& a1[i] != EOF);
		}
	}
	alltrim(a1);
	printf("\nSposob vivoda 1-konsol, 2-file\n");
	scanf_s("%d", &m);
	getchar();
	if (m == 1)
	{
		printf("\nnovaya stroka\n");
		i = 0;
		outputk(a1);
		getchar();
	}
	else
	{
		printf("Vvedite imya faila\n");
		fgets(fname1, 255, stdin);
		fname1[strlen(fname1) - 1] = '\0';
		FILE *fp;
		fp = fopen(fname1, "w");
		if (fp == NULL)
		{
			printf("Ne udalos otkrit file\n");
			getchar();
			return 1;
		}
		else
		{
			outputf(fp, a1);
			fclose(fp);
		}
	}
	getchar();
}
void inputk(char* p1)
{
	char s1;
	int i;
	i = 0;
	do
	{
		s1 = getchar();
		if (s1 != '\n')
			*(p1 + i) = s1;

		i++;
	} while (s1 != '\n');
	*(p1 + i) = 0;

}
void input1(FILE *fe, char* p1)
{
	char s3;
	int i;
	i = 0;
	do
	{
		s3 = fgetc(fe);
		*(p1 + i) = s3;
		i++;

	} while (s3 != EOF);
	*(p1 + i) = 0;
}

char* alltrim(char* str1) {
	int b;
	int e;
	int i;
	int lstr1;
	for (b = 0; b < lstr1, str1[b] == ' '; b++);
	i = 0;
	do
	{
		str1[i] = str1[i + b];
		i++;

	} while (str1[i + b] != '\n' && (i + b) < 256);
	lstr1 = strlen(str1);
	for (e = lstr1 - 2; e > 0, str1[e] == ' '; e--);
	str1[e + 1] = 0;
	return str1;
}
void outputk(char* p3)
{
	int i;
	i = 0;
	do
	{
		printf("%c", *(p3 + i));
		i++;
	} while (*(p3 + i) != 0);
}
void outputf(FILE *fp, char* a3)
{
	int i = 0;
	do {
		fprintf(fp, "%c", *(a3 + i));
		i++;
	} while (*(a3 + i) != '\0');
}