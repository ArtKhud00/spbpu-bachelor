# Лабораторная работа №1. Двумерные массивы
Данная программа реализует ввод и вывод двумерных массивов (матриц) двумя способами:
  - с консоли;
  - из файла.

Находит разность двух матриц. Реализация с помощью индексов и с помощью указателей.

## Пример работы
После запуска программы появляются варианты ввода строки
```
Viberite sposob vvoda matric 1-konsol, 2 - file
```
Выберем ввод с консоли и введем размерности матриц (обе матрицы имеют одинаковый размер)
```
Sposob vvoda 1-konsol, 2 - file
1
Vvedite kolichestvo strok matrici:
5
Vvedite kolichestvo stolbcov matrici:
4   
```
Далее введем сами матрицы и выберем вывод результата в файл
```
Sposob vvoda 1-konsol, 2 - file
1
Vvedite kolichestvo strok matrici:
5
Vvedite kolichestvo stolbcov matrici:
4
vvedite 1 matricu
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
vvedite 2 matricu
68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87
1 matrica
     1   2   3   4
     5   6   7   8
     9  10  11  12
	13	14  15  16
	17  18  19  20
2 matrica
    68  69  70  71
	72  73  74  75
	76  77  78  79
	80  81  82  83
	84  85  86  87
viberite sposob vivoda rezultata 1-konsol, 2-file
2
result.txt
```
Содержимое файла `result.txt`
```
    67  67  67  67
	67  67  67  67
	67  67  67  67
	67  67  67  67
	67  67  67  67
```
