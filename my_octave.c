// ILINCA SEBASTIAN - IONUT : 311CA
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void liberty(int ****mat, int **l, int **c, int count)
{
	if (count > 0) {
		for (int k = 0; k < count; ++k) {
			for (int i = 0; i < (*l)[k]; ++i)
				free((*mat)[k][i]);
		}
		for (int k = 0; k < count; ++k)
			free((*mat)[k]);
		free(*mat);
		(*mat) = NULL;
		free((*l));
		(*l) = NULL;
		free((*c));
		(*c) = NULL;
	}
}

int mod(int number)
{	if (number < 0)
		return number % 10007 + 10007;
	else
		return number % 10007;
}

int suma(int **mat, int a, int b)
{
	int sum = 0;
	for (int i = 0; i < a; ++i) {
		for (int j = 0; j < b; ++j) {
			sum = mod(sum);
			sum += mod(mat[i][j]);
			sum = mod(sum);
		}
	}
	return mod(sum);
}

void swap(int ****mat, int **lines, int **col, int i, int j)
{
	int **sm;
	//sm vine de la swap matrix
	int auxl = (*lines)[i];
	int auxc = (*col)[i];
	sm = (int **)malloc(auxl * sizeof(int *));
	if (!sm) {
		fprintf(stderr, "Malloc failed\n");
		return;
	}
	for (int k = 0; k < auxl; ++k) {
		sm[k] = (int *)malloc(auxc * sizeof(int));
		if (!sm[k]) {
			fprintf(stderr, "Malloc failed\n");
			return;
		}
	}
	for (int k = 0; k < auxl; ++k) {
		for (int q = 0; q < auxc; ++q)
			sm[k][q] = (*mat)[i][k][q];
	}
	(*lines)[i] = (*lines)[j];
	(*col)[i] = (*col)[j];
	for (int k = 0; k < auxl; ++k)
		free((*mat)[i][k]);
	free((*mat)[i]);
	(*mat)[i] = (int **)malloc((*lines)[i] * sizeof(int *));
	if ((*mat)[i] == NULL) {
		fprintf(stderr, "Malloc failed\n");
		return;
	}
	for (int k = 0; k < (*lines)[i]; ++k) {
		(*mat)[i][k] = (int *)malloc((*col)[i] * sizeof(int));
		if ((*mat)[i][k] == NULL) {
			fprintf(stderr, "Malloc failed\n");
			return;
		}
	}
	for (int k = 0; k < (*lines)[i]; ++k) {
		for (int q = 0; q < (*col)[i]; ++q)
			(*mat)[i][k][q] = (*mat)[j][k][q];
	}
	for (int k = 0; k < (*lines)[j]; ++k)
		free((*mat)[j][k]);
	free((*mat)[j]);
	(*lines)[j] = auxl;
	(*col)[j] = auxc;
	(*mat)[j] = (int **)malloc(auxl * sizeof(int *));
	if ((*mat)[j] == NULL) {
		fprintf(stderr, "Malloc failed\n");
		return;
	}
	for (int k = 0; k < auxl; ++k) {
		(*mat)[j][k] = (int *)malloc(auxc * sizeof(int));
		if ((*mat)[j][k] == NULL) {
			fprintf(stderr, "Malloc failed\n");
			return;
		}
	}
	for (int k = 0; k < auxl; ++k) {
		for (int q = 0; q < auxc; ++q)
			(*mat)[j][k][q] = sm[k][q];
	}
	for (int k = 0; k < auxl; ++k)
		free(sm[k]);
	free(sm);
	sm = NULL;
}

void transpose(int ****mat, int **l, int **col, int id)
{
	int max;
	if ((*l)[id] > (*col)[id]) {
		for (int k = 0; k < (*l)[id]; ++k) {
			(*mat)[id][k] = realloc((*mat)[id][k], (*l)[id] * sizeof(int));
			if ((*mat)[id][k] == NULL) {
				fprintf(stderr, "Realloc failed\n");
				return;
			}
		}
		max = (*l)[id];
	}
	if ((*col)[id] > (*l)[id]) {
		(*mat)[id] = (int **)realloc((*mat)[id], (*col)[id] * sizeof(int *));
		if ((*mat)[id] == NULL) {
			fprintf(stderr, "Realloc failed\n");
			return;
		}
		for (int k = (*l)[id]; k < (*col)[id]; ++k) {
			(*mat)[id][k] = (int *)malloc((*col)[id] * sizeof(int));
			if ((*mat)[id][k] == NULL) {
				fprintf(stderr, "Malloc failed\n");
				return;
			}
		}
		max = (*col)[id];
	}
	if ((*l)[id] == (*col)[id])
		max = (*col)[id];
	int aux, i = 0;
	while (i < max) {
		for (int k = i; k < max; ++k) {
			aux = (*mat)[id][k][i];
			(*mat)[id][k][i] = (*mat)[id][i][k];
			(*mat)[id][i][k] = aux;
		}
		i++;
	}
	if ((*l)[id] > (*col)[id]) {
		for (int i = (*col)[id]; i < (*l)[id]; i++)
			free((*mat)[id][i]);
	}
	aux = (*l)[id];
	(*l)[id] = (*col)[id];
	(*col)[id] = aux;
}

void recycle_bin(int ****mat, int **lines, int **col, int i, int *count)
{
	int sus = 0;
	if (i < *count - 1) {
		for (int k = i; k < *count - 1; ++k)
			swap(mat, lines, col, k, k + 1);
	}
		i = *count - 1;
		for (int k = 0; k < (*lines)[i]; ++k)
			free((*mat)[i][k]);
		free((*mat)[i]);
		(*mat)[i] = NULL;
		*count = *count - 1;
		if (*count == sus) {
			free(*mat);
			free(*lines);
			free(*col);
		}
		if (*count > sus) {
			(*mat) = (int ***)realloc(*mat, *count * sizeof(int **));
			if ((*mat) == NULL) {
				fprintf(stderr, "Realloc failed\n");
				return;
			}
			(*lines) = (int *)realloc(*lines, *count * sizeof(int));
			if ((*lines) == NULL) {
				fprintf(stderr, "Realloc failed\n");
				return;
			}
			(*col) = (int *)realloc(*col, *count * sizeof(int));
			if ((*col) == NULL) {
				fprintf(stderr, "Realloc failed\n");
				return;
			}
		}
	//merge mai frumos si mai eficient cred decat chestia
	//cu injumatatirea pt ca realoc de fiecare cate un nou spatiu
	//mi se pare mult mai eficient
}

void sort(int ****mat, int **lines, int **col, int count)
{
	int *sum;
	sum = (int *)malloc(count * sizeof(int));
	if (!sum) {
		fprintf(stderr, "Malloc failed\n");
		return;
	}
	for (int i = 0; i < count; ++i)
		sum[i] = suma((*mat)[i], (*lines)[i], (*col)[i]);
	for (int i = 0; i < count - 1; ++i) {
		for (int j = i + 1; j < count; j++) {
			if (sum[i] > sum[j]) {
				swap(mat, lines, col, i, j);
				int aux = sum[i];
				sum[i] = sum[j];
				sum[j] = aux;
			}
		}
	}
	free(sum);
	sum = NULL;
}

void alloc_dynamic(int ****mat, int **lines, int **col, int *a, int m, int n)
{
	*a = *a + 1;
	if (*a > 1) {
		//realocare_mat(&*&mat, *a);
		//realocare_lin(lines, a);
		//realocare_col(col, a);
		(*mat) = (int ***)realloc(*mat, *a * sizeof(int **));
		if ((*mat) == NULL) {
			fprintf(stderr, "Realloc failed\n");
			return;
		}
		(*lines) = (int *)realloc(*lines, *a * sizeof(int));
		if ((*lines) == NULL) {
			fprintf(stderr, "Realloc failed\n");
			return;
		}
		(*col) = (int *)realloc(*col, *a * sizeof(int));
		if ((*col) == NULL) {
			fprintf(stderr, "Realloc failed\n");
			return;
		}
	}
	if (*a == 1) {
		*mat = (int ***)malloc(1 * sizeof(int **));
	if ((*mat) == NULL) {
		fprintf(stderr, "Malloc failed\n");
		return;
	}
	*lines = (int *)malloc(1 * sizeof(int));
	if ((*lines) == NULL) {
		fprintf(stderr, "Malloc failed\n");
		return;
	}
	*col = (int *)malloc(1 * sizeof(int));
	if ((*col) == NULL) {
		fprintf(stderr, "Malloc failed\n");
		return;
	}
	}
	//*((*lines) + (*a - 1)) = *m;
	(*lines)[*a - 1] = m;
	//*((*col) + (*a - 1)) = n;
	(*col)[*a - 1] = n;
	(*mat)[*a - 1] = (int **)malloc(m * sizeof(int *));
	if ((*mat)[*a - 1] == NULL) {
		fprintf(stderr, "Malloc failed\n");
		return;
	}
	for (int i = 0; i < m; i++) {
		(*mat)[*a - 1][i] = (int *)malloc(n * sizeof(int));
		if ((*mat)[*a - 1][i] == NULL) {
			fprintf(stderr, "Malloc failed\n");
			return;
		}
	}
	for (int i = 0; i < m; i++) {
		for (int j = 0; j < n; j++) {
			int tmp;
			scanf("%d", &tmp);
			(*mat)[*a - 1][i][j] = tmp;
		}
	}
}

void print_mat_index(int ****mat, int *a, int **lines, int **col)
{
	int l = *((*lines) + *a);
	int c = *((*col) + *a);
	for (int i = 0; i < l; ++i) {
		for (int j = 0; j < c; ++j) {
			int val = *(*(*((*mat) + *a) + i) + j);
			printf("%d ", val);
		}
		printf("\n");
	}
}

void multiply(int ****mat, int i1, int i2, int **lines, int **col, int *a)
{
	if ((*lines)[i2] != (*col)[i1]) {
		printf("Cannot perform matrix multiplication\n");
		return;
	}
	if ((*col)[i1] == (*lines)[i2]) {
		(*mat) = (int ***)realloc(*mat, (*a + 1) * sizeof(int **));
		if ((*mat) == NULL) {
			fprintf(stderr, "Realloc failed\n");
			return;
		}
		(*lines) = (int *)realloc(*lines, (*a + 1) * sizeof(int));
		if ((*lines) == NULL) {
			fprintf(stderr, "Realloc failed\n");
			return;
		}
		(*col) = (int *)realloc(*col, (*a + 1) * sizeof(int));
		if ((*col) == NULL) {
			fprintf(stderr, "Realloc failed\n");
			return;
		}
		(*mat)[*a] = (int **)malloc((*lines)[i1] * sizeof(int *));
		if ((*mat)[*a] == NULL) {
			fprintf(stderr, "Malloc failed\n");
			return;
		}
		for (int i = 0; i < (*lines)[i1]; i++) {
			(*mat)[*a][i] = (int *)malloc((*col)[i2] * sizeof(int));
			if ((*mat)[*a][i] == NULL) {
				fprintf(stderr, "Malloc failed\n");
				return;
			}
		}
		for (int i = 0; i < (*lines)[i1]; i++)
			for (int j = 0; j < (*col)[i2]; j++) {
				(*mat)[*a][i][j] = 0;
				for (int k = 0; k < (*lines)[i2]; k++) {
					int temporary = 0;
					temporary = mod((*mat)[i1][i][k]);
					temporary *= mod((*mat)[i2][k][j]);
					temporary = mod(temporary);
					(*mat)[*a][i][j] +=  mod(temporary);
				}
				(*mat)[*a][i][j] = mod((*mat)[*a][i][j]);
			}
	}
		(*lines)[*a] = (*lines)[i1];
		(*col)[*a] = (*col)[i2];
	*a = *a + 1;
}

void redimension(int ****mat, int **lines, int **col, int count)
{	int k;
	int tmp;
	scanf("%d", &k);
	if (k >= count || k < 0) {
		printf("No matrix with the given index\n");
		return;
	}
	int l = (*lines)[k];
	int c = (*col)[k];
	int z1, z2;
	int *line_elements, *col_elements;
	scanf("%d", &z1);
	line_elements = (int *)malloc(z1 * sizeof(int));
	if (!line_elements) {
		fprintf(stderr, "Malloc failed\n");
		return;
	}
	for (int i = 0; i < z1; i++)
		scanf("%d", &line_elements[i]);
	scanf("%d", &z2);
	col_elements = (int *)malloc(z2 * sizeof(int));
	if (!col_elements) {
		fprintf(stderr, "Malloc failed\n");
		return;
	}
	for (int i = 0; i < z2; i++)
		scanf("%d", &col_elements[i]);
	for (int i = 0; i < l; ++i) {
		(*mat)[k][i] = (int *)realloc((*mat)[k][i], (c + z2) * sizeof(int));
		if ((*mat)[k][i] == NULL) {
			fprintf(stderr, "Realloc failed\n");
			return;
		}
	}
	for (int i = 0; i < z1; ++i) {
		for (int j = 0; j < z2; ++j) {
			tmp = (*mat)[k][line_elements[i]][col_elements[j]];
			(*mat)[k][i][j + c] = tmp;
		}
	}
	int aux = 0;
	for (int i = 0; i < l; ++i) {
		for (int j = c; j < (c + z2); ++j) {
			(*mat)[k][i][aux] = (*mat)[k][i][j];
			aux++;
		}
		aux = 0;
	}
	for (int i = z1; i < (*lines)[k]; i++)
		free((*mat)[k][i]);
	for (int i = 0; i < z1; i++) {
		(*mat)[k][i] = (int *)realloc((*mat)[k][i], z2 * sizeof(int));
		if ((*mat)[k][i] == NULL) {
			fprintf(stderr, "Realloc failed\n");
			return;
		}
	}
	(*lines)[k] = z1;
	(*col)[k] = z2;
	free(line_elements);
	free(col_elements);
}

int main(void)
{
	int m, n;
	//aceste doua variabile pentru dimensiunile matricilor;
	int count = 0;
	//pentru indexare si sa stiu cata memorie aloc;
	int *lines;
	int *col;
	//lines si col stocheaza dim matricilor;
	int ***mat;
	//aceeasi poveste ca mai sus;
	char oper_let;
	while (1 != 0) {
		scanf(" %c", &oper_let);
		int ok = 1;
		if (oper_let == 'L') {
			scanf("%d%d", &m, &n);
			alloc_dynamic(&mat, &lines, &col, &count, m, n);
			ok = 0;
		}
		if (oper_let == 'D') {
			scanf("%d", &m);
			//folosesc m desi este pentru linii pentru a
			//consuma mai putina memorie
			if (m >= count || m < 0)
				printf("No matrix with the given index\n");
			else
				printf("%d %d\n", lines[m], col[m]);
			ok = 0;
		}
		if (oper_let == 'P') {
			scanf("%d", &m);
			if (m >= count || m < 0)
				printf("No matrix with the given index\n");
			else
				print_mat_index(&mat, &m, &lines, &col);
			ok = 0;
		}
		if (oper_let == 'C') {
			redimension(&mat, &lines, &col, count);
			ok = 0;
		}
		if (oper_let == 'M') {
			int i1, i2;
			scanf("%d%d", &i1, &i2);
			if (i1 >= count || i2 >= count || i1 < 0 || i2 < 0)
				printf("No matrix with the given index\n");
			else
				multiply(&mat, i1, i2, &lines, &col, &count);
			ok = 0;
		}
		if (oper_let == 'O') {
			sort(&mat, &lines, &col, count);
			ok = 0;
		}
		if (oper_let == 'F') {
			int index;
			scanf("%d", &index);
			if (index >= count || index < 0)
				printf("No matrix with the given index\n");
			else
				recycle_bin(&mat, &lines, &col, index, &count);
			ok = 0;
		}
		if (oper_let == 'T') {
			int ind;
			scanf("%d", &ind);
			if (ind >= count || ind < 0)
				printf("No matrix with the given index\n");
			else
				transpose(&mat, &lines, &col, ind);
			ok = 0;
		}
		if (oper_let == 'Q') {
			liberty(&mat, &lines, &col, count);
			break;
		}
		if (oper_let >= 'A' && oper_let <= 'Z' && ok > 0)
			printf("Unrecognized command\n");
	}
}
