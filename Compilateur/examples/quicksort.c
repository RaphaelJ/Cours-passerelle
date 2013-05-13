/* Teste une implémentation d'un QuickSort écrit en Coda en vérifiant que le
 * résultat du tri d'un tableau de dix millions d'entiers aléatoirement
 * initialisés est un tableau trié. */

#include <stdio.h>
#include <stdlib.h>

#include "coda.h"

const int ARRAY_LEN = 10000000;

void quicksort(cint_t arr[], cint_t left, cint_t right);
// int int_cmp(const void *v_a, const void *v_b);

int main(void)
{
    srand(1);

    // Alloue le tableau sur la pile pour éviter un stack overflow.
    cint_t *arr = malloc(sizeof (cint_t) * ARRAY_LEN);

    for (int i = 0; i < ARRAY_LEN; i++)
        arr[i] = rand();

    printf("Sorting %d integers ...\n", ARRAY_LEN);

    // qsort(arr, ARRAY_LEN, sizeof (cint_t), int_cmp);
    quicksort(arr, 0, ARRAY_LEN);

    // Vérifie si le tableau est correctement trié.
    for (int i = 0; i < ARRAY_LEN - 1; i++) {
        if (arr[i] > arr[i + 1]) {
            printf("Array is NOT sorted.\n");
            return 1;
        }
    }

    printf("Array is sorted.\n");
    return 0;
}

// int int_cmp(const void *v_a, const void *v_b)
// {
//     return (int) (*((cint_t *) v_a) - *((cint_t *) v_b));
// }
