#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <string.h>

/* Struct declarations */
struct index_2d {
        int row;
        int col;
};

struct vector_2d {
        int x;
        int y;
};

/* Function declarations */
struct index_2d *get_center_offset(int input);
int get_manhattan_distance(int input);
int get_sumspiral_larger(int input);

int main(int argc, char **argv)
{
        if(argc < 2) {
                printf("Please provide the puzzle input");
                exit(1);
        }
        int input = atoi(argv[1]);

        /* Puzzle 1 */
        struct index_2d *center_offset = get_center_offset(input);
        printf("Offset for %d from center is: (%d, %d)\n", input, center_offset->row, center_offset->col);

        int steps = abs(center_offset->row) + abs(center_offset->col);
        printf("Steps required to reach the access point: %d\n", steps);

        free(center_offset);

        /* Puzzle 1 - alternative */
        int steps_manh = get_manhattan_distance(input);
        printf("Alternate steps calculation: %d\n", steps_manh);

        /* Puzzle 2 */
        int larger = get_sumspiral_larger(input);
        printf("First number larger than %d is: %d\n", input, larger);

        return 0;
}

/* Example spiral: */
/* ---------------------- */
/* | 17  16  15  14  13 | */
/* | 18   5   4   3  12 | */
/* | 19   6   1   2  11 | */
/* | 20   7   8   9  10 | */
/* | 21  22  23---> ... | */
/* ---------------------- */

/* Let R, D, L, U = Right, Down, Left, Up */
/* To find a pattern, let's denote the first couple of moves */

/* RU | LLDD | RRRUUU | LLLLDDDD | RRRRRUUUUU | ... */
/* Annotating with exponents we arrive at */
/* R1U1 | L2D2 | R3U3 | L4D4 | R5U5 | L6D6 | ... */
/* Now let's call any RULD sequence a `block` */
/* The first n blocks now consist of sum(k=1->m) k = m*(m+1) steps */
/* The k-th block is then R(2k+1) U(2k+1) L(2k+2) D(2k+2) */

/* After n steps, there is a unique, even number 2k such that: */
/* 2k*(2k+1) < n <= (2k+2)*(2k+3) */
/* at which point we've gone through k blocks and an additional n - 2k*(2k+1) steps */
/* This results in te following system of equations: */
/* -- (n - 4k^2 - 3k,     k)                  if 2k(2k+1)     < n <= (2k+1)^2 */
/* -- (k + 1,             4k^2 + 5k + 1 - n)  if (2k+1)^2     < n <= 2(k+1)(2k+1) */
/* -- (4k^2 + 7k + 3 - n, -k - 1)             if 2(k+1)(2k+1) < n <= 4(k+1)^2 */
/* -- (-k - 1,            n - 4k^2 -9k - 5)   if 4(k+1)^2     < n <= 2(k+1)(2k+3) */
/* with k = ceil(sqrt(n) - 1 / 2) */

struct index_2d *get_center_offset(int number)
{
        struct index_2d *center_offset = (struct index_2d *) malloc(sizeof(*center_offset));
        int k = ceil((sqrt(number) - 1) / 2);
        int t = 2 * k + 1;
        int m = pow(t, 2);

        t -= 1;
        if (number >= m - t) {
                center_offset->row = k - (m - number);
                center_offset->col = -k;
                return center_offset;
        }

        m -= t;
        if (number >= m - t) {
                center_offset->row = -k;
                center_offset->col = -k + (m - number);
                return center_offset;
        }

        m -= t;
        if (number >= m - t) {
                center_offset->row = -k + (m - number);
                center_offset->col = k;
                return center_offset;
        } else {
                center_offset->row = k;
                center_offset->col = k - (m - number - t);
                return center_offset;
        }
}

/* Example spiral: */
/* --------------------------- */
/* | 147  142  133  122   59 | */
/* | 304    5    4    2   57 | */
/* | 330   10    1    1   54 | */
/* | 351   11   23   25   26 | */
/* | 362  747  806   --> ... | */
/* --------------------------- */
int get_sumspiral_larger(int number) {
        int dim = (int) 2 * ceil(log(number)/log(16));
        /* Hacky way to circumvent out-of-bounds segfaults for small arrays */
        if(number < 25) dim += 3;

        struct index_2d *center = (struct index_2d *) malloc(sizeof(*center));
        center->row = (int) floor(dim/2);
        center->col = (dim % 2) == 0 ? (int) ceil(dim/2 - 1) : (int) floor(dim/2);

        printf("Spiral dimensions:\nx:%d\ty:%d\nCenter point is at: (%d, %d)\n",
               dim, dim, center->row, center->col);

        struct vector_2d *delta = (struct vector_2d *) malloc(sizeof(*delta));
        delta->x = 1;
        delta->y = 0;

        int **matrix = (int **) calloc(dim, sizeof(*matrix));
        for(int i = 0; i < dim; i++) {
                matrix[i] = (int *) calloc(dim, sizeof(*matrix[i]));
        }

        int x = 0, y = 0;
        int value = 1;
        matrix[center->row][center->col] = value;

        while(!(value > number)) {
                /* Directional chain is RULD */
                /* So the sequence is [0, 1] -> [-1, 0] -> [0, -1] -> [1, 0] */
                /* We switch directions every time we hit a corner/edge */
                if(x == -y || (y < 0 && y == x) || (y > 0 && y == (1 + x))) {
                        int temp = delta->x;
                        delta->x = -delta->y;
                        delta->y = temp;
                }
                x += delta->x;
                y += delta->y;
                int newx = center->row + x;
                int newy = center->col + y;

                /* Naive way to check out-of-bounds for surrounding square */
                /* [-1, -1] [-1, 0] [-1, 1] */
                /* [0, -1] @[0, 0]@ [0, 1] */
                /* [1, -1]  [1, 0]  [1, 1] */
                value = 0;
                int ring[3];
                memcpy(ring, (int []) { -1, 0, 1 }, sizeof(ring));
                for(int j = 0; j < 3; j++) {
                        for(int k = 0; k < 3; k++) {
                                if(0 <= newx + ring[j] && newx + ring[j] < dim
                                   && 0 <= newy + ring[k] && newy + ring[k] < dim)
                                {
                                        value += matrix[newx+ring[j]][newy+ring[k]];
                                }
                        }
                }
                matrix[newx][newy] = value;
        }

        free(center);
        free(delta);
        free(matrix);

        return value;
}

/* Extra: Easy way of solving puzzle 1 with Manhattan Distance */
int get_manhattan_distance(int input) {
        int base = (int) ceil(sqrt(input));

        int side = (base % 2) == 0 ? base + 1 : base;
        /* Steps from center to radius */
        int steps = (side - 1) / 2;
        /* Offset from previous radius */
        int prev_offset = (int) floor(input - pow((side - 2), 2));
        /* Side and element */
        int center_offset = (side > 1) ? prev_offset % (side - 1) : 0;

        return steps + abs(center_offset - steps);
}
