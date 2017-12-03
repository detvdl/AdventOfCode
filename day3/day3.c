#include <stdio.h>
#include <stdlib.h>
#include <math.h>

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

/* After n steps, there is a unique, event number 2k such that: */
/* 2k*(2k+1) < n <= (2k+2)*(2k+3) */
/* at which point we've gone through k blocks and an additional n - 2k*(2k+1) steps */
/* This results in te following system of equations: */
/* -- (n - 4k^2 - 3k,     k)                  if 2k(2k+1)     < n <= (2k+1)^2 */
/* -- (k + 1,             4k^2 + 5k + 1 - n)  if (2k+1)^2     < n <= 2(k+1)(2k+1) */
/* -- (4k^2 + 7k + 3 - n, -k - 1)             if 2(k+1)(2k+1) < n <= 4(k+1)^2 */
/* -- (-k - 1,            n - 4k^2 -9k - 5)   if 4(k+1)^2     < n <= 2(k+1)(2k+3) */
/* with k = ceil(sqrt(n) - 1 / 2) */

/* TODO: clean this up a little */
struct index_2d *get_center_offset(int number)
{
        struct index_2d *center_offset = (struct index_2d *) malloc(sizeof(*center_offset));
        int k = (int) ceil((sqrt(number) - 1) / 2);
        int t = 2 * k + 1;
        int m = pow(t, 2);

        t -= 1;

        if (number >= m - t) {
                center_offset->row = k;
                center_offset->col = k - (m - number);
                return center_offset;
        }

        m -= t;

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
        } else {
                center_offset->row = -k - (m - number - t);
                center_offset->col = k;
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
        int dim = ceil(sqrt(number));

        struct index_2d *center = (struct index_2d *) malloc(sizeof(*center));
        center->row = (int) floor(dim/2);
        center->col = (dim % 2) == 0 ? (int) ceil(dim/2 - 1) : (int) floor(dim/2);

        printf("Center point is at: (%d, %d)\n", center->row, center->col);

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

        while(value < number) {
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
                if(-dim/2 < x && x <= dim/2 && -dim/2 < y && y <= dim/2) {
                        value = matrix[newx-1][newy-1] + matrix[newx-1][newy] + matrix[newx-1][newy+1]
                                + matrix[newx][newy-1] + matrix[newx][newy+1]
                                + matrix[newx+1][newy-1] + matrix[newx+1][newy] + matrix[newx+1][newy+1];
                        matrix[newx][newy] = value;
                } else break;
        }

        free(center);
        free(delta);
        free(matrix);

        return value;
}
