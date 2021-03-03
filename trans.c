/*
 * trans.c - Matrix transpose B = A^T
 * 
 * #######################################################
 * ## trans.c completed by Sizhe Li 1900013061          ##
 * ## Method:                                           ##
 * ##   Consider the three different values of M,       ##
 * ##   then deal with them with different methods.     ##
 * ##   (See the notes in the codes for details)        ##
 * #######################################################
 *
 * Each transpose function must have a prototype of the form:
 * void trans(int M, int N, int A[N][M], int B[M][N]);
 *
 * A transpose function is evaluated by counting the number of misses
 * on a 1KB direct mapped cache with a block size of 32 bytes.
 */
#include <stdio.h>
#include "cachelab.h"
#include "contracts.h"

int is_transpose(int M, int N, int A[N][M], int B[M][N]);

/*
 * transpose_submit - This is the solution transpose function that you
 *     will be graded on for Part B of the assignment. Do not change
 *     the description string "Transpose submission", as the driver
 *     searches for that string to identify the transpose function to
 *     be graded. The REQUIRES and ENSURES from 15-122 are included
 *     for your convenience. They can be removed if you like.
 */
char transpose_submit_desc[] = "Transpose submission";
void transpose_submit(int M, int N, int A[N][M], int B[M][N])
{
    if (M == 32) // operate a block of 8*8 each time
    {
        for(int i = 0; i < M / 8; i++)
            for(int j = 0; j < N / 8; j++)
            {
                for(int k = i * 8; k < (i + 1) * 8; k++)
                {
                    int x0, x1, x2, x3, x4, x5, x6, x7;
                    x0 = A[j*8][k], x1 = A[j*8+1][k], x2 = A[j*8+2][k], x3 = A[j*8+3][k];
                    x4 = A[j*8+4][k], x5 = A[j*8+5][k], x6 = A[j*8+6][k], x7 = A[j*8+7][k];
                    B[k][j*8] = x0, B[k][j*8+1] = x1, B[k][j*8+2] = x2, B[k][j*8+3] = x3;
                    B[k][j*8+4] = x4, B[k][j*8+5] = x5, B[k][j*8+6] = x6, B[k][j*8+7] = x7;
                }
            }      
    }

    else if(M == 64) 
    // Divide an 8 * 8 block into 4 parts, and store them at the wrong position
    // temporarily  to avoid conflict
    {
        for(int i = 0; i < M / 8; i++)
        {
            for(int j = 0; j < N / 8; j++)
            {
                for(int k = 8 * j; k < 8 * j + 4; k++)
                // deal with the upper two of the 8 * 8 blocks
                {
                    int x0, x1, x2, x3, x4, x5, x6, x7;
                    x0 = A[k][8*i], x1 = A[k][8*i+1], x2 = A[k][8*i+2], x3 = A[k][8*i+3];
                    x4 = A[k][8*i+4], x5 = A[k][8*i+5], x6 = A[k][8*i+6], x7 = A[k][8*i+7];

                    // The block in the upper left corner is transposed and saved. 
                    // The block in the upper right corner needs to be operated again
                    B[8*i][k] = x0, B[8*i+1][k] = x1, B[8*i+2][k] = x2, B[8*i+3][k] = x3;
                    B[8*i+3][k+4] = x4, B[8*i+2][k+4] = x5, B[8*i+1][k+4] = x6, B[8*i][k+4] = x7;
                }

                for(int k = 0; k < 4; k++)
                {
                    // Read the bottom two pieces of the original 8 * 8 blocks by column
                    int x0, x1, x2, x3, x4, x5, x6, x7;
                    x0 = A[8*j+4][8*i+3-k], x1 = A[8*j+5][8*i+3-k];
                    x2 = A[8*j+6][8*i+3-k], x3 = A[8*j+7][8*i+3-k];
                    x4 = A[8*j+4][8*i+4+k], x5 = A[8*j+5][8*i+4+k];
                    x6 = A[8*j+6][8*i+4+k], x7 = A[8*j+7][8*i+4+k];

                    // Put them in the right place and sort out the elements in B 
                    // that were not put in the correct position in the previous step
                    B[8*i+4+k][8*j+4] = x4, B[8*i+4+k][8*j+5] = x5;
                    B[8*i+4+k][8*j+6] = x6, B[8*i+4+k][8*j+7] = x7;
                    B[8*i+4+k][8*j] = B[8*i+3-k][8*j+4], B[8*i+4+k][8*j+1] = B[8*i+3-k][8*j+5];
                    B[8*i+4+k][8*j+2] = B[8*i+3-k][8*j+6], B[8*i+4+k][8*j+3] = B[8*i+3-k][8*j+7];

                    B[8*i+3-k][8*j+4] = x0, B[8*i+3-k][8*j+5] = x1;
                    B[8*i+3-k][8*j+6] = x2, B[8*i+3-k][8*j+7] = x3;
                }
            }
        }
    }

    else if(M == 60) 
    // Still disassembled into 8 * 8, and the diagonal elements are specially treated
    {
        for(int i = 0; i < M; i += 8)
            for(int j = 0; j < N; j += 8)
                for(int k = j; k < j + 8 && k < N; k++)
                {
                    int pos = -1, val;                   
                    int x0 = -1, x1 = -1, x2 = -1, x3 = -1, x4 = -1, x5 = -1, x6 = -1, x7 = -1;
                    
                    // Determine if the element is on the diagonal
                    if(i == k)
                    {
                        pos = k;
                        val = A[k][k];
                    }
                    else x0 = A[k][i];

                    if(i + 1 == k)
                    {
                        pos = k;
                        val = A[k][k];
                    }
                    else x1 = A[k][i + 1];

                    if(i + 2 == k)
                    {
                        pos = k;
                        val = A[k][k];
                    }
                    else x2 = A[k][i + 2];

                    if(i + 3 == k)
                    {
                        pos = k;
                        val = A[k][k];
                    }
                    else x3 = A[k][i + 3];

                    // The last four elements may be out of bounds
                    if(i + 4 < M)
                    {
                        if(i + 4 == k)
                        {
                            pos = k;
                            val = A[k][k];
                        }
                        else x4 = A[k][i + 4];
                    }

                    if(i + 5 < M)
                    {
                        if(i + 5 == k)
                        {
                            pos = k;
                            val = A[k][k];
                        }
                        else x5 = A[k][i + 5];
                    }

                    if(i + 6 < M)
                    {
                        if(i + 6 == k)
                        {
                            pos = k;
                            val = A[k][k];
                        }
                        else x6 = A[k][i + 6];
                    }

                    if(i + 7 < M)
                    {
                        if(i + 7 == k)
                        {
                            pos = k;
                            val = A[k][k];
                        }
                        else x7 = A[k][i + 7];
                    }

                    if(x0 > 0)
                        B[i][k] = x0;
                    if(x1 > 0)
                        B[i+1][k] = x1;
                    if(x2 > 0)
                        B[i+2][k] = x2;
                    if(x3 > 0)
                        B[i+3][k] = x3;
                    if(x4 > 0)
                        B[i+4][k] = x4;
                    if(x5 > 0)
                        B[i+5][k] = x5;
                    if(x6 > 0)
                        B[i+6][k] = x6;
                    if(x7 > 0)
                        B[i+7][k] = x7;
                        
                    if(pos >= 0)
                        B[pos][pos] = val;
                }
    }
}

/*
 * You can define additional transpose functions below. We've defined
 * a simple one below to help you get started.
 */

 /*
  * trans - A simple baseline transpose function, not optimized for the cache.
  */
char trans_desc[] = "Simple row-wise scan transpose";
void trans(int M, int N, int A[N][M], int B[M][N])
{
    int i, j, tmp;

    REQUIRES(M > 0);
    REQUIRES(N > 0);

    for (i = 0; i < N; i++) {
        for (j = 0; j < M; j++) {
            tmp = A[i][j];
            B[j][i] = tmp;
        }
    }

    ENSURES(is_transpose(M, N, A, B));
}

/*
 * registerFunctions - This function registers your transpose
 *     functions with the driver.  At runtime, the driver will
 *     evaluate each of the registered functions and summarize their
 *     performance. This is a handy way to experiment with different
 *     transpose strategies.
 */
void registerFunctions()
{
    /* Register your solution function */
    registerTransFunction(transpose_submit, transpose_submit_desc);
}

/*
 * is_transpose - This helper function checks if B is the transpose of
 *     A. You can check the correctness of your transpose by calling
 *     it before returning from the transpose function.
 */
int is_transpose(int M, int N, int A[N][M], int B[M][N])
{
    int i, j;

    for (i = 0; i < N; i++) {
        for (j = 0; j < M; ++j) {
            if (A[i][j] != B[j][i]) {
                return 0;
            }
        }
    }
    return 1;
}