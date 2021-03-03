/*  
    csim.c completed by Sizhe Li 1900013061
    
    1. In this program, we first define a struct of cache, including s, E, b, 
    and also an integer named times to record the evicted order.
    When an address is added to the cache, the times equals to E;
    Everytime a new address is added, the times of all other elements minus 1;
    When an eviction happens, the elements whose times equals to 0 is evicted.

    2. When loading, saving or modifying the cache, we first find which group it is in.
    After that, we loop through all the lines to find if it hits.
    If so, we only modify the times of all of the elements.
    Otherwise, we check if it is a cold miss.
    If so, we add this adddress to the cache, and modify the times of all elements.
    Otherwise, we find the one which was visited the earliest and evict it.
*/

#include <stdio.h>
#include <getopt.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include "cachelab.h"

/* $begin unixerror */
void unix_error(char *msg) /* Unix-style error */
{
    fprintf(stderr, "%s: %s\n", msg, strerror(errno));
    exit(0);
}
/* $end unixerror */

void *Malloc(size_t size) 
{
    void *p;

    if ((p  = malloc(size)) == NULL)
	unix_error("Malloc error");
    return p;
}

void Free(void *ptr) 
{
    free(ptr);
    ptr = NULL;
}

// define a struct of cache
typedef struct{
    int avl, sign, times;
}Cache_line;
/*  avl means if the dara in the block is available
    sign means the tag of this line
    times records the order of being visited    */

typedef struct{
    int E_num;
    Cache_line* line;
}Cache_group;

typedef struct{
    int s, E, b;
    Cache_group* group;
}Cache;

Cache cache;
long S;
int miss = 0, hit = 0, eviction = 0;
char* File;

/*  check if the goal address is in the cache
    if so, update the last visit time.
    Otherwise, add it to the cache.         */
void check_Cache(long ad)
{
    long tag_ad = ad >> (cache.s + cache.b);
    long group_ad = (ad >> cache.b) % S;

    // loop through all the lines to find if it hits
    for(int i = 0; i < cache.E; i++)
    {
        if(cache.group[group_ad].line[i].sign == tag_ad && cache.group[group_ad].line[i].avl)
        {
            hit++;
            printf(" hit");           
            for(int j = 0; j < cache.E; j++)
            {
                if(cache.group[group_ad].line[j].times > cache.group[group_ad].line[i].times)
                cache.group[group_ad].line[j].times--;
            }
            // set the 'times' of the most recently visited element to E-1.
            cache.group[group_ad].line[i].times = cache.E - 1;
            return;
        }
    }

    // miss
    miss++;
    printf(" miss");
    for(int i = 0; i < cache.E; i++)
    {
        // when the cache is full, we can find and expel elements that meet the requirements
        if(cache.group[group_ad].line[i].times == 0 && cache.group[group_ad].line[i].avl)
        {
            eviction++;
            printf(" eviction");
            cache.group[group_ad].line[i].sign = tag_ad;
            cache.group[group_ad].line[i].times = cache.E;
            for(int j = 0; j < cache.E; j++)
                cache.group[group_ad].line[j].times--;
            return;
        }

        // when cold miss, we add this one to the cache
        if(cache.group[group_ad].line[i].avl == 0)
        {
            cache.group[group_ad].line[i].avl = 1;
            cache.group[group_ad].line[i].sign = tag_ad;
            cache.group[group_ad].line[i].times = cache.E;
            for(int j = 0; j < cache.E; j++)
                if(cache.group[group_ad].line[j].times > 0)
                    cache.group[group_ad].line[j].times--;
            return;
        }
    }
}

int main(int argc, char* argv[])
{
    int option;
    // evaluate the command parameter   
    while((option = getopt(argc, argv, "s:E:b:t:")) != -1)
    {
        miss = 0, hit = 0, eviction = 0;
        switch(option)
        {
            case 's':
                cache.s = atoi(optarg);
                break;
            case 'E':
                cache.E = atoi(optarg);
                break;
            case 'b':
                cache.b = atoi(optarg);
                break;
            case 't':
                File = optarg;
                break;
        }
    }
    S = (1 << cache.s);
    FILE* file;
    // check if the file is available
    if((file = fopen(File, "r")) == NULL)
    {
        printf("File Not Found!\n");
        exit(-1);
    }

    // initialize the cache
    // use the safe malloc defined earlier
    cache.group = (Cache_group*)Malloc(S*sizeof(Cache_group));
    for(int i = 0; i < S; i++)
    {
        cache.group[i].line = (Cache_line*)Malloc(cache.E*sizeof(Cache_line));
        for(int j = 0; j < cache.E; j++)
        {
            cache.group[i].line[j].avl = 0;
            cache.group[i].line[j].times = 0;
        }
    }

    char op;
    long address;
    int size;

    // read from the file and deal with the address
    while(fscanf(file, " %c %lx,%d", &op, &address, &size) != -1)
    {
        printf("%c %lx,%d", op, address, size);
        switch(op)
        {
            case 'L':
                check_Cache(address);
                break;
            case 'M':
                check_Cache(address);
            case 'S':
                check_Cache(address);
                break;
        }
       printf("\n");
    }

    // free the space
    for(int i = 0; i < cache.s; i++)
        Free(cache.group[i].line);
    Free(cache.group);
    fclose(file);
    printSummary(hit, miss, eviction);
    return 0;
}