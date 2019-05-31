#include <stdio.h>
#include <stdlib.h>

// Sieve of Eratosthenes proof of concept in C
// Todo: 1) Efficient refactoring of array at particular points.
//          Waste of memory that could go towards finding more primes.
//       2) Use of heap to allocate more memory to find bigger primes.
//          Stack can only hold so much before a segfault occurs.
//          Update: Lmao heap gets exhausted at 1 billion primes.
//                  Assuming I got this algorithm right, it still finds primes
//                  up to 250 million.
//                  Displaying them is harder than actually finding them.
void v1();
void v2(int); 
void main(){
    v2(250000000);
}

void v1(){
    int size = 500000;
    unsigned int nums[size];
    printf("Size of nums: %d\n", sizeof(nums));

    int a[17];
    printf("Size of 17 bit array: %d\n", sizeof(a));
    printf("Ints are %d bytes long.\n", sizeof(a)/17);
    
    int i;
    int j;
    
    for(i = 2;i<=size;i++){
        nums[i] = i;
        // printf("%d\n", i);
    }

    for(i = 2;i<size;i++){
        if(nums[i] != 0){
            for(j = i*2;j<size;j+=i){
                // printf("%d is a multiple of %d, zeroing\n", j, i);
                nums[j] = 0;
            }
        }
    }

    for(i=0;i<size;i++){
        if(nums[i] != 0){
            //printf("%d, ", i);
        }
    }
}

void v2(int size){
    int *buff = NULL;    // Initializes an int pointer. Have to before use.
    int i, j;
    
    buff = (int *) malloc(sizeof(int) * size);    // Cast malloc to int pointer

    if (buff == NULL){
        fprintf(stderr, "Failed malloc\n");
        return;
    }


    for(i = 2;i<=size;i++){
        // *(buff + i) = i;
        // printf("Added to buffer: %d\n", *(buff + i));
        buff[i] = i;
        //printf("Added to the buffer: %d\n", buff[i]);
    }
    printf("Added %d elements to the buffer\n", size);

    // Do some pointer arithmetic on buff[] to deallocate memory?
    // But them memory isn't actually deallocated. Pointer is just moved.
    // Maybe partition buff into discrete sections of memory pointers?
    for(i = 2;i<=size;i++){
        if(buff[i] != 0) {
            for(j = i*2;j<size;j+=i){
                buff[j] = 0;
            }
        }
    }

    printf("Primes up to %d were found\n", size);

    for(i=0;i<size;i++){
        if(buff[i] != 0){
            printf("%d, ", i);
        }
    }
    
    free(buff);
}


