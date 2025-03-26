
#include <stdio.h>
#include <stdlib.h>

int main(int reason)
{
    int result;

    if (!reason) {

        /* ** DLL initialisation. */
        printf("DLL startup...\n");

        /* return zero to let the load continue */
        result = 0;

    } else {

        /* ** DLL clean up. */
        printf("DLL shutdown...\n");

    }

    return(result);
}


void __export SayHello( char * message)
{
    printf("Received DLL Message: ");
    printf(message);
}

