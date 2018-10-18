#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
void *foo(void *vargp)
{
	int myid;
    myid = *((int *)vargp);
    free(vargp);
    printf("Thread %d\n", myid);
}
int main()
{
    pthread_t tid[2];
    int i, *ptr;
    for (i = 0; i < 2; i++)
    {
        ptr = malloc(sizeof(int));
        *ptr = i;
        pthread_create(&tid[i], 0, foo, ptr);
    }
    pthread_join(tid[0], 0);
    pthread_join(tid[1], 0);
}
