#include <pthread.h>
#include <stdio.h>

void *foo(void *vargp) {
    int id;
    id = *((int *)vargp);
    printf("Thread %d\n", id);
    
}
int main()
{
    pthread_t tid[2];
    int i;
    for (i = 0; i < 2; i++)
            pthread_create(&tid[i], NULL, foo, &i);
    
    pthread_join(tid[0], NULL);
    pthread_join(tid[1], NULL);
}
