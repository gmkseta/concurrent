
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
void *thread(void *vargp)
{
    exit(42);
}
int main()
{
    int i;
    pthread_t tid;
    pthread_create(&tid, NULL, thread, NULL);
    pthread_join(tid, (void **)&i);
    printf("%d\n",i);
    printf("asda");
}
