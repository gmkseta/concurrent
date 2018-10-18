#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
void *thread(void *vargp)
{
    int *ptr = (int*)vargp;
    pthread_exit((void*)*ptr);
}
void *thread2(void *vargp)
{
    int *ptr = (int*)vargp;
    *ptr = 0;
    pthread_exit((void*)31);
}
int main() {
    int i = 42;
    pthread_t tid, tid2;
    pthread_create(&tid, NULL, thread, (void*)&i);
    pthread_create(&tid2, NULL, thread2, (void*)&i);
    pthread_join(tid, (void**)&i);
    pthread_join(tid2, NULL);
    printf("%d\n",i);
}
