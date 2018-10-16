#include <pthread.h>
#include <semaphore.h>
#include <stdio.h>
void *thread(void *vargp) {
    pthread_exit((void*)42);
}


int main() {
    int i;
    pthread_t tid;
    pthread_create(&tid, NULL, thread, NULL);
    pthread_join(tid, (void **)&i);
    printf("%d\n",i);
}


