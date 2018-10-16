#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

void *thread(void *vargp) {
    pthread_detach(pthread_self());
    pthread_exit((void*)42);
}
int main() {
    int i = 0;
    pthread_t tid;
    pthread_create(&tid, NULL, thread, (void*)&i);
    pthread_join(tid, (void**)&i);
    printf("%d\n",i);
}

