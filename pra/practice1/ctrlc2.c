#include <signal.h>
#include <stdio.h>
#include <unistd.h>
void ouch(int sig) {
  printf("OUCH! - I got signal %d\n", sig);
}

int main() {
  struct sigaction act;
  act.sa_handler = ouch;
  sigemptyset(&act.sa_mask);//sa_mask 사용시 경쟁조건 제거라고? 차단 안됨?..
  act.sa_flags = 0;
  sigaction(SIGINT, &act, 0);//이전 액션이 없으면서 sa_flags가 0
  while(1) {
    printf("Hello World!\n");
    sleep(1);
  }
}
