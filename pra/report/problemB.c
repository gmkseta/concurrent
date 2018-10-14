#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>

// This function will handle a signal.
void HandleSignal(int sig, siginfo_t *si, void *context);

int main(int argc, char *argv[]) {
  struct sigaction sVal;
  pid_t myPID;
  pid_t myG_PID;
  // Specify that we will use a signal handler that takes three arguments
  // instead of one, which is the default.
  sVal.sa_flags = SA_SIGINFO;

  // Indicate which function is the signal handler.
  sVal.sa_sigaction = HandleSignal;

  myPID = getpid();
  myG_PID = getpgid(myPID);
  printf("\nMy process id = %d.\n", myPID);
  printf("My process group id = %d.\n", myG_PID);

  if(fork() == 0) {//자식 프로세스일때.
    myPID = getpid();
    myG_PID = getpgid(myPID);
    printf("\nChild: My process id = %d.\n", myPID);
    printf("Child: My process group id = %d.\n", myG_PID);
    // Create a new process group that contains this process

    setpgid(0,0); //
    myPID = getpid();
    myG_PID = getpgid(myPID);
    printf("\nChild: My process id = %d.\n", myPID);
    printf("Child: My process group id = %d.\n", myG_PID);
  }
  else
  {
    // Register for SIGINT
    sigaction(SIGINT, &sVal, NULL);
    // Register for SIGCHLD
    sigaction(SIGCHLD, &sVal, NULL);

    myPID = getpid();
    myG_PID = getpgid(myPID);

    printf("\nParent: My process id = %d.\n", myPID);
    printf("Parent: My process group id = %d.\n", myG_PID);
    while(1) {}
  }
  return(0);
}

void HandleSignal(int sig, siginfo_t *si, void *context) {
  switch(sig)
  {
    case SIGINT:
      printf("\nControl-C was pressed: mypid = %d, mypgid = %d\n",getpid(), getpgid(getpid()));
      _exit(0);
      break;
    case SIGCHLD:
      printf("\nSIGCHLD. mypid = %d, mypgid = %d\n", getpid(), getpgid(getpid()));
      if(si->si_code == CLD_EXITED || si->si_code == CLD_KILLED) {
        printf("Process %d is done!\n", si->si_pid);
      }
        break;
  }
}
