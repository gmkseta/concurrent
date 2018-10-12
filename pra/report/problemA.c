/*
Signal Example with wait() and waitpid() SIGUSR1, SIGUSR2, and SIGINT
*/
#include <signal.h>
#include <stdio.h>
#include <sys/wait.h>
#include <errno.h>

static void signal_handler(int);

int i, pid1, pid2, status;

int main( int argc, char *argv[], char *env[] )
{
  int exit_status;

  if( signal( SIGUSR1, signal_handler) == SIG_ERR )
  {//SIGUSR1 에대한 핸들러 설정
    printf("P?ent: Unable to create handler for SIGUSR1\n");
  }

  if( signal( SIGUSR2, signal_handler) == SIG_ERR )
  {//SIGUSR2 에대한 핸들러 설정
    printf("P?ent: Unable to create handler for SIGUSR2\n");
  }

  printf( "Parent pid = %d\n", pid1=getpid());

  if( (pid2 = fork()) == 0 )
  {
    printf( "Child pid = %d\n", getpid() );
    printf( "Child: sending parent SIGUSR1\n", getpid() );
    kill( pid1, SIGUSR1 );
    for( ;; );
    /* loop forever */
  }
  else {
    /*
    * This waits for ANY child to die. It doesn't matter if the child
    * dies normally or from a signal. The status information is then
    * stored in the status integer.
    *
    * If you want to wait on a particular process use waitpid():
    * waitpid( childPID, &status, 0 );
    * is the common usage.
    *
    * Solaris acts weirdly when a signal is given to the parent process.
    * Therefore we place the wait() inside a while loop so that wait()
    * will not return before the child has died. */
    /* while( (wait( &status ) == -1) && (errno == EINTR) ) {} */
    wait(&status);
    /*
    * The information in status is *NOT* the return code!! To make use
    * of the information we must macros to extract the needed
    * information.
    */
    /* WIFEXITED() determines if the process exited normally (returned a
    * number). This can be done through a return or exit()
    */
    if( WIFEXITED( status ) )
    {
      /*
      * Now we know the process exited properly so we can get the
      * return value
      *
      * Note that WEXITSTATUS only retuns the lower 8 bits! That means
      * that if we ever expect a negative number then we have to count
      * the 8th bit as a sign bit.
      */

      exit_status = WEXITSTATUS( status );

      /*
      * Since we expect negative numbers...
      *
      * If the exit_status is greater than 2^7 (128), th? the eigth bit
      * is a 1, so we subtract 2^8 (256) from it to make it look like
      * a negative number. */
      if( exit_status > 128 )
      {
        exit_status -= 256;
      }
      printf( "Child return - %d\n", WEXITSTATUS( status ) );
    }
    else
    {
      /* Well it didn't exit properly. Was it a signal? */
      if( WIFSIGNALED( status ) )
      {
        /*
        * Yes. A signal killed the child process. Now we can extract
        * the signal information from status
        */
        printf( "Child died on signal - %d\n", WTERMSIG( status ));
      }
    }
    /*
    * There are two other macros most UNIXes use. They are:
    *WIFSTOPPED() and WSTOPSIG(). See the man pages on the dells for
    *more information.
    *To wait on a particular pid - see waitpid()
    */
  }
  return 0;
}




static void signal_handler(int signo)
{
  /* signo contains the signal number that was received */
  switch( signo )
  {
    /* Signal is a SIGUSR1 */
    case SIGUSR1:
      printf( "Process %d: received SIGUSR1 \n", getpid() );
      if(pid1==getpid()) /* it is the parent */
      {
        printf( "Process %d is passing SIGUSR1 to %d...\n", getpid(),pid2 );
        kill( pid2, SIGUSR1 );
      }
      else /* it is the child */
      {
        printf( "Process %d is passing SIGUSR2 to itself...\n", getpid());
        kill(getpid(), SIGUSR2);
      }
      break;
      /* It's a SIGUSR2 */
    case SIGUSR2:
      printf( "Process %d: received SIGUSR2 \n", getpid() );
      if(pid1==getpid())
      {
        printf( "Process %d is passing SIGUSR2 to %d...\n", getpid(),pid2 );
        kill( pid2, SIGUSR2 );
      }
      else /* it is the child */
      {
        printf( "Process %d will terminate itself using SIGINT\n", getpid());
        kill(getpid(), SIGINT);
      }
      break;
    default:
      break;
    }
  return;
}
