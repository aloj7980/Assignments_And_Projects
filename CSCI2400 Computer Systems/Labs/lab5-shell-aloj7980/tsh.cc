// 
// tsh - A tiny shell program with job control
// 
// Alex Ojemann 109722375
//

using namespace std;
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>
#include <string>
#include "globals.h"
#include "jobs.h"
#include "helper-routines.h"

static char prompt[] = "tsh> ";
int verbose = 0;
//
// You need to implement the functions eval, builtin_cmd, do_bgfg,
// waitfg, sigchld_handler, sigstp_handler, sigint_handler
//
// The code below provides the "prototypes" for those functions
// so that earlier code can refer to them. You need to fill in the
// function bodies below.
// 
void eval(char *cmdline);
int builtin_cmd(char **argv);
void do_bgfg(char **argv);
void waitfg(pid_t pid);
void sigchld_handler(int sig);
void sigtstp_handler(int sig);
void sigint_handler(int sig);
//
// main - The shell's main routine 
//
int main(int argc, char **argv) 
{
  int emit_prompt = 1; // emit prompt (default)
  //
  // Redirect stderr to stdout (so that driver will get all output
  // on the pipe connected to stdout)
  //
  dup2(1, 2); 
  /* Parse the command line */
  char c;
  while ((c = getopt(argc, argv, "hvp")) != EOF) {
    switch (c) {
    case 'h':             // print help message
      usage();
      break;
    case 'v':             // emit additional diagnostic info
      verbose = 1;
      break;
    case 'p':             // don't print a prompt
      emit_prompt = 0;  // handy for automatic testing
      break;
    default:
      usage();
    }
  }

  Signal(SIGINT,  sigint_handler);   // ctrl-c
  Signal(SIGTSTP, sigtstp_handler);  // ctrl-z
  Signal(SIGCHLD, sigchld_handler);  // Terminated or stopped child

  Signal(SIGQUIT, sigquit_handler); 

  initjobs(jobs);

  for(;;) {

    if (emit_prompt) {
      printf("%s", prompt);
      fflush(stdout);
    }
    char cmdline[MAXLINE];
    if ((fgets(cmdline, MAXLINE, stdin) == NULL) && ferror(stdin)) {
      app_error("fgets error");
    }

    if (feof(stdin)) {
      fflush(stdout);
      exit(0);
    }
    eval(cmdline);
    fflush(stdout);
    fflush(stdout);
  } 
  exit(0); 
}
/////////////////////////////////////////////////////////////////////////////
//
// eval - Evaluate the command line that the user has just typed in
// 
// If the user has requested a built-in command (quit, jobs, bg or fg)
// then execute it immediately. Otherwise, fork a child process and
// run the job in the context of the child. If the job is running in
// the foreground, wait for it to terminate and then return.  Note:
// each child process must have a unique process group ID so that our
// background children don't receive SIGINT (SIGTSTP) from the kernel
// when we type ctrl-c (ctrl-z) at the keyboard.
//
void eval(char *cmdline) {
  /* Parse command line */
  //
  // The 'argv' vector is filled in by the parseline
  // routine below. It provides the arguments needed
  // for the execve() routine, which you'll need to
  // use below to launch a process.
  // The 'bg' variable is TRUE if the job should run
  // in background mode or FALSE if it should run in FG
    char *argv[MAXARGS];
    int bg = parseline(cmdline, argv); 
    pid_t myPid;
    struct job_t *jobp;
    if (!builtin_cmd(argv)) {
        myPid = fork();
        setpgid(0,0);
        if(myPid == 0) {
            execv(argv[0], argv); 
            printf("%s: Command not found\n", argv[0]);
            exit(0);
        }else {
            if(!bg) { 
                addjob(jobs, myPid, FG, cmdline);
                waitfg(myPid); 
            }else {
                addjob(jobs, myPid, BG, cmdline);
                jobp = getjobpid(jobs, myPid);
                printf("[%d] (%d) %s", jobp->jid, myPid, cmdline);
            }
        }
    }
    return;
}
/////////////////////////////////////////////////////////////////////////////
//
// builtin_cmd - If the user has typed a built-in command then execute
// it immediately. The command name would be in argv[0] and
// is a C string. We've cast this to a C++ string type to simplify
// string comparisons; however, the do_bgfg routine will need 
// to use the argv array as well to look for a job number.
//
int builtin_cmd(char **argv) 
{
    string cmd(argv[0]); 
    if (cmd == "quit") { 
        exit(0);
    }else if(cmd == "bg" || cmd == "fg") {
      do_bgfg(argv);
      return 1;
    }else if (cmd == "jobs"){
        listjobs(jobs);
        return 1;
    }
  return 0;     
}

void waitfg(pid_t pid)
{
    while(pid == fgpid(jobs)){
        sleep(.1);
    }
  return;
}

void do_bgfg(char **argv) 
{
  struct job_t *jobp=NULL;
  if (argv[1] == NULL) {
    printf("%s command requires PID or %%jobid argument\n", argv[0]);
    return;
  }
  /* Parse the required PID or %JID arg */
  if (isdigit(argv[1][0])) {
    pid_t pid = atoi(argv[1]);
    if (!(jobp = getjobpid(jobs, pid))) {
      printf("(%d): No such process\n", pid);
      return;
    }
  }
  else if (argv[1][0] == '%') {
    int jid = atoi(&argv[1][1]);
    if (!(jobp = getjobjid(jobs, jid))) {
      printf("%s: No such job\n", argv[1]);
      return;
    }
  }	    
  else {
    printf("%s: argument must be a PID or %%jobid\n", argv[0]);
    return;
  }

  string cmd(argv[0]);
  if(cmd == "bg")  {
      jobp->state = BG; 
      kill(-jobp->pid, SIGCONT); 
      printf("[%d] (%d) %s", jobp->jid, jobp->pid, jobp->cmdline);
  }
  if(cmd == "fg")  {
      jobp->state = FG;
      kill(-jobp->pid, SIGCONT); 
      waitfg(jobp->pid); 
  }
  return;
}
//
// Signal handlers
//
//
// sigchld_handler - The kernel sends a SIGCHLD to the shell whenever
//     a child job terminates (becomes a zombie), or stops because it
//     received a SIGSTOP or SIGTSTP signal. The handler reaps all
//     available zombie children, but doesn't wait for any other
//     currently running children to terminate.  
//
void sigchld_handler(int sig) {
    int statusValue; 
    pid_t myPid;
    while((myPid = waitpid(-1, &statusValue, WNOHANG | WUNTRACED)) > 0) {
        if(WIFSTOPPED(statusValue)) { 
            struct job_t *jobp = getjobpid(jobs, myPid);
            printf("Job [%d] (%d) stopped by signal 20\n", jobp->jid, myPid);
            jobp->state = ST;
            return;
        }else if(WIFSIGNALED(statusValue)) { 
            struct job_t *jobp = getjobpid(jobs, myPid); 
            printf("Job [%d] (%d) terminated by signal 2\n", jobp->jid, myPid);
            deletejob(jobs, myPid);
        }else {
            deletejob(jobs, myPid);
        }
    }
    return; 
}
/////////////////////////////////////////////////////////////////////////////
//
// sigint_handler - The kernel sends a SIGINT to the shell whenver the
//    user types ctrl-c at the keyboard.  Catch it and send it along
//    to the foreground job.  
//
void sigint_handler(int sig) {
    pid_t myPid = fgpid(jobs);
    if (myPid > 0) {
        kill(-myPid, sig);
    }
    return;
}
/////////////////////////////////////////////////////////////////////////////
//
// sigtstp_handler - The kernel sends a SIGTSTP to the shell whenever
//     the user types ctrl-z at the keyboard. Catch it and suspend the
//     foreground job by sending it a SIGTSTP.  
//
void sigtstp_handler(int sig) {
  pid_t myPid = fgpid(jobs);
  if(myPid > 0) {
      kill(-myPid, sig);
  }
  return;
}
/*********************
 * End signal handlers
 *********************/