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

//
// Needed global variable definitions
//

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

  //
  // Install the signal handlers
  //

  //
  // These are the ones you will need to implement
  //
  Signal(SIGINT,  sigint_handler);   // ctrl-c
  Signal(SIGTSTP, sigtstp_handler);  // ctrl-z
  Signal(SIGCHLD, sigchld_handler);  // Terminated or stopped child

  //
  // This one provides a clean way to kill the shell
  //
  Signal(SIGQUIT, sigquit_handler); 

  //
  // Initialize the job list
  //
  initjobs(jobs);

  //
  // Execute the shell's read/eval loop
  //
  for(;;) {
    //
    // Read command line
    //
    if (emit_prompt) {
      printf("%s", prompt);
      fflush(stdout);
    }

    char cmdline[MAXLINE];

    if ((fgets(cmdline, MAXLINE, stdin) == NULL) && ferror(stdin)) {
      app_error("fgets error");
    }
    //
    // End of file? (did user type ctrl-d?)
    //
    if (feof(stdin)) {
      fflush(stdout);
      exit(0);
    }

    //
    // Evaluate command line
    //
    eval(cmdline);
    fflush(stdout);
    fflush(stdout);
  } 

  exit(0); //control never reaches here
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

//In eval, the parent must (1) use sigprocmask to block SIGCHLD signals before it (2) forks the child,
//and then (3) unblock these signals, again using sigprocmask after it adds the child to the job list by
//calling addjob. Since children inherit the blocked vectors of their parents, the child must be sure
//to then unblock SIGCHLD signals before it execs the new program.
//The parent needs to block the SIGCHLD signals in this way in order to avoid the race condition where
//the child is reaped by sigchld handler (and thus removed from the job list) before the parent
//calls addjob.
//
void eval(char *cmdline) 
{
  /* Parse command line */
  //
  // The 'argv' vector is filled in by the parseline
  // routine below. It provides the arguments needed
  // for the execve() routine, which you'll need to
  // use below to launch a process.
  //
  char *argv[MAXARGS];
  pid_t pid; //process ID
  int bic, n; //return values bic=built in command, n=
  sigset_t mask; //Signal Mask for Blocking Signals
  //
  // The 'bg' variable is TRUE if the job should run
  // in background mode or FALSE if it should run in FG
  //
  int bg = parseline(cmdline, argv);
  	// initialize empty signal set
	if(sigemptyset(&mask)) 
		//unix_error is a helper function described in helper_rountines.cc
		unix_error("eval: sigemptyset error");
	// add SIGCHLD signal to signal set
	if(sigaddset(&mask, SIGTSTP)) 
		//unix_error is a helper function
		unix_error("eval: sigaddset error");
//"After parsing the command line, the eval function calls the builtin_command
//function, which checks whether the first command line argument is a built-in shell
//command. If so, it interprets the command immediately and returns 1. Otherwise,
//it returns 0" (CSAPP textbook).
	if((bic = builtin_cmd(argv)) == 0)
	{	// block SIGCHLD  signal
		if(sigprocmask(SIG_BLOCK, &mask, NULL)) 
			unix_error("eval: sigprocmask error");
	
		// create child process with fork command
		if((pid = fork()) < 0) 
			unix_error("eval: fork error");
			
		if(pid == 0)
		{	// child process sets new group pid and executes
			if(setpgrp() == -1) unix_error("eval: setpgrp error");
			
			sigprocmask(SIG_UNBLOCK, &mask, NULL);
		
			if(execvp(argv[0], argv) < 0)
			{	// if execvp fails print message and exit
				printf("%s: Command not found. (%d)\n", argv[0], pid);
				exit(0);
			}
		}	
		
		// Parent Process adds job to jobs list
		if((n = addjob(jobs, pid, (bg == 1 ? BG : FG), cmdline)) == 0) unix_error("eval: addjob error");
			
		//then unblocks SIGCHLD
		sigprocmask(SIG_UNBLOCK, &mask, NULL);	
			
		if(!bg)
		{	//init. waitfg if foreground process
			waitfg(pid);
		}
		else 
		{	//background job prints the job?
			printf("[%d] (%d) %s", pid2jid(pid), pid, cmdline);
		}
	}
	// bi == 1
 
  if (argv[0] == NULL)  
    return;   //ignore empty lines
  return;
}


/////////////////////////////////////////////////////////////////////////////
//
// builtin_cmd - If the user has typed a built-in command then execute
// it immediately. The command name would be in argv[0] and
// is a C string. We've cast this to a C++ string type to simplify
// string comparisons; however, the do_bgfg routine will need 
// to use the argv array as well to look for a job number.

//"After parsing the command line, the eval function calls the builtin_command
//function, which checks whether the first command line argument is a built-in shell
//command. If so, it interprets the command immediately and returns 1. Otherwise,
//it returns 0" (CSAPP textbook).

int builtin_cmd(char **argv) 
{
  string cmd(argv[0]);
  //if user inputs "quit" then exit
  if(!strcmp(argv[0], "quit"))
	{
		exit(0);
	}
   //if user inputs "jobs" call listjobs(jobs)
   else if(!strcmp(argv[0], "jobs"))
	{
		listjobs(jobs);
		return 1;
	}
   //if user inputs "fg" or "bg" call do_bgfg
   else if(!strcmp(argv[0], "fg") || !strcmp(argv[0], "bg"))
	{
		do_bgfg(argv);
		return 1;
	}
   //if not a built in command, then return 0
	else
	{	//not a builtin command
		return 0;
	}
}

/////////////////////////////////////////////////////////////////////////////
//
// do_bgfg - Execute the builtin bg and fg commands
//bg=Change a stopped background job to a running background job.
//fg=Change a stopped or running background job to a running in the foreground.
void do_bgfg(char **argv) 
{
  struct job_t *jobp=NULL;
    
  /* Ignore command if no argument */
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

  //
  // You need to complete rest. At this point,
  // the variable 'jobp' is the job pointer
  // for the job ID specified as an argument.
  //
  // Your actions will depend on the specified command
  // so we've converted argv[0] to a string (cmd) for
  // your benefit.
  //
  string cmd(argv[0]);
  
  return;
}

/////////////////////////////////////////////////////////////////////////////
//
// waitfg - Block until process pid is no longer the foreground process
//
void waitfg(pid_t pid)
{
	struct job_t *job; // holds job
	
	//if foreground job is done, return
	if(!(job = getjobpid(jobs, pid)))
	{	 
		return;
	}
	// loops while state is FG and same process
	while(job->state == FG && job->pid == pid)
	{
		sleep(1);
	}
		  return;
}

/////////////////////////////////////////////////////////////////////////////
//
// Signal handlers
//


/////////////////////////////////////////////////////////////////////////////
//
// sigchld_handler - The kernel sends a SIGCHLD to the shell whenever
//     a child job terminates (becomes a zombie), or stops because it
//     received a SIGSTOP or SIGTSTP signal. The handler reaps all
//     available zombie children, but doesn't wait for any other
//     currently running children to terminate.  
//
void sigchld_handler(int sig) 
{
        int status; // status of job and jid
	int jid; // jid of job
	pid_t pid; // pid of job
	struct job_t *job; // job holder
	
	
	while((pid = waitpid(-1, &status, WNOHANG|WUNTRACED)) > 0)
	{	//find the status of job signal
		if(WIFSTOPPED(status))
		{	//if stopped get job from jobs table
			if(!(job = getjobpid(jobs, pid)))
			{	//print message and return if cant find job
				printf("(%d): No such process.\n", pid);
				return;
			}
			//change state to stopped and print message
			job->state = ST;
			printf("[%d] Stopped %s\n", (job->jid), (job->cmdline));
		}
		else if(WIFSIGNALED(status))
		{	//if signaled delete job and print message
			jid = pid2jid(pid);
			deletejob(jobs, pid);
			printf("Job [%d] (%d) terminated by signal %d\n", jid, pid, WTERMSIG(status));
		}
		else if(WIFEXITED(status))
		{	//if exited just delete job
			jid = pid2jid(pid);
			deletejob(jobs, pid);
		}
		else
		{	//unix error, print message
		unix_error("waitpid error");
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
void sigint_handler(int sig) 
{
  pid_t pid;
	
	if((pid = fgpid(jobs)) > 0)
	{
		if(kill(-pid, SIGINT) < 0) unix_error("sigint_handler: Kill Error.");
	}
	else
	{	/* sent sigint to shell, print message about shell */
		printf("shell's sigint_handler\n");
		printf("shell pid = %d, fg_pid1 = %d, fg_pid2 = %d\n", getpid(), getpgrp(), getppid());
		printf("sending SIGTSTP to process group %d\n", getpgrp());
	}
        return;
}
/////////////////////////////////////////////////////////////////////////////
//
// sigtstp_handler - The kernel sends a SIGTSTP to the shell whenever
//     the user types ctrl-z at the keyboard. Catch it and suspend the
//     foreground job by sending it a SIGTSTP.  
//
void sigtstp_handler(int sig) 
{
	pid_t pid;
	
	if((pid = fgpid(jobs)) > 0)
	{
		if(kill(-pid, SIGTSTP) < 0) unix_error("sigstp_handler: Kill Error.");
	}
	else
	{	/* sent sigtstp to shell, print message about shell */
		printf("shell's sigstp_handler\n");
		printf("shell pid = %d, fg_pid1 = %d, fg_pid2 = %d\n", getpid(), getpgrp(), getppid());
		printf("sending SIGSTP to process group %d\n", getpgrp());
	}
        return;
}

/*********************
 * End signal handlers
 *********************/