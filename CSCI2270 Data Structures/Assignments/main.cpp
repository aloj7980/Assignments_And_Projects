#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

void parse(char *buf, char **args);
void execute(char **args);

int main(int argc, char *argv[])
{
char buf[1024];
char *args[64];
int n = 0;
for (;;) {
/*
* Prompt for and read a command.
*/
printf("Command: ");
  
if (fgets(buf, 1024, stdin) == NULL) {
printf("\n");
exit(0);
}
  
   while(buf[n] != '\n')
       n++;
     
   buf[n] = '\0'; //remove the newline character at end
  
/*
* Split the string into arguments.
*/
parse(buf, args);

/*
* Execute the command.
*/
execute(args);
}
}

/*
* parse--split the command in buf into
* individual arguments.
*/
void parse(char *buf, char **args)
{
while (*buf != '\0') {
/*
* Strip whitespace. Use nulls, so
* that the previous argument is terminated
* automatically.
*/
while ((*buf == ' ') || (*buf == '\t'))
*buf++ = '\0';

/*
* Save the argument.
*/
*args++ = buf;

/*
* Skip over the argument.
*/
while ((*buf != '\0') && (*buf != ' ') && (*buf != '\t'))
buf++;
}

*args = NULL;
}

/*
* execute--spawn a child process and execute
* the program.
*/
void execute(char **args)

{
//fill in your code here
pid_t pid;
switch (pid = fork()) {
case -1:
perror("error");

case 0:
execvp(args[0], args);
perror(*args);
exit(1);
}
}

