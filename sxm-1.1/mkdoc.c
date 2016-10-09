/* mkdoc.c - filter to extract documentation */

#include <stdio.h>
#include <stdlib.h>

/*
 * scan - scan input at given nesting level. returns 0 if EOF was found or 1
 * if level was terminated by |#
 */

int scan(int level)
{
  int ch0, ch1;

  /* look for |# or #| sequences */
  for (;;) {
    ch0 = fgetc(stdin);
check_ch0:
    switch (ch0) {
      case EOF:
	return 0;
      case '|':
	ch1 = getc(stdin);
	if (ch1 != '#') {
	  /* false alarm! continue scan */
	  if (level == 1)
	    fputc(ch0, stdout);
	  ch0 = ch1;
	  goto check_ch0;
	} else {
	  /* level is terminated - return to previous one */
	  if (level == 1)
	    fputc('\n', stdout);
	  return 1;
	}
      case '#':
	ch1 = getc(stdin);
	if (ch1 != '|') {
	  /* false alarm! continue scan */
	  if (level == 1)
	    fputc(ch0, stdout);
	  ch0 = ch1;
	  goto check_ch0;
	} else {
	  /* start of nested comment - scan at higher level */
	  if (scan(level + 1) == 0) {
	    fputs("\nError: Unexpected EOF (unbalanced #| comment)\n",
		  stderr);
	    exit(1);
	  }
	  /* nested comment terminated normally - continue */
	}
	break;
      default:
	if (level == 1)
	  fputc(ch0, stdout);
	break;
    }
  }
}

/* main - scan input at level 0 */
int main(int argc, char *argv[])
{
  if (scan(0) != 0) {
    /* level 0 terminated by |# ? */
    fputs("\nError: Unbalanced comment (extra |#)\n", stderr);
    return 1;
  } else
    return 0;
}
