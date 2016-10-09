/* mktable.c - filter to extract table entries */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

/*
given input string of the form:
<whitespace>DEFINE_<key><rest-of-line>
writes to the output:
<key><rest-of-line>
*/

void extract (char *key)
{
  static char buf[200];
  char *def = "DEFINE_";
  static char defkey[50];
  sprintf (defkey, "%s%s", def, key);
  { int deflen = strlen (def);
    int defkeylen = strlen (defkey);
    while (fgets(buf, sizeof(buf)-1, stdin) != NULL) {
      /* skip leading whitespace */
      char *cp = buf; while (isspace (*cp)) cp++;
      if (strncmp (defkey, cp, defkeylen) == 0)
        puts(cp+deflen);
    }
  }
}

int main(int argc, char *argv[])
{
  if (argc != 2) {
    printf("usage: mktable KEY <infile >outfile\n");
    exit(0);
  }
  extract(argv[1]);
  return 0;
}
