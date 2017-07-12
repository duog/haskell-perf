#include "sqlite.h"

#ifndef SQLITE_SHELL_IS_UTF8
#  if (defined(_WIN32) || defined(WIN32)) && defined(_MSC_VER)
#    define SQLITE_SHELL_IS_UTF8          (0)
#  else
#    define SQLITE_SHELL_IS_UTF8          (1)
#  endif
#endif

#if SQLITE_SHELL_IS_UTF8
int SQLITE_CDECL main(int argc, char **argv)
#else
int SQLITE_CDECL wmain(int argc, wchar_t **wargv)
#endif
