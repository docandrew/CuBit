/*
 * CuBit OS - stdio.h
 * Freestanding stdio for CuBit userspace programs.
 */
#ifndef _STDIO_H
#define _STDIO_H

#include <stdarg.h>
#include <stdint.h>

#ifndef NULL
#define NULL ((void *)0)
#endif

#ifndef _SIZE_T_DEFINED
#define _SIZE_T_DEFINED
typedef unsigned long size_t;
#endif

/* FILE type */
typedef struct _FILE FILE;

/* Standard streams (defined in libc_stubs.c) */
extern FILE *stderr;
extern FILE *stdout;
extern FILE *stdin;

/* Seek whence */
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

/* File operations (implemented in stdio.c via IPC to FS server) */
FILE  *fopen(const char *path, const char *mode);
size_t fread(void *ptr, size_t size, size_t nmemb, FILE *stream);
int    fseek(FILE *stream, long offset, int whence);
long   ftell(FILE *stream);
int    fclose(FILE *stream);

/* File write */
size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream);

/* Formatted I/O (implemented in libc_stubs.c) */
int printf(const char *fmt, ...);
int fprintf(FILE *stream, const char *fmt, ...);
int sprintf(char *str, const char *fmt, ...);
int snprintf(char *str, size_t size, const char *fmt, ...);
int vsnprintf(char *str, size_t size, const char *fmt, va_list ap);
int vfprintf(FILE *stream, const char *fmt, va_list ap);
int sscanf(const char *str, const char *fmt, ...);

/* Character I/O */
int puts(const char *s);
int putchar(int c);
int fflush(FILE *stream);
char *fgets(char *s, int size, FILE *stream);
int fputs(const char *s, FILE *stream);
int feof(FILE *stream);
int ferror(FILE *stream);

/* File management stubs */
int remove(const char *pathname);
int rename(const char *oldpath, const char *newpath);

#endif /* _STDIO_H */
