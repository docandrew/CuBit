/*
 * CuBit OS - stdlib.h
 */
#ifndef _STDLIB_H
#define _STDLIB_H

#ifndef _SIZE_T_DEFINED
#define _SIZE_T_DEFINED
typedef unsigned long size_t;
typedef long ssize_t;
#endif

#ifndef NULL
#define NULL ((void *)0)
#endif

#ifndef RAND_MAX
#define RAND_MAX 2147483647
#endif

/* Memory */
void *malloc(size_t size);
void *calloc(size_t nmemb, size_t size);
void *realloc(void *ptr, size_t size);
void  free(void *ptr);

/* Process */
void exit(int status);
void abort(void);
int atexit(void (*function)(void));
int system(const char *command);

/* Conversion */
int    atoi(const char *nptr);
long   atol(const char *nptr);
long   strtol(const char *nptr, char **endptr, int base);
unsigned long strtoul(const char *nptr, char **endptr, int base);
long long strtoll(const char *nptr, char **endptr, int base);
unsigned long long strtoull(const char *nptr, char **endptr, int base);

/* Math */
int abs(int x);
int rand(void);
void srand(unsigned seed);
double atof(const char *nptr);
float strtof(const char *nptr, char **endptr);

/* Sorting */
void qsort(void *base, size_t nmemb, size_t size,
            int (*compar)(const void *, const void *));
void *bsearch(const void *key, const void *base, size_t nmemb, size_t size,
              int (*compar)(const void *, const void *));

#endif /* _STDLIB_H */
