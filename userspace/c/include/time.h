/*
 * CuBit OS - Minimal ISO C time interface
 * Copyright (C) 2026 Jon Andrew
 */
#ifndef CUBIT_TIME_H
#define CUBIT_TIME_H

#include <stddef.h>

typedef long time_t;

struct tm {
    int tm_sec;
    int tm_min;
    int tm_hour;
    int tm_mday;
    int tm_mon;
    int tm_year;
    int tm_wday;
    int tm_yday;
    int tm_isdst;
};

/*
 * During early bring-up this is seconds since boot, not civil/UTC time.
 * A future clock-service session will provide the wall-clock epoch.
 */
time_t time(time_t *result);
struct tm *gmtime(const time_t *timer);

#endif
