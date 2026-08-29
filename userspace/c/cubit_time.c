/*
 * CuBit OS - Time Syscall Wrappers
 * Copyright (C) 2026 Jon Andrew
 */
#include "cubit.h"
#include <time.h>

uint64_t cubit_gettime_ms(void)
{
    return (uint64_t)syscall0(SYSCALL_GETTIME);
}

void cubit_sleep_ms(uint32_t ms)
{
    syscall1(SYSCALL_SLEEP, ms);
}

time_t time(time_t *result)
{
    time_t seconds = (time_t)(cubit_gettime_ms() / 1000U);

    if (result != NULL)
        *result = seconds;

    return seconds;
}

static int leap_year(long year)
{
    return year % 4 == 0 && (year % 100 != 0 || year % 400 == 0);
}

struct tm *gmtime(const time_t *timer)
{
    static struct tm result;
    static const int month_days[12] =
        { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
    long seconds;
    long days;
    long year = 1970;
    int month;

    if (timer == NULL || *timer < 0)
        return NULL;
    seconds = *timer;
    days = seconds / 86400L;
    result.tm_sec = (int)(seconds % 60L);
    result.tm_min = (int)((seconds / 60L) % 60L);
    result.tm_hour = (int)((seconds / 3600L) % 24L);
    result.tm_wday = (int)((days + 4L) % 7L);

    while (days >= (leap_year(year) ? 366L : 365L)) {
        days -= leap_year(year) ? 366L : 365L;
        year++;
    }
    result.tm_year = (int)(year - 1900L);
    result.tm_yday = (int)days;
    for (month = 0; month < 12; month++) {
        int length = month_days[month];
        if (month == 1 && leap_year(year)) length++;
        if (days < length) break;
        days -= length;
    }
    result.tm_mon = month;
    result.tm_mday = (int)days + 1;
    result.tm_isdst = 0;
    return &result;
}
