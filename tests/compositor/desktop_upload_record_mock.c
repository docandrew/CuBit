#include <stdint.h>
#include <assert.h>
static uint32_t result,calls;
/* Last recorded region and the total rows recorded since the last set. */
static uint32_t last_y,last_height,last_discard,last_width,rows;
void desktop_upload_record_set(uint32_t value){result=value;calls=0;rows=0;}
uint32_t desktop_upload_record_calls(void){return calls;}
uint32_t desktop_upload_record_last_y(void){return last_y;}
uint32_t desktop_upload_record_last_height(void){return last_height;}
uint32_t desktop_upload_record_last_width(void){return last_width;}
uint32_t desktop_upload_record_last_discard(void){return last_discard;}
uint32_t desktop_upload_record_rows(void){return rows;}
uint32_t cubit_vulkan_upload_record(void *c,void *u,void *s,const uint32_t *r)
{assert(c&&u&&s&&r&&r[4]>0&&r[5]>0);++calls;
 last_y=r[3];last_width=r[4];last_height=r[5];last_discard=r[9];rows+=r[5];return result;}
