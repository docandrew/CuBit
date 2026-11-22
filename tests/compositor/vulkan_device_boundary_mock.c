#include <stdint.h>
static uint32_t owned,description,retirement,starts,closes,health,checks;
static unsigned context_request;
void device_mock_set(uint32_t a,uint32_t b,uint32_t c)
{owned=a;description=b;retirement=c;starts=closes=health=checks=0;}
uint32_t device_mock_starts(void){return starts;}
uint32_t device_mock_closes(void){return closes;}
void device_mock_start(uint64_t slot,uint32_t *out,void **request)
{(void)slot;starts++;*out=owned;*request=description?&context_request:0;}
uint32_t device_mock_close(void){closes++;return retirement;}

void device_mock_health_set(uint32_t value){health=value;}
uint32_t device_mock_health_checks(void){return checks;}
uint32_t device_mock_health(void){checks++;return health;}
