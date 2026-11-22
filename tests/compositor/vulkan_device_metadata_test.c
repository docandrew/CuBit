#include "vulkan_device_storage.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
static uint32_t count = 3, mode;
static PFN_vkVoidFunction VKAPI_CALL device_proc(VkDevice d, const char *name)
{ (void)d; (void)name; return NULL; }
static void VKAPI_CALL memory_properties(VkPhysicalDevice d, VkPhysicalDeviceMemoryProperties *p)
{
    (void)d; memset(p, 0, sizeof(*p)); p->memoryTypeCount=count;
    p->memoryTypes[0].propertyFlags=VK_MEMORY_PROPERTY_PROTECTED_BIT;
    p->memoryTypes[1].propertyFlags=VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT;
    p->memoryTypes[2].propertyFlags=VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT;
}
static PFN_vkVoidFunction VKAPI_CALL instance_proc(VkInstance i, const char *name)
{
    (void)i;
    if (!strcmp(name,"vkGetDeviceProcAddr"))
        return mode==1 ? NULL : (PFN_vkVoidFunction)device_proc;
    if (!strcmp(name,"vkGetPhysicalDeviceMemoryProperties"))
        return mode==2 ? NULL : (PFN_vkVoidFunction)memory_properties;
    assert(0); return NULL;
}
static void rejected(uint32_t w,uint32_t h)
{
    struct cubit_vulkan_device_targets out;
    memset(&out,0xFF,sizeof(out));
    assert(cubit_vulkan_device_targets_prepare(w,h,&out)==1);
    assert(!out.description&&!out.images[0]&&!out.images[1]&&!out.images[2]&&!out.allowed_types);
}
int main(void)
{
    assert(cubit_vulkan_device_targets_prepare(32,24,NULL)==1);
    rejected(32,24);
    struct cubit_mesa_service_device v={.instance=(VkInstance)(uintptr_t)1,
        .physical=(VkPhysicalDevice)(uintptr_t)2,.device=(VkDevice)(uintptr_t)3,
        .queue=(VkQueue)(uintptr_t)4,.instance_proc=instance_proc};
    struct cubit_vulkan_context_request *c=cubit_vulkan_device_context_request(&v);
    assert(c && !cubit_vulkan_device_context_request(&v));
    rejected(32,24);
    /* Simulate the audited context boundary's successful publication only. */
    c->fresh->live=1;c->fresh->device=v.device;c->fresh->pass=(VkRenderPass)(uintptr_t)5;
    rejected(0,24);rejected(32,0);rejected(16385,24);rejected(32,16385);
    mode=1;rejected(32,24);mode=2;rejected(32,24);mode=0;
    count=0;rejected(32,24);count=33;rejected(32,24);count=2;rejected(32,24);count=3;
    struct cubit_vulkan_device_targets out;
    assert(!cubit_vulkan_device_targets_prepare(32,24,&out));
    assert(out.allowed_types==4);
    struct cubit_vulkan_target_request *t=out.description;
    assert(t&&t->fresh&&t->device==v.device&&t->pass==c->fresh->pass&&t->proc==device_proc);
    assert(t->width==32&&t->height==24);
    for(unsigned n=0;n<3;n++) {
        struct cubit_vulkan_owned_image *a=out.images[n];
        assert(a&&a->physical==v.physical&&a->device==v.device&&a->proc==device_proc);
        assert(a->width==32&&a->height==24&&a->format==VK_FORMAT_B8G8R8A8_UNORM);
        assert(a->usage==(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT|VK_IMAGE_USAGE_TRANSFER_SRC_BIT));
        assert(!a->image&&!a->memory&&!a->stage&&!t->images[n]);
        for(unsigned m=0;m<n;m++)assert(out.images[m]!=a);
    }
    struct cubit_vulkan_target_request saved=*t;
    rejected(64,48);assert(!memcmp(t,&saved,sizeof(saved)));
    c->fresh->live=0;rejected(32,24);
    puts("PASS private device target metadata, mask filtering, invalid boundaries, one-shot preservation");
}
