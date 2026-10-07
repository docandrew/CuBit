#include "vulkan_sources.h"
#include "vulkan_checker.h"
#include "vulkan_device_storage.h"
/* Same-process private metadata only. One device lifetime, no heap, no reset,
 * no authority acquisition. Copy the borrowed view so Ada stack storage can
 * expire; the actual device remains owned by Mesa_Service until policy closes
 * every consumer. Vulkan_Context_Owner owns all context creation/destruction. */
static struct cubit_vulkan_context context;
static struct cubit_mesa_service_device device;
static struct cubit_vulkan_context_request request = { &context, &device };
static unsigned prepared;
void *cubit_vulkan_device_context_request(const struct cubit_mesa_service_device *view)
{
    if (prepared || !view) return NULL;
    prepared = 1;
    device = *view;
    return &request;
}

static struct cubit_vulkan_owned_image images[3];
static struct cubit_vulkan_targets targets;
static struct cubit_vulkan_target_request target_request;
static unsigned targets_prepared;
static uint32_t target_allowed_types;
uint32_t cubit_vulkan_device_targets_prepare(uint32_t width, uint32_t height,
                                           struct cubit_vulkan_device_targets *out)
{
    if (!out) return 1;
    *out = (struct cubit_vulkan_device_targets){0};
    if (targets_prepared || !prepared || !context.live ||
        !width || !height || width > 16384 || height > 16384 ||
        !device.instance_proc || !device.instance || !device.physical ||
        !device.device || context.device != device.device || !context.pass) return 1;
    PFN_vkGetDeviceProcAddr proc = (PFN_vkGetDeviceProcAddr)
        device.instance_proc(device.instance, "vkGetDeviceProcAddr");
    PFN_vkGetPhysicalDeviceMemoryProperties properties =
        (PFN_vkGetPhysicalDeviceMemoryProperties)device.instance_proc(
            device.instance, "vkGetPhysicalDeviceMemoryProperties");
    if (!proc || !properties) return 1;
    VkPhysicalDeviceMemoryProperties memory = {0};
    properties(device.physical, &memory);
    if (!memory.memoryTypeCount || memory.memoryTypeCount > 32) return 1;
    uint32_t allowed = 0;
    for (uint32_t n = 0; n < memory.memoryTypeCount; ++n)
        if (!(memory.memoryTypes[n].propertyFlags &
              (VK_MEMORY_PROPERTY_PROTECTED_BIT | VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT)))
            allowed |= UINT32_C(1) << n;
    if (!allowed) return 1;
    targets_prepared = 1;
    for (unsigned n = 0; n < 3; ++n) {
        images[n] = (struct cubit_vulkan_owned_image){
            .physical=device.physical, .device=device.device,
            .instance_proc=device.instance_proc, .instance=device.instance,
            .proc=proc, .width=width, .height=height,
            .format=VK_FORMAT_B8G8R8A8_UNORM,
            .usage=VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT|VK_IMAGE_USAGE_TRANSFER_SRC_BIT};
        out->images[n] = &images[n];
    }
    target_request = (struct cubit_vulkan_target_request){
        .fresh=&targets, .device=device.device, .proc=proc, .pass=context.pass,
        .width=width, .height=height};
    out->description = &target_request;
    out->allowed_types = allowed;
    target_allowed_types = allowed;
    return 0;
}

/* Existing affine shader engine + descriptor provider, one process lifetime.
 * SPARK registers a context child before create and proves quiescence before
 * close. This adds no pixel allocation, command submission or new authority. */
static struct cubit_vulkan_affine_engine pipeline;
static struct cubit_vulkan_checker checker;
static struct cubit_vulkan_sources sources;
static unsigned pipeline_attempted, pipeline_live;
uint32_t cubit_vulkan_device_pipeline_create(void)
{
    if(pipeline_attempted)return 2;
    pipeline_attempted=1;
    if(!prepared||!context.live||!context.pass||context.device!=device.device||
       !device.instance_proc)return 1;
    PFN_vkGetDeviceProcAddr proc=(PFN_vkGetDeviceProcAddr)
        device.instance_proc(device.instance,"vkGetDeviceProcAddr");
    if(!proc)return 1;
    if(cubit_vulkan_affine_create(&pipeline,device.device,proc,context.pass)!=VK_SUCCESS)return 1;
    if(cubit_vulkan_checker_create(&checker,device.device,proc,context.pass)!=VK_SUCCESS){
        cubit_vulkan_affine_destroy(&pipeline);return 1;
    }
    if(cubit_vulkan_sources_init(&sources,&pipeline,context.submission.command,proc)!=VK_SUCCESS){
        cubit_vulkan_checker_destroy(&checker);
        cubit_vulkan_affine_destroy(&pipeline);return 1;
    }
    pipeline_live=1;return 0;
}
uint32_t cubit_vulkan_device_pipeline_close(void)
{
    if(!pipeline_live)return 2;
    if(cubit_vulkan_sources_destroy(&sources)!=0)return 2;
    cubit_vulkan_checker_destroy(&checker);
    cubit_vulkan_affine_destroy(&pipeline);pipeline_live=0;return 0;
}

/* SPARK owns active-pass ordering and command quiescence. Authenticate the
 * exact private submission and immutable target extent before recording. */
uint32_t cubit_vulkan_device_checker_record(void *borrowed,const struct cubit_vulkan_checker_request *r)
{
    if(!r||!prepared||!pipeline_live||!context.live||!targets_prepared||
       borrowed!=&context.submission||context.device!=device.device||
       checker.device!=device.device||pipeline.device!=device.device||
       r->width!=target_request.width||r->height!=target_request.height)return 1;
    return cubit_vulkan_checker_record(&checker,context.submission.command,r);
}

static struct cubit_vulkan_source_request source_requests[CUBIT_VULKAN_SOURCE_CAPACITY];
/* Only private owned-image metadata is accepted, not untrusted client memory.
 * No image/view allocation or descriptor update occurs here. The source owner
 * retains its image/backing and shader-readable layout through GPU completion. */
void *cubit_vulkan_device_source_request(uint32_t slot,void *owned_image)
{
    const struct cubit_vulkan_owned_image *s=owned_image;
    if(!pipeline_live||!context.live||!targets_prepared||!sources.pool||
       slot>=CUBIT_VULKAN_SOURCE_CAPACITY||sources.entries[slot].view||!s||
       s->stage!=2||!s->image||!s->memory||s->device!=device.device||
       s->physical!=device.physical||s->instance!=device.instance||
       s->proc!=target_request.proc||s->instance_proc!=device.instance_proc||
       !s->width||s->width>65535||!s->height||s->height>65535||
       (s->format!=VK_FORMAT_B8G8R8A8_UNORM&&s->format!=VK_FORMAT_R8_UNORM)||
       s->usage!=(VK_IMAGE_USAGE_SAMPLED_BIT|VK_IMAGE_USAGE_TRANSFER_DST_BIT))return NULL;
    for(unsigned n=0;n<3;n++)
        if(images[n].stage!=2||!targets.views[n]||!targets.framebuffers[n]||
           s==&images[n]||s->image==images[n].image||s->memory==images[n].memory)return NULL;
    source_requests[slot]=(struct cubit_vulkan_source_request){
        .provider=&sources,.slot=slot,.image=s->image,.format=s->format,
        .output_width=target_request.width,.output_height=target_request.height};
    return &source_requests[slot];
}

static struct cubit_vulkan_owned_image owned_sources[CUBIT_VULKAN_OWNED_SOURCE_CAPACITY];
uint32_t cubit_vulkan_device_source_prepare(uint32_t slot,uint32_t width,
    uint32_t height,uint32_t mask,struct cubit_vulkan_device_source *out)
{
    if(!out)return 1;
    *out=(struct cubit_vulkan_device_source){0};
    if(slot>=CUBIT_VULKAN_OWNED_SOURCE_CAPACITY||!width||width>65535||
       !height||height>65535||mask>1||!pipeline_live||!context.live||
       !targets_prepared||!target_allowed_types||!sources.pool||
       context.device!=device.device||!target_request.proc)return 1;
    struct cubit_vulkan_owned_image *s=&owned_sources[slot];
    if(sources.entries[slot].view||s->image||s->memory||
       (s->stage!=0&&s->stage!=3))return 1;
    for(unsigned n=0;n<3;n++)
        if(images[n].stage!=2||!images[n].image||!images[n].memory||
           !targets.views[n]||!targets.framebuffers[n])return 1;
    *s=(struct cubit_vulkan_owned_image){
        .physical=device.physical,.device=device.device,.instance=device.instance,
        .instance_proc=device.instance_proc,.proc=target_request.proc,
        .width=width,.height=height,.format=mask?VK_FORMAT_R8_UNORM:VK_FORMAT_B8G8R8A8_UNORM,
        .usage=VK_IMAGE_USAGE_SAMPLED_BIT|VK_IMAGE_USAGE_TRANSFER_DST_BIT};
    out->image=s;out->allowed_types=target_allowed_types;
    return 0;
}

static struct cubit_vulkan_upload_buffer upload;
static struct cubit_vulkan_upload_buffer readback;
static void *prepare_staging(struct cubit_vulkan_upload_buffer *staging)
{
    if(!prepared||!context.live||!pipeline_live||!targets_prepared||
       !device.instance||!device.physical||!device.device||!device.instance_proc||
       context.device!=device.device||!target_request.proc||
       staging->buffer||staging->memory||staging->mapped||
       (staging->stage!=0&&staging->stage!=3))return NULL;
    for(unsigned n=0;n<3;n++)
        if(images[n].stage!=2||!images[n].image||!images[n].memory||
           !targets.views[n]||!targets.framebuffers[n])return NULL;
    *staging=(struct cubit_vulkan_upload_buffer){
        .instance=device.instance,.physical=device.physical,.device=device.device,
        .instance_proc=device.instance_proc,.proc=target_request.proc};
    return staging;
}
void *cubit_vulkan_device_upload_prepare(void){return prepare_staging(&upload);}
void *cubit_vulkan_device_readback_prepare(void){return prepare_staging(&readback);}
