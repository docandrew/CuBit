#define _POSIX_C_SOURCE 200809L   /* clock_gettime under -std=c11 */
#include "vulkan_submission.h"
#include "vulkan_affine.h"
#include <stdio.h>
#include <time.h>

/* Job accounting (see vulkan_submission.h). One recording at a time: the
 * kinds and amounts noted since the last start belong to the next submit.
 * In flight, per submission object: its kinds and submit time, until a
 * poll sees it done. Per kinds combination: counts, amounts and the
 * submit-to-seen time, since the last report. Single-threaded (Desktop's
 * loop), like every call in this file. */
#define JOB_KINDS 8u
#define JOB_FLIGHTS 8u
struct job_amounts {
    uint64_t upload_bytes,render_px,readback_bytes;
    uint64_t draws[CUBIT_VULKAN_DRAW_SOURCES],draw_px[CUBIT_VULKAN_DRAW_SOURCES];
};
static uint32_t recording_kinds;
static struct job_amounts recording;
static struct { const void *owner; uint32_t kinds; uint64_t submitted_ns; struct job_amounts amounts; } flights[JOB_FLIGHTS];
/* The slowest render job of the period, submit to seen, and what it drew. */
static struct { uint64_t seen_ns; uint32_t kinds; struct job_amounts amounts; } slowest_render;
static struct { uint64_t jobs,seen_total_ns,seen_max_ns; struct job_amounts amounts; } job_stats[JOB_KINDS];
/* The Mesa glue's tag for the next vkQueueSubmit (absent without it). */
extern void cubit_gpu_tag_next_submit(uint32_t kinds) __attribute__((weak));
static uint64_t job_now_ns(void)
{
    struct timespec t;
    return clock_gettime(CLOCK_MONOTONIC,&t)?0:(uint64_t)t.tv_sec*1000000000u+(uint64_t)t.tv_nsec;
}
void cubit_vulkan_job_note(enum cubit_vulkan_job_kind kind,uint64_t amount)
{
    recording_kinds|=(uint32_t)kind;
    if(kind==CUBIT_VULKAN_JOB_UPLOAD)recording.upload_bytes+=amount;
    else if(kind==CUBIT_VULKAN_JOB_RENDER)recording.render_px+=amount;
    else if(kind==CUBIT_VULKAN_JOB_READBACK)recording.readback_bytes+=amount;
}
void cubit_vulkan_job_note_draw(enum cubit_vulkan_draw_source source,uint64_t pixels)
{
    if(source>=CUBIT_VULKAN_DRAW_SOURCES)return;
    recording.draws[source]++;
    recording.draw_px[source]+=pixels;
}
static void job_submitted(const void *owner)
{
    const uint32_t kinds=recording_kinds%JOB_KINDS;
    job_stats[kinds].jobs++;
    job_stats[kinds].amounts.upload_bytes+=recording.upload_bytes;
    job_stats[kinds].amounts.render_px+=recording.render_px;
    job_stats[kinds].amounts.readback_bytes+=recording.readback_bytes;
    for(uint32_t d=0;d<CUBIT_VULKAN_DRAW_SOURCES;d++){
        job_stats[kinds].amounts.draws[d]+=recording.draws[d];
        job_stats[kinds].amounts.draw_px[d]+=recording.draw_px[d];
    }
    for(uint32_t i=0;i<JOB_FLIGHTS;i++)
        if(!flights[i].owner||flights[i].owner==owner){
            flights[i].owner=owner;flights[i].kinds=kinds;flights[i].submitted_ns=job_now_ns();
            flights[i].amounts=recording;break;
        }
    recording_kinds=0;recording=(struct job_amounts){0};
}
static void job_seen(const void *owner)
{
    const uint64_t now=job_now_ns();
    for(uint32_t i=0;i<JOB_FLIGHTS;i++)
        if(flights[i].owner==owner){
            if(now&&flights[i].submitted_ns&&now>=flights[i].submitted_ns){
                const uint64_t took=now-flights[i].submitted_ns;
                job_stats[flights[i].kinds].seen_total_ns+=took;
                if(took>job_stats[flights[i].kinds].seen_max_ns)job_stats[flights[i].kinds].seen_max_ns=took;
                if((flights[i].kinds&CUBIT_VULKAN_JOB_RENDER)&&took>slowest_render.seen_ns){
                    slowest_render.seen_ns=took;slowest_render.kinds=flights[i].kinds;
                    slowest_render.amounts=flights[i].amounts;
                }
            }
            flights[i].owner=NULL;break;
        }
}
uint32_t cubit_vulkan_job_report(char *buffer,uint32_t capacity)
{
    static const char *const names[JOB_KINDS]={"none","u","r","ur","b","ub","rb","urb"};
    uint32_t length=0;
    if(!buffer||!capacity)return 0;
    buffer[0]=0;
    for(uint32_t k=0;k<JOB_KINDS;k++){
        if(!job_stats[k].jobs)continue;
        uint64_t draws=0,draw_px=0;
        for(uint32_t d=0;d<CUBIT_VULKAN_DRAW_SOURCES;d++)
            draws+=job_stats[k].amounts.draws[d],draw_px+=job_stats[k].amounts.draw_px[d];
        const int wrote=snprintf(buffer+length,capacity-length,
            "%s%s=%llu up=%lluB px=%llu draws=%llu/%llupx back=%lluB seen_us=%llu/%llu",length?" ":"",names[k],
            (unsigned long long)job_stats[k].jobs,(unsigned long long)job_stats[k].amounts.upload_bytes,
            (unsigned long long)job_stats[k].amounts.render_px,(unsigned long long)draws,(unsigned long long)draw_px,
            (unsigned long long)job_stats[k].amounts.readback_bytes,
            (unsigned long long)(job_stats[k].seen_total_ns/job_stats[k].jobs/1000),
            (unsigned long long)(job_stats[k].seen_max_ns/1000));
        if(wrote<0||(uint32_t)wrote>=capacity-length){buffer[length]=0;break;}
        length+=(uint32_t)wrote;
    }
    if(slowest_render.seen_ns&&length<capacity){
        const struct job_amounts *a=&slowest_render.amounts;
        const int wrote=snprintf(buffer+length,capacity-length,
            "%sslowest_%s seen_us=%llu affine=%llu/%llupx backdrop=%llu/%llupx checker=%llu/%llupx fill=%llu/%llupx up=%lluB back=%lluB",
            length?" ":"",names[slowest_render.kinds%JOB_KINDS],(unsigned long long)(slowest_render.seen_ns/1000),
            (unsigned long long)a->draws[CUBIT_VULKAN_DRAW_AFFINE],(unsigned long long)a->draw_px[CUBIT_VULKAN_DRAW_AFFINE],
            (unsigned long long)a->draws[CUBIT_VULKAN_DRAW_BACKDROP],(unsigned long long)a->draw_px[CUBIT_VULKAN_DRAW_BACKDROP],
            (unsigned long long)a->draws[CUBIT_VULKAN_DRAW_CHECKER],(unsigned long long)a->draw_px[CUBIT_VULKAN_DRAW_CHECKER],
            (unsigned long long)a->draws[CUBIT_VULKAN_DRAW_FILL],(unsigned long long)a->draw_px[CUBIT_VULKAN_DRAW_FILL],
            (unsigned long long)a->upload_bytes,(unsigned long long)a->readback_bytes);
        if(wrote>0&&(uint32_t)wrote<capacity-length)length+=(uint32_t)wrote;
        else buffer[length]=0;
    }
    slowest_render.seen_ns=0;
    for(uint32_t k=0;k<JOB_KINDS;k++)job_stats[k].jobs=job_stats[k].seen_total_ns=job_stats[k].seen_max_ns=0,
        job_stats[k].amounts=(struct job_amounts){0};
    return length;
}
uint32_t cubit_vulkan_submission_matches(void *borrowed,void *draw)
{
    const struct cubit_vulkan_submission *s=borrowed;
    const struct cubit_vulkan_affine_draw *d=draw;
    return s&&d&&d->engine&&s->command&&s->device&&
        s->command==d->command&&s->device==d->engine->device?0:2;
}
uint32_t cubit_vulkan_submission_init(struct cubit_vulkan_submission *s,
    VkDevice d,VkQueue q,VkCommandBuffer c,VkFence f,PFN_vkGetDeviceProcAddr proc)
{
    if(!s)return 2;
    *s=(struct cubit_vulkan_submission){.device=d,.queue=q,.command=c,.fence=f};
    if(!d||!q||!c||!f||!proc)return 2;
#define LOAD(field,name) s->field=(PFN_vk##name)proc(d,"vk" #name);if(!s->field)return 2
    LOAD(reset_fences,ResetFences);LOAD(reset_command,ResetCommandBuffer);LOAD(begin,BeginCommandBuffer);
    LOAD(end,EndCommandBuffer);LOAD(submit,QueueSubmit);LOAD(status,GetFenceStatus);
    LOAD(begin_scene,CmdBeginRenderPass);LOAD(end_scene,CmdEndRenderPass);LOAD(fill,CmdClearAttachments);
#undef LOAD
    return 0;
}
uint32_t cubit_vulkan_submission_start(void *borrowed)
{
    const struct cubit_vulkan_submission *s=borrowed;
    if(!s||!s->device||!s->fence||!s->command||!s->reset_fences||!s->reset_command||!s->begin)return 2;
    if(s->reset_fences(s->device,1,&s->fence)!=VK_SUCCESS)return 2;
    if(s->reset_command(s->command,0)!=VK_SUCCESS)return 2;
    recording_kinds=0;recording=(struct job_amounts){0};
    const VkCommandBufferBeginInfo info={.sType=VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,.flags=VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT};
    return s->begin(s->command,&info)==VK_SUCCESS?0:2;
}
uint32_t cubit_vulkan_submission_seal(void *borrowed)
{
    const struct cubit_vulkan_submission *s=borrowed;
    return s&&s->command&&s->end&&s->end(s->command)==VK_SUCCESS?0:2;
}
uint32_t cubit_vulkan_submission_submit(void *borrowed)
{
    const struct cubit_vulkan_submission *s=borrowed;
    if(!s||!s->command||!s->queue||!s->fence||!s->submit)return 2;
    const VkSubmitInfo info={.sType=VK_STRUCTURE_TYPE_SUBMIT_INFO,.commandBufferCount=1,.pCommandBuffers=&s->command};
    if(cubit_gpu_tag_next_submit)cubit_gpu_tag_next_submit(recording_kinds);
    job_submitted(s);
    return s->submit(s->queue,1,&info,s->fence)==VK_SUCCESS?0:2;
}
uint32_t cubit_vulkan_submission_poll(void *borrowed)
{
    const struct cubit_vulkan_submission *s=borrowed;
    if(!s||!s->device||!s->fence||!s->status)return 2;
    VkResult result=s->status(s->device,s->fence);
    if(result==VK_SUCCESS)job_seen(s);
    return result==VK_SUCCESS?0:result==VK_NOT_READY?1:2;
}
uint32_t cubit_vulkan_submission_cancel(void *borrowed)
{
    const struct cubit_vulkan_submission *s=borrowed;
    return s&&s->command&&s->reset_command&&s->reset_command(s->command,0)==VK_SUCCESS?0:2;
}

uint32_t cubit_vulkan_submission_begin_scene(void *borrowed,void *pass,uint32_t width,uint32_t height)
{
    const struct cubit_vulkan_submission *s=borrowed;
    const struct cubit_vulkan_scene *p=pass;
    if(!s||!p||!s->device||s->device!=p->device||!s->command||!s->begin_scene||
       p->begin.sType!=VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO||p->begin.pNext||
       !p->begin.renderPass||!p->begin.framebuffer||!width||width>65535||!height||height>65535||
       p->begin.renderArea.offset.x||p->begin.renderArea.offset.y||
       p->begin.renderArea.extent.width!=width||p->begin.renderArea.extent.height!=height||
       p->begin.clearValueCount>1||(p->begin.clearValueCount&&!p->begin.pClearValues))return 2;
    s->begin_scene(s->command,&p->begin,VK_SUBPASS_CONTENTS_INLINE);
    cubit_vulkan_job_note(CUBIT_VULKAN_JOB_RENDER,(uint64_t)width*height);
    return 0;
}
uint32_t cubit_vulkan_submission_end_scene(void *borrowed)
{
    const struct cubit_vulkan_submission *s=borrowed;
    if(!s||!s->command||!s->end_scene)return 2;
    s->end_scene(s->command);return 0;
}

uint32_t cubit_vulkan_submission_fill(void *borrowed,uint32_t width,uint32_t height,
    uint32_t left,uint32_t top,uint32_t right,uint32_t bottom,uint32_t rgb)
{
    const struct cubit_vulkan_submission *s=borrowed;
    if(!s||!s->command||!s->fill||!width||width>65535||!height||height>65535||
       left>=right||top>=bottom||right>width||bottom>height)return 2;
    const VkClearAttachment clear={.aspectMask=VK_IMAGE_ASPECT_COLOR_BIT,.colorAttachment=0,
        .clearValue={.color={{((rgb>>16)&255)/255.0f,((rgb>>8)&255)/255.0f,(rgb&255)/255.0f,1}}}};
    const VkClearRect rect={.rect={{(int32_t)left,(int32_t)top},{right-left,bottom-top}},.baseArrayLayer=0,.layerCount=1};
    s->fill(s->command,1,&clear,1,&rect);
    cubit_vulkan_job_note_draw(CUBIT_VULKAN_DRAW_FILL,(uint64_t)(right-left)*(bottom-top));
    return 0;
}
