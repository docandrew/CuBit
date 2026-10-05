#define _DEFAULT_SOURCE
#ifdef CUBIT_TEAPOT_GALLERY
#include <time.h>
static int clock_fault;
static unsigned clock_calls;
static int gallery_test_clock(clockid_t clock,struct timespec *value)
{
   if(clock_fault && ++clock_calls%3==0)return -1;
   return clock_gettime(clock,value);
}
#define clock_gettime gallery_test_clock
#endif
#include "render.h"
#ifdef CUBIT_TEAPOT_GALLERY
#undef clock_gettime
#endif
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
static unsigned validation_errors, validation_warnings;
#include "../mesa-anv/completed-image-host.h"
#ifdef CUBIT_TEST_SCENE
#define CUBIT_SCENE_HOSTED 1
#include "../mesa-anv/native-scene-consumer.h"
#endif
static unsigned consumed;
static bool reject_completed;
static bool disable_depth;
static unsigned producer_pipelines, producer_recordings, producer_submissions;
static VkResult VKAPI_CALL counted_pipeline(VkDevice device,VkPipelineCache cache,
    uint32_t count,const VkGraphicsPipelineCreateInfo *infos,
    const VkAllocationCallbacks *allocator,VkPipeline *pipelines);
static VkResult VKAPI_CALL counted_begin(VkCommandBuffer command,const VkCommandBufferBeginInfo *info)
{
   ++producer_recordings;
   return vkBeginCommandBuffer(command,info);
}
static VkResult VKAPI_CALL counted_submit(VkQueue queue,uint32_t count,const VkSubmitInfo *info,VkFence fence)
{
   ++producer_submissions;
   return vkQueueSubmit(queue,count,info,fence);
}
/* Hosted negative control only: intercept the real Vulkan pipeline creation,
 * leaving mesh, shaders, camera and command recording unchanged. */
static VkResult VKAPI_CALL depthless_pipeline(VkDevice device,VkPipelineCache cache,
    uint32_t count,const VkGraphicsPipelineCreateInfo *infos,
    const VkAllocationCallbacks *allocator,VkPipeline *pipelines)
{
   if(count!=1 || !infos || !infos[0].pDepthStencilState)
      return VK_ERROR_INITIALIZATION_FAILED;
   VkGraphicsPipelineCreateInfo info=infos[0];
   VkPipelineDepthStencilStateCreateInfo depth=*info.pDepthStencilState;
   depth.depthTestEnable=VK_FALSE;
   depth.depthWriteEnable=VK_FALSE;
   info.pDepthStencilState=&depth;
   return vkCreateGraphicsPipelines(device,cache,1,&info,allocator,pipelines);
}
static VkResult VKAPI_CALL counted_pipeline(VkDevice device,VkPipelineCache cache,
    uint32_t count,const VkGraphicsPipelineCreateInfo *infos,
    const VkAllocationCallbacks *allocator,VkPipeline *pipelines)
{
   ++producer_pipelines;
   return disable_depth ? depthless_pipeline(device,cache,count,infos,allocator,pipelines)
                        : vkCreateGraphicsPipelines(device,cache,count,infos,allocator,pipelines);
}
static PFN_vkVoidFunction VKAPI_CALL probe_device_proc(VkDevice device,const char *name)
{
   if(!strcmp(name,"vkCreateGraphicsPipelines"))return (PFN_vkVoidFunction)counted_pipeline;
   if(!strcmp(name,"vkBeginCommandBuffer"))return (PFN_vkVoidFunction)counted_begin;
   if(!strcmp(name,"vkQueueSubmit"))return (PFN_vkVoidFunction)counted_submit;
   return vkGetDeviceProcAddr(device,name);
}
static PFN_vkVoidFunction VKAPI_CALL probe_instance_proc(VkInstance instance,const char *name)
{
   if(!strcmp(name,"vkGetDeviceProcAddr"))return (PFN_vkVoidFunction)probe_device_proc;
   return vkGetInstanceProcAddr(instance,name);
}
static unsigned hidden_dispatch;
static PFN_vkVoidFunction VKAPI_CALL
missing_device_proc(VkDevice device, const char *name)
{
   if (!strcmp(name, "vkCmdCopyImageToBuffer")) {
      ++hidden_dispatch;
      return NULL;
   }
   return vkGetDeviceProcAddr(device, name);
}
static PFN_vkVoidFunction VKAPI_CALL
missing_instance_proc(VkInstance instance, const char *name)
{
   if (!strcmp(name, "vkGetDeviceProcAddr"))
      return (PFN_vkVoidFunction)missing_device_proc;
   return vkGetInstanceProcAddr(instance, name);
}
static VkResult consume_completed(VkDevice device, VkDeviceMemory memory,
                                 VkDeviceSize bytes, uint32_t width,
                                 uint32_t height, uint32_t pitch)
{
#ifdef CUBIT_TEAPOT_GALLERY
   if (bytes!=800*600*4 || width!=800 || height!=600 || pitch!=3200)
#else
   if (bytes!=256*256*4 || width!=256 || height!=256 || pitch!=1024)
#endif
      return VK_ERROR_UNKNOWN;
   void *mapping=NULL;
   VkResult result=vkMapMemory(device,memory,0,bytes,0,&mapping);
   if (result!=VK_SUCCESS) return result;
   const uint8_t *pixels=mapping;
   unsigned foreground=0, background=0, bad=0;
#ifdef CUBIT_TEAPOT_GALLERY
   unsigned cells[20]={0};
   uint64_t hashes[20];
   static uint64_t previous_hashes[20];
   for(unsigned i=0;i<20;i++)hashes[i]=1469598103934665603ull;
#endif
   const char *path=getenv("TEAPOT_PPM");
   FILE *file=path && consumed==0 ? fopen(path,"wb") : NULL;
   if (file) fprintf(file,"P6\n%u %u\n255\n",width,height);
   for(unsigned y=0;y<height;y++)for(unsigned x=0;x<width;x++){
      const uint8_t *p=pixels+y*pitch+4*x;
      if(p[3]!=255)bad++;
#ifdef CUBIT_TEST_SCENE
      if(x>=4&&x<12&&y>=4&&y<12){if(p[0]!=0||p[1]!=255||p[2]!=0)bad++;}
      else
#endif
#ifdef CUBIT_TEAPOT_GALLERY
      if(p[0]>=14 && p[0]<=16 && p[1]>=7 && p[1]<=9 && p[2]>=4 && p[2]<=6)background++;
      else {foreground++;cells[(y/150)*5+x/160]++;}
      const unsigned cell=(y/150)*5+x/160;
      for(unsigned k=0;k<4;k++)hashes[cell]=(hashes[cell]^p[k])*1099511628211ull;
#else
      if(p[2]>p[1]*2 && p[1]>p[0]*2)foreground++;
      else if(p[0]>=14 && p[0]<=16 && p[1]>=7 && p[1]<=9 && p[2]>=4 && p[2]<=6)background++;
      else bad++;
#endif
      if(file){const uint8_t rgb[3]={p[2],p[1],p[0]};if(fwrite(rgb,1,3,file)!=3)bad++;}
   }
   if(file && fclose(file))bad++;
#ifdef CUBIT_TEAPOT_GALLERY
   for(unsigned i=0;i<20;i++)if(cells[i]<150){
      printf("GALLERY CHECK missing-cell=%u\n",i);bad++;
   }
   /* Accepted cycles must visibly change after the first frame; rejected
    * cycles and their successor restart at deterministic animation time0. */
   for(unsigned i=0;i<20;i++){
      if(!reject_completed && consumed%(1+CUBIT_TEAPOT_FRAME_COUNT)>1 &&
         hashes[i]==previous_hashes[i]){
         printf("GALLERY CHECK frozen-cell=%u\n",i);bad++;
      }
      previous_hashes[i]=hashes[i];
   }
#endif
   vkUnmapMemory(device,memory);
   printf("HOST ONLY teapot foreground=%u background=%u bad=%u\n",foreground,background,bad);
   if(foreground<6000 || background<20000 || bad)return VK_ERROR_UNKNOWN;
   consumed++;
   return reject_completed ? VK_ERROR_UNKNOWN : VK_SUCCESS;
}

static VKAPI_ATTR VkBool32 VKAPI_CALL validation(
   VkDebugUtilsMessageSeverityFlagBitsEXT severity,
   VkDebugUtilsMessageTypeFlagsEXT type,
   const VkDebugUtilsMessengerCallbackDataEXT *data, void *context)
{
   (void)type; (void)context;
   if (severity & VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT) validation_errors++;
   if (severity & VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT) validation_warnings++;
   fprintf(stderr,"VULKAN VALIDATION: %s\n",data->pMessage);
   return VK_FALSE;
}
static void log_message(const char *format, ...)
{
   va_list args; va_start(args,format); vprintf(format,args); va_end(args);
}
#ifdef CUBIT_TEST_SCENE
static VkResult composed_completed_image(const struct mesa_completed_image *s,mesa_completed_pixels present)
{
   ++image_borrows;
   return mesa_scene_compose(s,present,log_message);
}
#endif
int main(int argc, char **argv)
{
   disable_depth=argc==2 && !strcmp(argv[1],"--depth-negative");
   const int negative=argc==2 && !strcmp(argv[1],"--negative-control");
#ifdef CUBIT_TEAPOT_GALLERY
   clock_fault=argc==2 && !strcmp(argv[1],"--clock-negative");
   if (argc!=1 && !negative && !disable_depth && !clock_fault) return 9;
#else
   if (argc!=1 && !negative && !disable_depth) return 9;
#endif
   VkInstance instance=VK_NULL_HANDLE;
   const char *layers[]={"VK_LAYER_KHRONOS_validation"};
   const char *extensions[]={VK_EXT_DEBUG_UTILS_EXTENSION_NAME,
                             VK_EXT_VALIDATION_FEATURES_EXTENSION_NAME};
   const VkValidationFeatureEnableEXT enabled[]={
      VK_VALIDATION_FEATURE_ENABLE_SYNCHRONIZATION_VALIDATION_EXT};
   const VkValidationFeaturesEXT features={.sType=VK_STRUCTURE_TYPE_VALIDATION_FEATURES_EXT,
      .enabledValidationFeatureCount=1,.pEnabledValidationFeatures=enabled};
   VkDebugUtilsMessengerCreateInfoEXT debug={
      .sType=VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT,.pNext=&features,
      .messageSeverity=VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT|
                       VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT,
      .messageType=VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT|
                   VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT|
                   VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT,
      .pfnUserCallback=validation};
   const VkApplicationInfo application={.sType=VK_STRUCTURE_TYPE_APPLICATION_INFO,
#ifdef CUBIT_TEST_SCENE
      .apiVersion=VK_API_VERSION_1_1
#else
      .apiVersion=VK_API_VERSION_1_0
#endif
   };
   const VkInstanceCreateInfo info={.sType=VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO,.pApplicationInfo=&application,
      .pNext=&debug,.enabledLayerCount=1,.ppEnabledLayerNames=layers,
      .enabledExtensionCount=2,.ppEnabledExtensionNames=extensions};
   if (vkCreateInstance(&info,NULL,&instance)!=VK_SUCCESS) return 1;
   PFN_vkCreateDebugUtilsMessengerEXT create_debug=(PFN_vkCreateDebugUtilsMessengerEXT)
      vkGetInstanceProcAddr(instance,"vkCreateDebugUtilsMessengerEXT");
   PFN_vkDestroyDebugUtilsMessengerEXT destroy_debug=(PFN_vkDestroyDebugUtilsMessengerEXT)
      vkGetInstanceProcAddr(instance,"vkDestroyDebugUtilsMessengerEXT");
   VkDebugUtilsMessengerEXT messenger=VK_NULL_HANDLE;
   debug.pNext=NULL;
   if (!create_debug || !destroy_debug ||
       create_debug(instance,&debug,NULL,&messenger)!=VK_SUCCESS) return 8;
   uint32_t count=1;
   VkPhysicalDevice physical=VK_NULL_HANDLE;
   if (vkEnumeratePhysicalDevices(instance,&count,&physical)!=VK_SUCCESS || count!=1) return 2;
   VkPhysicalDeviceProperties props;
   vkGetPhysicalDeviceProperties(physical,&props);
   printf("HOST ONLY Vulkan device: %s\n",props.deviceName);
   if (props.deviceType!=VK_PHYSICAL_DEVICE_TYPE_CPU) return 3;
   uint32_t families=0;
   vkGetPhysicalDeviceQueueFamilyProperties(physical,&families,NULL);
   VkQueueFamilyProperties *queues=calloc(families,sizeof(*queues));
   if (!queues || !families) return 4;
   vkGetPhysicalDeviceQueueFamilyProperties(physical,&families,queues);
   if (!(queues[0].queueFlags & VK_QUEUE_GRAPHICS_BIT)) return 5;
   free(queues);
   const float priority=1;
   const VkDeviceQueueCreateInfo queue={.sType=VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO,
      .queueFamilyIndex=0,.queueCount=1,.pQueuePriorities=&priority};
   const VkDeviceCreateInfo create={.sType=VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO,
      .queueCreateInfoCount=1,.pQueueCreateInfos=&queue};
   VkDevice device=VK_NULL_HANDLE;
   if (vkCreateDevice(physical,&create,NULL,&device)!=VK_SUCCESS) return 6;
   const VkResult missing_result=mesa_teapot_probe(instance,physical,device,
      missing_instance_proc,log_message,consume_completed);
   if (missing_result!=VK_ERROR_INITIALIZATION_FAILED ||
       hidden_dispatch!=1 || consumed!=0) return 9;
   printf("HOST ONLY missing readback dispatch rejected before consumption\n");
   if (negative) {
      /* Linux CPU ICD only: an invalid zero-size buffer must be diagnosed.
       * This block is never compiled into the CuBit/native probe. */
      const VkBufferCreateInfo bad={.sType=VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,
         .size=0,.usage=VK_BUFFER_USAGE_TRANSFER_DST_BIT,
         .sharingMode=VK_SHARING_MODE_EXCLUSIVE};
      VkBuffer bad_buffer=VK_NULL_HANDLE;
      VkResult bad_result=vkCreateBuffer(device,&bad,NULL,&bad_buffer);
      if (bad_result==VK_SUCCESS && bad_buffer) vkDestroyBuffer(device,bad_buffer,NULL);
   }
   VkResult result=VK_SUCCESS;
   const unsigned cycles=8;
   unsigned expected_consumed=0;
   for (unsigned cycle=0; cycle<cycles; cycle++) {
      reject_completed=(cycle%2)==0;
      const VkResult expected=reject_completed ? VK_ERROR_UNKNOWN : VK_SUCCESS;
      result=mesa_teapot_probe_with_source(instance,physical,device,probe_instance_proc,
                                 log_message,consume_completed,
#ifdef CUBIT_TEST_SCENE
                                 composed_completed_image);
#else
                                 cycle%4<2 ? borrow_completed_image : NULL);
#endif
      printf("HOST ONLY cycle=%u result=%d expected=%d consumed=%u\n",
             cycle,result,expected,consumed);
      expected_consumed+=reject_completed ? 1 : CUBIT_TEAPOT_FRAME_COUNT;
      if (result!=expected || consumed!=expected_consumed) {
         result=VK_ERROR_UNKNOWN;
         break;
      }
      result=VK_SUCCESS;
   }
   vkDestroyDevice(device,NULL);
   destroy_debug(instance,messenger,NULL);
   vkDestroyInstance(instance,NULL);
   printf("HOST ONLY offscreen teapot result=%d\n",result);
   printf("VULKAN VALIDATION errors=%u warnings=%u (synchronization enabled)\n",
          validation_errors,validation_warnings);
#ifdef CUBIT_TEST_SCENE
   const unsigned expected_borrows=4*(1+CUBIT_TEAPOT_FRAME_COUNT);
#else
   const unsigned expected_borrows=2*(1+CUBIT_TEAPOT_FRAME_COUNT);
#endif
   printf("HOST ONLY completed image borrows=%u expected=%u\n",image_borrows,expected_borrows);
   printf("HOST ONLY producer pipelines=%u recordings=%u submissions=%u expected=%u\n",
          producer_pipelines,producer_recordings,producer_submissions,expected_consumed);
   return result==VK_SUCCESS && consumed==expected_consumed &&
      producer_pipelines==cycles && producer_recordings==
#ifdef CUBIT_TEAPOT_GALLERY
         expected_consumed &&
#else
         cycles &&
#endif
      producer_submissions==expected_consumed && image_borrows==expected_borrows &&
      !validation_errors && !validation_warnings ? 0 : 7;
}
