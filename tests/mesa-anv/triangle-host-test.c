#define _DEFAULT_SOURCE
#include "native-triangle-probe.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "completed-image-host.h"
#ifdef CUBIT_TEST_SCENE
#define CUBIT_SCENE_HOSTED 1
#include "native-scene-consumer.h"
static unsigned scene_record_failures;
static unsigned scene_map_countdown,scene_map_arm;
#endif
static unsigned validation_errors, validation_warnings;
static unsigned consumed;
static bool reject_completed;
static unsigned hidden_dispatch;
/* Negative control: prove the validation gate observes the newly required
 * SAMPLED usage, rather than accepting any completed readback as a texture. */
static VkResult VKAPI_CALL source_create_image(VkDevice device,
   const VkImageCreateInfo *info,const VkAllocationCallbacks *allocator,VkImage *image)
{
   VkImageCreateInfo copy=*info;
   if(getenv("CUBIT_TEST_STRIP_SAMPLED"))copy.usage&=~VK_IMAGE_USAGE_SAMPLED_BIT;
   return vkCreateImage(device,&copy,allocator,image);
}
#ifdef CUBIT_TEST_SCENE
static VkResult VKAPI_CALL source_map_memory(VkDevice device,VkDeviceMemory memory,
   VkDeviceSize offset,VkDeviceSize size,VkMemoryMapFlags flags,void **data)
{
   if(scene_map_countdown && --scene_map_countdown==0)return VK_ERROR_MEMORY_MAP_FAILED;
   return vkMapMemory(device,memory,offset,size,flags,data);
}
#endif
static PFN_vkVoidFunction VKAPI_CALL source_device_proc(VkDevice device,const char *name)
{
   if(!strcmp(name,"vkCreateImage"))return (PFN_vkVoidFunction)source_create_image;
#ifdef CUBIT_TEST_SCENE
   if(!strcmp(name,"vkMapMemory"))return (PFN_vkVoidFunction)source_map_memory;
#endif
   return vkGetDeviceProcAddr(device,name);
}
static PFN_vkVoidFunction VKAPI_CALL source_instance_proc(VkInstance instance,const char *name)
{
   if(!strcmp(name,"vkGetDeviceProcAddr"))return (PFN_vkVoidFunction)source_device_proc;
   return vkGetInstanceProcAddr(instance,name);
}
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
   if (bytes!=16384 || width!=64 || height!=64 || pitch!=256)
      return VK_ERROR_UNKNOWN;
   void *mapping=NULL;
   /* Validation catches an overlapping mapping if the producer forgot to
    * unmap. This is a Linux test consumer, not a Desktop export or extra copy. */
   VkResult result=vkMapMemory(device,memory,0,bytes,0,&mapping);
   if (result!=VK_SUCCESS) return result;
   const uint8_t *pixels=mapping;
   for (uint32_t row=0; row<height; row++) {
      for (uint32_t column=0; column<width; column++) {
         const int32_t x=2*(int32_t)column+1, y=2*(int32_t)row+1;
         const int inside=y>16 && 2*x-y>16 && 2*x+y<240;
         int fill=0;
#if defined(CUBIT_TEST_COMPOSITOR) || defined(CUBIT_TEST_SCENE)
         fill=column>=4 && column<12 && row>=4 && row<12;
#endif
         const uint8_t *p=pixels+row*pitch+4*column;
         if (p[0]!=(fill || inside ? 0 : 255) || p[1]!=(fill ? 255 : 0) ||
             p[2]!=(!fill && inside ? 255 : 0) || p[3]!=255)
            result=VK_ERROR_UNKNOWN;
      }
   }
   vkUnmapMemory(device,memory);
   printf("HOST ONLY completed-buffer consumer result=%d\n",result);
   if (result==VK_SUCCESS) {
      consumed++;
      /* A consumer can decline a valid frame after releasing its mapping.
       * The producer must clean up that frame and remain usable afterward. */
      if (reject_completed) return VK_ERROR_UNKNOWN;
   }
   return result;
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
   fflush(stdout);
}
#ifdef CUBIT_TEST_SCENE
static VkResult composed_completed_image(const struct mesa_completed_image *s,mesa_completed_pixels present)
{
   ++image_borrows;
   scene_map_countdown=scene_map_arm;scene_map_arm=0;
   return mesa_scene_compose(s,present,log_message);
}
#endif
int main(int argc, char **argv)
{
   const int negative=argc==2 && !strcmp(argv[1],"--negative-control");
   if (argc!=1 && !negative) return 9;
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
   const VkResult missing_result=mesa_triangle_probe(instance,physical,device,
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
#ifdef CUBIT_TEST_SCENE
   const int inject_cancel=getenv("CUBIT_SCENE_FAIL_RECORD")!=NULL;
   const char *map_fault=getenv("CUBIT_SCENE_FAIL_MAP");
   if(inject_cancel){
      scene_record_failures=1;
      result=mesa_triangle_probe_with_source(instance,physical,device,source_instance_proc,
         log_message,consume_completed,composed_completed_image);
      if(result!=VK_ERROR_UNKNOWN||scene_record_failures||consumed)return 9;
      printf("HOST ONLY scene record cancelled cleanly; testing reopen next\n");
   }
   if(map_fault){
      if(strcmp(map_fault,"1")&&strcmp(map_fault,"2"))return 9;
      scene_map_arm=(unsigned)(map_fault[0]-'0');
      result=mesa_triangle_probe_with_source(instance,physical,device,source_instance_proc,
         log_message,consume_completed,composed_completed_image);
      if(result!=VK_ERROR_MEMORY_MAP_FAILED||scene_map_countdown||consumed)return 9;
      printf("HOST ONLY scene mapping failure retired cleanly; testing reopen next\n");
   }
#endif
   const unsigned cycles=8;
   for (unsigned cycle=0; cycle<cycles; cycle++) {
      reject_completed=(cycle%2)==0;
      const VkResult expected=reject_completed ? VK_ERROR_UNKNOWN : VK_SUCCESS;
#ifdef CUBIT_TEST_SCENE
      result=mesa_triangle_probe_with_source(instance,physical,device,source_instance_proc,
                                 log_message,consume_completed,composed_completed_image);
#else
      result=mesa_triangle_probe_with_source(instance,physical,device,source_instance_proc,
                                 log_message,consume_completed,
                                 cycle%4<2 ? borrow_completed_image : NULL);
#endif
      printf("HOST ONLY cycle=%u result=%d expected=%d consumed=%u\n",
             cycle,result,expected,consumed);
      if (result!=expected || consumed!=cycle+1) {
         result=VK_ERROR_UNKNOWN;
         break;
      }
      result=VK_SUCCESS;
   }
   vkDestroyDevice(device,NULL);
   destroy_debug(instance,messenger,NULL);
   vkDestroyInstance(instance,NULL);
   printf("HOST ONLY offscreen triangle result=%d\n",result);
#ifdef CUBIT_TEST_SCENE
   const unsigned expected_borrows=8+(unsigned)inject_cancel+(unsigned)(map_fault!=NULL);
#else
   const unsigned expected_borrows=4;
#endif
   printf("HOST ONLY completed image borrows=%u expected=%u\n",image_borrows,expected_borrows);
   printf("VULKAN VALIDATION errors=%u warnings=%u (synchronization enabled)\n",
          validation_errors,validation_warnings);
   return result==VK_SUCCESS && consumed==cycles && image_borrows==expected_borrows && !validation_errors && !validation_warnings ? 0 : 7;
}
