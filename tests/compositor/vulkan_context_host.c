#include "vulkan_context.h"
#include <stdio.h>
#include <stdlib.h>
#define CHECK(x) do {if(!(x)){fprintf(stderr,"context host failure line %d: %s\n",__LINE__,#x);exit(1);}}while(0)
static unsigned errors;
static VKAPI_ATTR VkBool32 VKAPI_CALL report(VkDebugUtilsMessageSeverityFlagBitsEXT severity,
 VkDebugUtilsMessageTypeFlagsEXT type,const VkDebugUtilsMessengerCallbackDataEXT *data,void *user)
{(void)type;(void)user;if(severity&VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT){errors++;fprintf(stderr,"%s\n",data->pMessage);}return VK_FALSE;}
int main(void)
{
 const char *layer="VK_LAYER_KHRONOS_validation",*extension=VK_EXT_DEBUG_UTILS_EXTENSION_NAME;
 const VkDebugUtilsMessengerCreateInfoEXT debug={.sType=VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT,
  .messageSeverity=VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT,
  .messageType=VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT|VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT|VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT,.pfnUserCallback=report};
 const VkApplicationInfo app={.sType=VK_STRUCTURE_TYPE_APPLICATION_INFO,.apiVersion=VK_API_VERSION_1_1};
 const VkInstanceCreateInfo ici={.sType=VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO,.pNext=&debug,.pApplicationInfo=&app,
  .enabledLayerCount=1,.ppEnabledLayerNames=&layer,.enabledExtensionCount=1,.ppEnabledExtensionNames=&extension};
 VkInstance instance;CHECK(vkCreateInstance(&ici,NULL,&instance)==VK_SUCCESS);
 PFN_vkCreateDebugUtilsMessengerEXT create_debug=(PFN_vkCreateDebugUtilsMessengerEXT)vkGetInstanceProcAddr(instance,"vkCreateDebugUtilsMessengerEXT");
 PFN_vkDestroyDebugUtilsMessengerEXT destroy_debug=(PFN_vkDestroyDebugUtilsMessengerEXT)vkGetInstanceProcAddr(instance,"vkDestroyDebugUtilsMessengerEXT");
 CHECK(create_debug&&destroy_debug);VkDebugUtilsMessengerEXT messenger;CHECK(create_debug(instance,&debug,NULL,&messenger)==VK_SUCCESS);
 uint32_t count=1;VkPhysicalDevice physical;CHECK(vkEnumeratePhysicalDevices(instance,&count,&physical)==VK_SUCCESS&&count==1);
 VkQueueFamilyProperties families[32];count=32;vkGetPhysicalDeviceQueueFamilyProperties(physical,&count,families);
 uint32_t family;for(family=0;family<count;family++)if(families[family].queueFlags&VK_QUEUE_GRAPHICS_BIT)break;CHECK(family<count);
 const float priority=1;const VkDeviceQueueCreateInfo qi={.sType=VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO,.queueFamilyIndex=family,.queueCount=1,.pQueuePriorities=&priority};
 const VkDeviceCreateInfo di={.sType=VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO,.queueCreateInfoCount=1,.pQueueCreateInfos=&qi};
 VkDevice device;CHECK(vkCreateDevice(physical,&di,NULL,&device)==VK_SUCCESS);VkQueue queue;vkGetDeviceQueue(device,family,0,&queue);
 const struct cubit_mesa_service_device view={instance,physical,device,queue,family,vkGetInstanceProcAddr};
 for(unsigned i=0;i<16;i++){
  struct cubit_vulkan_context context={0};struct cubit_vulkan_context_request request={&context,&view};void *submission=NULL;
  CHECK(cubit_vulkan_context_create(&request,&submission)==0&&submission==&context.submission&&context.pass);
  CHECK(context.submission.device==device&&context.submission.queue==queue);
  /* No submission or child image/view exists: immediate retirement is valid. */
  CHECK(cubit_vulkan_context_release(&request)==0);
  CHECK(cubit_vulkan_context_release(&request)==2);
 }
 vkDestroyDevice(device,NULL);destroy_debug(instance,messenger,NULL);vkDestroyInstance(instance,NULL);
 CHECK(!errors);puts("VULKAN CONTEXT: PASS 16 real hosted Mesa context lifecycles, zero validation errors; NOT CuBit GPU");
}
