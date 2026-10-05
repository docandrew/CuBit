static unsigned guard_calls, allocation_step, fail_step, live_layouts, live_shaders, live_pipelines;
static const char *hide_dispatch;
static void VKAPI_CALL guard_bind(VkCommandBuffer c,VkPipelineBindPoint p,VkPipeline h)
{(void)c;(void)p;(void)h;++guard_calls;}
static void VKAPI_CALL guard_view(VkCommandBuffer c,uint32_t a,uint32_t b,const VkViewport *p)
{(void)c;(void)a;(void)b;(void)p;++guard_calls;}
static void VKAPI_CALL guard_clip(VkCommandBuffer c,uint32_t a,uint32_t b,const VkRect2D *p)
{(void)c;(void)a;(void)b;(void)p;++guard_calls;}
static void VKAPI_CALL guard_constants(VkCommandBuffer c,VkPipelineLayout l,VkShaderStageFlags s,uint32_t o,uint32_t z,const void *p)
{(void)c;(void)l;(void)s;(void)o;(void)z;(void)p;++guard_calls;}
static void VKAPI_CALL guard_draw(VkCommandBuffer c,uint32_t a,uint32_t b,uint32_t d,uint32_t e)
{(void)c;(void)a;(void)b;(void)d;(void)e;++guard_calls;}
static VkResult VKAPI_CALL fault_layout(VkDevice d,const VkPipelineLayoutCreateInfo *i,const VkAllocationCallbacks *a,VkPipelineLayout *p)
{
    if(++allocation_step==fail_step){*p=VK_NULL_HANDLE;return VK_ERROR_OUT_OF_DEVICE_MEMORY;}
    VkResult r=vkCreatePipelineLayout(d,i,a,p);if(r==VK_SUCCESS)++live_layouts;return r;
}
static VkResult VKAPI_CALL fault_shader(VkDevice d,const VkShaderModuleCreateInfo *i,const VkAllocationCallbacks *a,VkShaderModule *p)
{
    if(++allocation_step==fail_step){*p=VK_NULL_HANDLE;return VK_ERROR_OUT_OF_DEVICE_MEMORY;}
    VkResult r=vkCreateShaderModule(d,i,a,p);if(r==VK_SUCCESS)++live_shaders;return r;
}
static VkResult VKAPI_CALL fault_pipeline(VkDevice d,VkPipelineCache c,uint32_t n,const VkGraphicsPipelineCreateInfo *i,const VkAllocationCallbacks *a,VkPipeline *p)
{
    if(++allocation_step==fail_step){*p=VK_NULL_HANDLE;return VK_ERROR_OUT_OF_DEVICE_MEMORY;}
    VkResult r=vkCreateGraphicsPipelines(d,c,n,i,a,p);if(r==VK_SUCCESS)++live_pipelines;
    /* Driver may publish a handle before an aggregate creation error. */
    return fail_step==5&&r==VK_SUCCESS?VK_ERROR_OUT_OF_DEVICE_MEMORY:r;
}
static void VKAPI_CALL free_layout(VkDevice d,VkPipelineLayout p,const VkAllocationCallbacks *a)
{assert(live_layouts);--live_layouts;vkDestroyPipelineLayout(d,p,a);}
static void VKAPI_CALL free_shader(VkDevice d,VkShaderModule p,const VkAllocationCallbacks *a)
{assert(live_shaders);--live_shaders;vkDestroyShaderModule(d,p,a);}
static void VKAPI_CALL free_pipeline(VkDevice d,VkPipeline p,const VkAllocationCallbacks *a)
{assert(live_pipelines);--live_pipelines;vkDestroyPipeline(d,p,a);}
static PFN_vkVoidFunction VKAPI_CALL fault_proc(VkDevice d,const char *name)
{
    if(hide_dispatch&&!strcmp(name,hide_dispatch))return NULL;
#define MAP(n,f) if(!strcmp(name,"vk" #n))return (PFN_vkVoidFunction)f
    MAP(CreatePipelineLayout,fault_layout);MAP(CreateShaderModule,fault_shader);MAP(CreateGraphicsPipelines,fault_pipeline);
    MAP(DestroyPipelineLayout,free_layout);MAP(DestroyShaderModule,free_shader);MAP(DestroyPipeline,free_pipeline);
#undef MAP
    return vkGetDeviceProcAddr(d,name);
}
static int checker_guards(VkDevice device,VkRenderPass pass,const struct cubit_vulkan_checker *valid)
{
    const char *names[]={"vkDestroyPipelineLayout","vkDestroyPipeline","vkCmdBindPipeline","vkCmdSetViewport",
        "vkCmdSetScissor","vkCmdPushConstants","vkCmdDraw","vkCreatePipelineLayout","vkCreateShaderModule",
        "vkDestroyShaderModule","vkCreateGraphicsPipelines"};
    for(unsigned j=0;j<sizeof names/sizeof *names;j++){
        struct cubit_vulkan_checker e;hide_dispatch=names[j];allocation_step=0;
        CHECK(cubit_vulkan_checker_create(&e,device,fault_proc,pass)==VK_ERROR_INITIALIZATION_FAILED);
        CHECK(!e.layout&&!e.pipeline&&allocation_step==0);cubit_vulkan_checker_destroy(&e);
    }
    hide_dispatch=NULL;
    for(fail_step=1;fail_step<=5;fail_step++){
        struct cubit_vulkan_checker e;allocation_step=0;
        CHECK(cubit_vulkan_checker_create(&e,device,fault_proc,pass)==VK_ERROR_OUT_OF_DEVICE_MEMORY);
        CHECK(!e.layout&&!e.pipeline&&!live_layouts&&!live_shaders&&!live_pipelines);
        cubit_vulkan_checker_destroy(&e);
    }
    fail_step=0;
    for(unsigned fault=0;fault<=40;fault++){
        struct cubit_vulkan_checker e=*valid;
        e.bind=guard_bind;e.viewport=guard_view;e.scissor=guard_clip;e.constants=guard_constants;e.draw=guard_draw;
        struct cubit_vulkan_checker_request r={.left=-2,.top=1,.right=9,.bottom=8,.numerator=5,.denominator=4,
            .width=W,.height=H,.clip_w=W,.clip_h=H};
        const struct cubit_vulkan_checker *ep=&e;const struct cubit_vulkan_checker_request *rp=&r;
        VkCommandBuffer command=(VkCommandBuffer)(uintptr_t)1;guard_calls=0;
        switch(fault){
        case 0:ep=NULL;break;case 1:rp=NULL;break;case 2:command=VK_NULL_HANDLE;break;
        case 3:e.layout=VK_NULL_HANDLE;break;case 4:e.pipeline=VK_NULL_HANDLE;break;
        case 5:e.bind=NULL;break;case 6:e.viewport=NULL;break;case 7:e.scissor=NULL;break;
        case 8:e.constants=NULL;break;case 9:e.draw=NULL;break;
        case 10:r.width=0;break;case 11:r.width=65536;break;case 12:r.height=0;break;case 13:r.height=65536;break;
        case 14:r.rotation=4;break;case 15:r.numerator=0;break;case 16:r.numerator=17;break;
        case 17:r.denominator=0;break;case 18:r.denominator=17;break;
        case 19:r.left=-1073741825;break;case 20:r.top=-1073741825;break;
        case 21:r.right=1073741825;break;case 22:r.bottom=1073741825;break;
        case 23:r.left=r.right;break;case 24:r.top=r.bottom;break;
        case 25:r.origin_x=-16777217;break;case 26:r.origin_x=16777217;break;
        case 27:r.origin_y=-16777217;break;case 28:r.origin_y=16777217;break;
        case 29:r.clip_x=W;break;case 30:r.clip_y=H;break;case 31:r.clip_w=0;break;case 32:r.clip_h=0;break;
        case 33:r.clip_w=W+1;break;case 34:r.clip_h=H+1;break;
        case 35:r.clip_x=UINT32_MAX;break;case 36:r.clip_y=UINT32_MAX;break;
        case 37:r.clip_w=UINT32_MAX;break;case 38:r.clip_h=UINT32_MAX;break;
        case 39:r.left=INT32_MAX;r.right=INT32_MIN;break;case 40:break;
        }
        CHECK(cubit_vulkan_checker_record(ep,command,rp)==(fault==40?0:1));
        CHECK(guard_calls==(fault==40?5:0));
    }
    printf("PASS checker guards: 40 malformed requests, 11 missing dispatch, 5 partial creation failures, positive recording control\n");
    return 0;
}
