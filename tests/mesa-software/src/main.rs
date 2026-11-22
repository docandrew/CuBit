//! Linux-only Vulkan transfer/compute readback baseline; NOT a CuBit renderer.
//! Pass the freshly built lavapipe .so explicitly, never use a host GPU.
use ash::{vk, Entry};
mod triangle;
use std::{ffi::CStr, path::Path};

fn main() {
    let library = std::env::args()
        .nth(1)
        .expect("lavapipe library path required");
    assert!(Path::new(&library).is_file());
    let shader = std::env::args()
        .nth(2)
        .expect("compiled pattern.comp SPIR-V required");
    unsafe { run(&library, &shader) }
}

unsafe fn run(library: &str, shader_path: &str) {
    // ICDs export the ICD entrypoint, not necessarily the loader's symbol.
    // Keep the library alive until every Vulkan object has been destroyed.
    let library = libloading::Library::new(library).expect("load explicit lavapipe ICD");
    let get_proc = *library
        .get::<vk::PFN_vkGetInstanceProcAddr>(b"vk_icdGetInstanceProcAddr\0")
        .expect("ICD entrypoint");
    let entry = Entry::from_static_fn(ash::StaticFn {
        get_instance_proc_addr: get_proc,
    });
    let app = vk::ApplicationInfo::default().api_version(vk::API_VERSION_1_1);
    let info = vk::InstanceCreateInfo::default().application_info(&app);
    let instance = entry.create_instance(&info, None).expect("create instance");
    let physical = instance.enumerate_physical_devices().unwrap();
    assert_eq!(physical.len(), 1, "expected only explicit software device");
    let physical = physical[0];
    let props = instance.get_physical_device_properties(physical);
    let name = CStr::from_ptr(props.device_name.as_ptr()).to_str().unwrap();
    assert_eq!(props.device_type, vk::PhysicalDeviceType::CPU);
    assert!(name.contains("llvmpipe"), "unexpected renderer: {name}");
    println!("Linux-hosted software device: {name}");
    let families = instance.get_physical_device_queue_family_properties(physical);
    let family = families
        .iter()
        .position(|f| {
            f.queue_count > 0
                && f.queue_flags
                    .contains(vk::QueueFlags::GRAPHICS | vk::QueueFlags::COMPUTE)
        })
        .expect("graphics queue") as u32;
    let priorities = [1.0];
    let queues = [vk::DeviceQueueCreateInfo::default()
        .queue_family_index(family)
        .queue_priorities(&priorities)];
    let device = instance
        .create_device(
            physical,
            &vk::DeviceCreateInfo::default().queue_create_infos(&queues),
            None,
        )
        .unwrap();
    let queue = device.get_device_queue(family, 0);
    const BYTES: u64 = 4096;
    const PATTERN: u32 = 0xC0B17A5E;
    let buffer = device
        .create_buffer(
            &vk::BufferCreateInfo::default()
                .size(BYTES)
                .usage(vk::BufferUsageFlags::TRANSFER_DST | vk::BufferUsageFlags::STORAGE_BUFFER)
                .sharing_mode(vk::SharingMode::EXCLUSIVE),
            None,
        )
        .unwrap();
    let requirements = device.get_buffer_memory_requirements(buffer);
    let memory_props = instance.get_physical_device_memory_properties(physical);
    let memory_type = (0..memory_props.memory_type_count)
        .find(|&i| {
            requirements.memory_type_bits & (1 << i) != 0
                && memory_props.memory_types[i as usize]
                    .property_flags
                    .contains(
                        vk::MemoryPropertyFlags::HOST_VISIBLE
                            | vk::MemoryPropertyFlags::HOST_COHERENT,
                    )
        })
        .expect("host coherent memory");
    let memory = device
        .allocate_memory(
            &vk::MemoryAllocateInfo::default()
                .allocation_size(requirements.size)
                .memory_type_index(memory_type),
            None,
        )
        .unwrap();
    device.bind_buffer_memory(buffer, memory, 0).unwrap();
    let mapped = device
        .map_memory(memory, 0, BYTES, vk::MemoryMapFlags::empty())
        .unwrap();
    std::ptr::write_bytes(mapped.cast::<u8>(), 0, BYTES as usize);
    let pool = device
        .create_command_pool(
            &vk::CommandPoolCreateInfo::default().queue_family_index(family),
            None,
        )
        .unwrap();
    let cmd = device
        .allocate_command_buffers(
            &vk::CommandBufferAllocateInfo::default()
                .command_pool(pool)
                .level(vk::CommandBufferLevel::PRIMARY)
                .command_buffer_count(1),
        )
        .unwrap()[0];
    device
        .begin_command_buffer(cmd, &vk::CommandBufferBeginInfo::default())
        .unwrap();
    device.cmd_fill_buffer(cmd, buffer, 0, BYTES, PATTERN);
    let barriers = [vk::MemoryBarrier::default()
        .src_access_mask(vk::AccessFlags::TRANSFER_WRITE)
        .dst_access_mask(vk::AccessFlags::HOST_READ)];
    device.cmd_pipeline_barrier(
        cmd,
        vk::PipelineStageFlags::TRANSFER,
        vk::PipelineStageFlags::HOST,
        vk::DependencyFlags::empty(),
        &barriers,
        &[],
        &[],
    );
    device.end_command_buffer(cmd).unwrap();
    let fence = device
        .create_fence(&vk::FenceCreateInfo::default(), None)
        .unwrap();
    let commands = [cmd];
    device
        .queue_submit(
            queue,
            &[vk::SubmitInfo::default().command_buffers(&commands)],
            fence,
        )
        .unwrap();
    device
        .wait_for_fences(&[fence], true, 10_000_000_000)
        .expect("bounded completion");
    let words = std::slice::from_raw_parts(mapped.cast::<u32>(), BYTES as usize / 4);
    assert!(
        words.iter().all(|&word| word == PATTERN),
        "readback mismatch"
    );
    let bytes = std::fs::read(shader_path).expect("SPIR-V file");
    let code = ash::util::read_spv(&mut std::io::Cursor::new(bytes)).expect("SPIR-V words");
    let shader = device
        .create_shader_module(&vk::ShaderModuleCreateInfo::default().code(&code), None)
        .unwrap();
    let bindings = [vk::DescriptorSetLayoutBinding::default()
        .binding(0)
        .descriptor_type(vk::DescriptorType::STORAGE_BUFFER)
        .descriptor_count(1)
        .stage_flags(vk::ShaderStageFlags::COMPUTE)];
    let set_layout = device
        .create_descriptor_set_layout(
            &vk::DescriptorSetLayoutCreateInfo::default().bindings(&bindings),
            None,
        )
        .unwrap();
    let layouts = [set_layout];
    let layout = device
        .create_pipeline_layout(
            &vk::PipelineLayoutCreateInfo::default().set_layouts(&layouts),
            None,
        )
        .unwrap();
    let stage = vk::PipelineShaderStageCreateInfo::default()
        .module(shader)
        .name(c"main")
        .stage(vk::ShaderStageFlags::COMPUTE);
    let pipeline = device
        .create_compute_pipelines(
            vk::PipelineCache::null(),
            &[vk::ComputePipelineCreateInfo::default()
                .stage(stage)
                .layout(layout)],
            None,
        )
        .expect("Mesa compute compilation")[0];
    let sizes = [vk::DescriptorPoolSize::default()
        .ty(vk::DescriptorType::STORAGE_BUFFER)
        .descriptor_count(1)];
    let descriptor_pool = device
        .create_descriptor_pool(
            &vk::DescriptorPoolCreateInfo::default()
                .max_sets(1)
                .pool_sizes(&sizes),
            None,
        )
        .unwrap();
    let set = device
        .allocate_descriptor_sets(
            &vk::DescriptorSetAllocateInfo::default()
                .descriptor_pool(descriptor_pool)
                .set_layouts(&layouts),
        )
        .unwrap()[0];
    let buffers = [vk::DescriptorBufferInfo::default()
        .buffer(buffer)
        .offset(0)
        .range(BYTES)];
    device.update_descriptor_sets(
        &[vk::WriteDescriptorSet::default()
            .dst_set(set)
            .dst_binding(0)
            .descriptor_type(vk::DescriptorType::STORAGE_BUFFER)
            .buffer_info(&buffers)],
        &[],
    );
    device
        .reset_command_pool(pool, vk::CommandPoolResetFlags::empty())
        .unwrap();
    device.reset_fences(&[fence]).unwrap();
    device
        .begin_command_buffer(cmd, &vk::CommandBufferBeginInfo::default())
        .unwrap();
    let before = [vk::MemoryBarrier::default()
        .src_access_mask(vk::AccessFlags::TRANSFER_WRITE)
        .dst_access_mask(vk::AccessFlags::SHADER_WRITE)];
    device.cmd_pipeline_barrier(
        cmd,
        vk::PipelineStageFlags::TRANSFER,
        vk::PipelineStageFlags::COMPUTE_SHADER,
        vk::DependencyFlags::empty(),
        &before,
        &[],
        &[],
    );
    device.cmd_bind_pipeline(cmd, vk::PipelineBindPoint::COMPUTE, pipeline);
    device.cmd_bind_descriptor_sets(cmd, vk::PipelineBindPoint::COMPUTE, layout, 0, &[set], &[]);
    device.cmd_dispatch(cmd, 4, 4, 1);
    let after = [vk::MemoryBarrier::default()
        .src_access_mask(vk::AccessFlags::SHADER_WRITE)
        .dst_access_mask(vk::AccessFlags::HOST_READ)];
    device.cmd_pipeline_barrier(
        cmd,
        vk::PipelineStageFlags::COMPUTE_SHADER,
        vk::PipelineStageFlags::HOST,
        vk::DependencyFlags::empty(),
        &after,
        &[],
        &[],
    );
    device.end_command_buffer(cmd).unwrap();
    device
        .queue_submit(
            queue,
            &[vk::SubmitInfo::default().command_buffers(&commands)],
            fence,
        )
        .unwrap();
    device
        .wait_for_fences(&[fence], true, 10_000_000_000)
        .expect("bounded shader completion");
    let pixels = std::slice::from_raw_parts(mapped.cast::<u32>(), 1024);
    for y in 0..32u32 {
        for x in 0..32u32 {
            assert_eq!(
                pixels[(y * 32 + x) as usize],
                0xff000000 | ((x ^ y) << 16) | (y << 8) | x,
                "shader output at {x},{y}"
            );
        }
    }
    device.destroy_descriptor_pool(descriptor_pool, None);
    device.destroy_pipeline(pipeline, None);
    device.destroy_pipeline_layout(layout, None);
    device.destroy_descriptor_set_layout(set_layout, None);
    device.destroy_shader_module(shader, None);
    triangle::run(
        &instance,
        physical,
        &device,
        queue,
        pool,
        cmd,
        fence,
        buffer,
        mapped,
        Path::new(shader_path).parent().unwrap(),
    );
    device.unmap_memory(memory);
    device.destroy_fence(fence, None);
    device.destroy_command_pool(pool, None);
    device.destroy_buffer(buffer, None);
    device.free_memory(memory, None);
    device.destroy_device(None);
    instance.destroy_instance(None);
    println!("PASS: software Vulkan transfer + 1024 exact compute-generated pixels + triangle");
}
