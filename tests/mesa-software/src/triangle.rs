//! Offscreen rasterization check, no window system or display driver involved.
use ash::{vk, Device, Instance};

pub unsafe fn run(
    instance: &Instance,
    physical: vk::PhysicalDevice,
    device: &Device,
    queue: vk::Queue,
    pool: vk::CommandPool,
    cmd: vk::CommandBuffer,
    fence: vk::Fence,
    buffer: vk::Buffer,
    mapped: *mut std::ffi::c_void,
    shader_dir: &std::path::Path,
) {
    let format = vk::Format::R8G8B8A8_UNORM;
    let image = device
        .create_image(
            &vk::ImageCreateInfo::default()
                .image_type(vk::ImageType::TYPE_2D)
                .format(format)
                .extent(vk::Extent3D {
                    width: 32,
                    height: 32,
                    depth: 1,
                })
                .mip_levels(1)
                .array_layers(1)
                .samples(vk::SampleCountFlags::TYPE_1)
                .tiling(vk::ImageTiling::OPTIMAL)
                .usage(vk::ImageUsageFlags::COLOR_ATTACHMENT | vk::ImageUsageFlags::TRANSFER_SRC)
                .sharing_mode(vk::SharingMode::EXCLUSIVE),
            None,
        )
        .unwrap();
    let req = device.get_image_memory_requirements(image);
    let props = instance.get_physical_device_memory_properties(physical);
    let index = (0..props.memory_type_count)
        .find(|&i| req.memory_type_bits & (1 << i) != 0)
        .unwrap();
    let memory = device
        .allocate_memory(
            &vk::MemoryAllocateInfo::default()
                .allocation_size(req.size)
                .memory_type_index(index),
            None,
        )
        .unwrap();
    device.bind_image_memory(image, memory, 0).unwrap();
    let range = vk::ImageSubresourceRange::default()
        .aspect_mask(vk::ImageAspectFlags::COLOR)
        .level_count(1)
        .layer_count(1);
    let view = device
        .create_image_view(
            &vk::ImageViewCreateInfo::default()
                .image(image)
                .view_type(vk::ImageViewType::TYPE_2D)
                .format(format)
                .subresource_range(range),
            None,
        )
        .unwrap();
    let attachments = [vk::AttachmentDescription::default()
        .format(format)
        .samples(vk::SampleCountFlags::TYPE_1)
        .load_op(vk::AttachmentLoadOp::CLEAR)
        .store_op(vk::AttachmentStoreOp::STORE)
        .initial_layout(vk::ImageLayout::UNDEFINED)
        .final_layout(vk::ImageLayout::TRANSFER_SRC_OPTIMAL)];
    let colors = [vk::AttachmentReference::default()
        .attachment(0)
        .layout(vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL)];
    let subpasses = [vk::SubpassDescription::default()
        .pipeline_bind_point(vk::PipelineBindPoint::GRAPHICS)
        .color_attachments(&colors)];
    let dependencies = [vk::SubpassDependency::default()
        .src_subpass(0)
        .dst_subpass(vk::SUBPASS_EXTERNAL)
        .src_stage_mask(vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT)
        .dst_stage_mask(vk::PipelineStageFlags::TRANSFER)
        .src_access_mask(vk::AccessFlags::COLOR_ATTACHMENT_WRITE)
        .dst_access_mask(vk::AccessFlags::TRANSFER_READ)];
    let pass = device
        .create_render_pass(
            &vk::RenderPassCreateInfo::default()
                .attachments(&attachments)
                .subpasses(&subpasses)
                .dependencies(&dependencies),
            None,
        )
        .unwrap();
    let views = [view];
    let framebuffer = device
        .create_framebuffer(
            &vk::FramebufferCreateInfo::default()
                .render_pass(pass)
                .attachments(&views)
                .width(32)
                .height(32)
                .layers(1),
            None,
        )
        .unwrap();
    let module = |name: &str| {
        let bytes = std::fs::read(shader_dir.join(name)).unwrap();
        let code = ash::util::read_spv(&mut std::io::Cursor::new(bytes)).unwrap();
        device
            .create_shader_module(&vk::ShaderModuleCreateInfo::default().code(&code), None)
            .unwrap()
    };
    let vertex = module("triangle.vert.spv");
    let fragment = module("triangle.frag.spv");
    let stages = [
        vk::PipelineShaderStageCreateInfo::default()
            .stage(vk::ShaderStageFlags::VERTEX)
            .module(vertex)
            .name(c"main"),
        vk::PipelineShaderStageCreateInfo::default()
            .stage(vk::ShaderStageFlags::FRAGMENT)
            .module(fragment)
            .name(c"main"),
    ];
    let layout = device
        .create_pipeline_layout(&vk::PipelineLayoutCreateInfo::default(), None)
        .unwrap();
    let vertex_input = vk::PipelineVertexInputStateCreateInfo::default();
    let assembly = vk::PipelineInputAssemblyStateCreateInfo::default()
        .topology(vk::PrimitiveTopology::TRIANGLE_LIST);
    let viewport = [vk::Viewport::default()
        .width(32.0)
        .height(32.0)
        .max_depth(1.0)];
    let rect = vk::Rect2D {
        offset: vk::Offset2D { x: 0, y: 0 },
        extent: vk::Extent2D {
            width: 32,
            height: 32,
        },
    };
    let scissors = [rect];
    let viewport_state = vk::PipelineViewportStateCreateInfo::default()
        .viewports(&viewport)
        .scissors(&scissors);
    let raster = vk::PipelineRasterizationStateCreateInfo::default()
        .polygon_mode(vk::PolygonMode::FILL)
        .cull_mode(vk::CullModeFlags::NONE)
        .front_face(vk::FrontFace::COUNTER_CLOCKWISE)
        .line_width(1.0);
    let multisample = vk::PipelineMultisampleStateCreateInfo::default()
        .rasterization_samples(vk::SampleCountFlags::TYPE_1);
    let blend_attachments = [vk::PipelineColorBlendAttachmentState::default()
        .color_write_mask(vk::ColorComponentFlags::RGBA)];
    let blend = vk::PipelineColorBlendStateCreateInfo::default().attachments(&blend_attachments);
    let pipeline = device
        .create_graphics_pipelines(
            vk::PipelineCache::null(),
            &[vk::GraphicsPipelineCreateInfo::default()
                .stages(&stages)
                .vertex_input_state(&vertex_input)
                .input_assembly_state(&assembly)
                .viewport_state(&viewport_state)
                .rasterization_state(&raster)
                .multisample_state(&multisample)
                .color_blend_state(&blend)
                .layout(layout)
                .render_pass(pass)],
            None,
        )
        .expect("graphics pipeline")[0];
    device
        .reset_command_pool(pool, vk::CommandPoolResetFlags::empty())
        .unwrap();
    device.reset_fences(&[fence]).unwrap();
    device
        .begin_command_buffer(cmd, &vk::CommandBufferBeginInfo::default())
        .unwrap();
    // Order the preceding compute writes before the transfer overwrites.
    let before = [vk::MemoryBarrier::default()
        .src_access_mask(vk::AccessFlags::SHADER_WRITE)
        .dst_access_mask(vk::AccessFlags::TRANSFER_WRITE)];
    device.cmd_pipeline_barrier(
        cmd,
        vk::PipelineStageFlags::COMPUTE_SHADER,
        vk::PipelineStageFlags::TRANSFER,
        vk::DependencyFlags::empty(),
        &before,
        &[],
        &[],
    );
    let clear = [vk::ClearValue {
        color: vk::ClearColorValue {
            float32: [0.0, 0.0, 0.0, 1.0],
        },
    }];
    device.cmd_begin_render_pass(
        cmd,
        &vk::RenderPassBeginInfo::default()
            .render_pass(pass)
            .framebuffer(framebuffer)
            .render_area(rect)
            .clear_values(&clear),
        vk::SubpassContents::INLINE,
    );
    device.cmd_bind_pipeline(cmd, vk::PipelineBindPoint::GRAPHICS, pipeline);
    device.cmd_draw(cmd, 3, 1, 0, 0);
    device.cmd_end_render_pass(cmd);
    let copy = [vk::BufferImageCopy::default()
        .image_subresource(
            vk::ImageSubresourceLayers::default()
                .aspect_mask(vk::ImageAspectFlags::COLOR)
                .layer_count(1),
        )
        .image_extent(vk::Extent3D {
            width: 32,
            height: 32,
            depth: 1,
        })];
    device.cmd_copy_image_to_buffer(
        cmd,
        image,
        vk::ImageLayout::TRANSFER_SRC_OPTIMAL,
        buffer,
        &copy,
    );
    let after = [vk::MemoryBarrier::default()
        .src_access_mask(vk::AccessFlags::TRANSFER_WRITE)
        .dst_access_mask(vk::AccessFlags::HOST_READ)];
    device.cmd_pipeline_barrier(
        cmd,
        vk::PipelineStageFlags::TRANSFER,
        vk::PipelineStageFlags::HOST,
        vk::DependencyFlags::empty(),
        &after,
        &[],
        &[],
    );
    device.end_command_buffer(cmd).unwrap();
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
        .expect("bounded draw completion");
    let pixels = std::slice::from_raw_parts(mapped.cast::<u8>(), 4096);
    let mut checked = 0;
    for y in 0..32 {
        for x in 0..32 {
            // Exclude diagonal samples exactly on the shared edge; check all others.
            if x + y == 31 {
                continue;
            }
            let expected = if x + y < 31 {
                [255, 0, 0, 255]
            } else {
                [0, 0, 0, 255]
            };
            assert_eq!(
                &pixels[(y * 32 + x) * 4..(y * 32 + x + 1) * 4],
                &expected,
                "triangle pixel at {x},{y}"
            );
            checked += 1;
        }
    }
    assert_eq!(checked, 992);
    device.destroy_pipeline(pipeline, None);
    device.destroy_pipeline_layout(layout, None);
    device.destroy_shader_module(vertex, None);
    device.destroy_shader_module(fragment, None);
    device.destroy_framebuffer(framebuffer, None);
    device.destroy_render_pass(pass, None);
    device.destroy_image_view(view, None);
    device.destroy_image(image, None);
    device.free_memory(memory, None);
    println!("PASS: offscreen software triangle, 992 exact interior/exterior RGBA samples");
}
