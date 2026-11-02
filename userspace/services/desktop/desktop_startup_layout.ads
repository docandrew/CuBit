with Interfaces;
-- Startup layout admission for the current BGRA image/readback backend.
-- Numeric validation grants no authority over a mapping or GPU allocation.
--
-- Sized from the actual display mode (LIM-001): a frame is Width * Height *
-- 4 bytes (a 3840x2160 frame is 33,177,600), and the renderer's aggregate
-- GPU budget (targets, upload staging, readback, sources) is Budget_Frames
-- frames, never less than Minimum_GPU_Budget, the budget every mode up to
-- 1920x1080 has always had. The only ceilings are representational: an
-- edge is a 16-bit display extent, and the budget is counted in Natural
-- bytes (GPU accounting, Vulkan_Image_Owner.Accounting). A mode past them
-- is a named CAPACITY refusal (Capacity_Limit, GPU_Budget_Demand).
package Desktop_Startup_Layout with SPARK_Mode, Pure is
   subtype Wide is Interfaces.Unsigned_64;
   use type Wide;
   Bytes_Per_Pixel : constant Wide := 4;
   -- Display extents (Compositor_Damage.Extent, Compositor_Upload.Edge).
   Maximum_Edge : constant Wide := 65535;
   Budget_Frames : constant Wide := 16;
   Minimum_GPU_Budget : constant Wide := 128 * 1024 * 1024;
   -- The aggregate budget is counted in Natural bytes.
   Capacity_Limit : constant Wide := Wide (Natural'Last);
   pragma Compile_Time_Error
     (Minimum_GPU_Budget > Capacity_Limit, "the minimum GPU budget is not countable");

   Maximum_Frame_Bytes : constant Wide := Maximum_Edge * Maximum_Edge * Bytes_Per_Pixel;
   function Frame_Bytes (Width, Height : Wide) return Wide
     with Pre => Width <= Maximum_Edge and Height <= Maximum_Edge,
          Post => Frame_Bytes'Result <= Maximum_Frame_Bytes
                  and Long_Long_Integer (Frame_Bytes'Result) =
                    Long_Long_Integer (Width) * Long_Long_Integer (Height)
                    * Long_Long_Integer (Bytes_Per_Pixel)
                  and (if Width >= 1 and Height >= 1 then Frame_Bytes'Result >= Bytes_Per_Pixel);
   -- What the mode's aggregate GPU budget would be, before the Natural
   -- ceiling (zero for an edge past Maximum_Edge: nothing to demand).
   function GPU_Budget_Demand (Width, Height : Wide) return Wide is
     (if Width > Maximum_Edge or else Height > Maximum_Edge then 0
      else Wide'Max (Minimum_GPU_Budget, Budget_Frames * Frame_Bytes (Width, Height)));

   function Supported (Width, Height : Wide) return Boolean is
     (Width in 1 .. Maximum_Edge and then Height in 1 .. Maximum_Edge
      and then GPU_Budget_Demand (Width, Height) <= Capacity_Limit);
   -- Zero is rejection. Multiplication only follows bounded-edge validation.
   function Required_Bytes (Width, Height : Wide) return Natural
     with Post =>
       (if Supported (Width, Height) then
          Required_Bytes'Result >= 1 and
          Wide (Required_Bytes'Result) = Frame_Bytes (Width, Height)
        else Required_Bytes'Result = 0);
   -- The renderer's aggregate GPU budget for the mode; zero is rejection.
   function GPU_Budget_Bytes (Width, Height : Wide) return Natural
     with Post =>
       (if Supported (Width, Height) then
          Wide (GPU_Budget_Bytes'Result) = GPU_Budget_Demand (Width, Height)
          and Wide (GPU_Budget_Bytes'Result) >= Minimum_GPU_Budget
        else GPU_Budget_Bytes'Result = 0);
end Desktop_Startup_Layout;
