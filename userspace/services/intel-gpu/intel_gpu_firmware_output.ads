with Interfaces;
with Intel_GPU_Display_Topology;
with Intel_GPU_Plane_Decode;
--  Which pipe scans out the firmware framebuffer that the kernel handed to
--  display.svc (SYSINFO FB width, height and pitch). Pure selection from
--  the boot inventory's plane-1 samples, no MMIO.
--
--  A pipe matches when its primary plane decodes as the supported linear
--  32-bit RGB scanout (Intel_GPU_Plane_Decode.Linear_Ready), starts at its
--  surface (PLANE_OFFSET zero) and has exactly the framebuffer's width,
--  height and pitch. Exactly one matching pipe selects it. None, or several
--  (a cloned firmware mode), select nothing: display keeps the firmware
--  framebuffer without driver planes, and the reason is logged.
--
--  Matching geometry is the evidence that the driver drives display.svc's
--  output; the GGTT surface is not compared with the framebuffer's
--  physical address, which the kernel does not publish.
package Intel_GPU_Firmware_Output with SPARK_Mode is
   use Interfaces;
   package Topology renames Intel_GPU_Display_Topology;
   use type Topology.Pipe;

   --  PLANE_SIZE holds 13-bit (dimension - 1) fields; linear PLANE_STRIDE
   --  counts 64-byte units in 12 bits.
   Size_Field_Mask   : constant Unsigned_32 := 16#1FFF#;
   Height_Shift      : constant := 16;
   Stride_Field_Mask : constant Unsigned_32 := 16#FFF#;
   Stride_Unit       : constant := 64;
   Bytes_Per_Pixel   : constant := 4;
   Extent_Limit      : constant := 8_192;
   Pitch_Limit       : constant := 4_095 * Stride_Unit;
   subtype Extent is Positive range 1 .. Extent_Limit;
   subtype Pitch_Bytes is Natural range 0 .. Pitch_Limit;

   type Geometry is record
      Width, Height : Extent := 1;
      Pitch : Pitch_Bytes := 0;
   end record;

   function Width_Of (Size : Unsigned_32) return Extent is
     (Extent ((Size and Size_Field_Mask) + 1));
   function Height_Of (Size : Unsigned_32) return Extent is
     (Extent ((Interfaces.Shift_Right (Size, Height_Shift) and Size_Field_Mask) + 1));
   function Pitch_Of (Stride : Unsigned_32) return Pitch_Bytes is
     (Pitch_Bytes ((Stride and Stride_Field_Mask) * Stride_Unit));
   function Shape (Sample : Intel_GPU_Plane_Decode.Sample) return Geometry is
     ((Width_Of (Sample.Size), Height_Of (Sample.Size), Pitch_Of (Sample.Stride)));

   --  Plane 1 of one pipe, as collected twice by the boot inventory.
   type Primary is record
      Collected : Boolean := False;
      Before, After : Intel_GPU_Plane_Decode.Sample;
   end record;
   type Primaries is array (Topology.Pipe) of Primary;

   function Matches
     (Item : Primary; Table_Bytes : Unsigned_64; Wanted : Geometry)
      return Boolean is
     (Item.Collected and then Item.Before.Offset = 0 and then
      Intel_GPU_Plane_Decode."="
        (Intel_GPU_Plane_Decode.Decode (Item.Before, Item.After, Table_Bytes).State,
         Intel_GPU_Plane_Decode.Linear_Ready) and then
      Shape (Item.Before) = Wanted);

   type Outcome is (Invalid_Framebuffer, No_Pipe, Several_Pipes, Selected);
   type Selection (State : Outcome := No_Pipe) is record
      case State is
         when Selected =>
            Pipe : Topology.Pipe;
            Shape : Geometry;
            --  The live GGTT surface (PLANE_SURFLIVE) and its control word.
            Surface : Unsigned_32;
            Control : Unsigned_32;
         when others => null;
      end case;
   end record;

   --  Framebuffer geometry as SYSINFO reports it (64-bit words).
   function Framebuffer_Valid (Width, Height, Pitch : Unsigned_64) return Boolean is
     (Width in 1 .. Extent_Limit and then Height in 1 .. Extent_Limit and then
      Pitch <= Pitch_Limit and then Pitch >= Width * Bytes_Per_Pixel);

   function Select_Output
     (Planes : Primaries; Table_Bytes, FB_Width, FB_Height, FB_Pitch : Unsigned_64)
      return Selection
     with Global => null,
          Post =>
            (case Select_Output'Result.State is
               when Invalid_Framebuffer =>
                 not Framebuffer_Valid (FB_Width, FB_Height, FB_Pitch),
               when No_Pipe =>
                 Framebuffer_Valid (FB_Width, FB_Height, FB_Pitch) and then
                 (for all P in Topology.Pipe =>
                    not Matches (Planes (P), Table_Bytes,
                                 (Extent (FB_Width), Extent (FB_Height),
                                  Pitch_Bytes (FB_Pitch)))),
               when Several_Pipes =>
                 Framebuffer_Valid (FB_Width, FB_Height, FB_Pitch) and then
                 (for some P in Topology.Pipe =>
                    Matches (Planes (P), Table_Bytes,
                             (Extent (FB_Width), Extent (FB_Height),
                              Pitch_Bytes (FB_Pitch))) and then
                    (for some Q in Topology.Pipe =>
                       P /= Q and then
                       Matches (Planes (Q), Table_Bytes,
                                (Extent (FB_Width), Extent (FB_Height),
                                 Pitch_Bytes (FB_Pitch))))),
               when Selected =>
                 Framebuffer_Valid (FB_Width, FB_Height, FB_Pitch) and then
                 Select_Output'Result.Shape =
                   (Extent (FB_Width), Extent (FB_Height), Pitch_Bytes (FB_Pitch)) and then
                 Matches (Planes (Select_Output'Result.Pipe), Table_Bytes,
                          Select_Output'Result.Shape) and then
                 Select_Output'Result.Surface =
                   Planes (Select_Output'Result.Pipe).Before.Live_Surface and then
                 Select_Output'Result.Control =
                   Planes (Select_Output'Result.Pipe).Before.Control and then
                 (for all P in Topology.Pipe =>
                    (if P /= Select_Output'Result.Pipe then
                       not Matches (Planes (P), Table_Bytes,
                                    Select_Output'Result.Shape))));

   function Name (Item : Outcome) return String is
     (case Item is
        when Invalid_Framebuffer => "INVALID-FRAMEBUFFER",
        when No_Pipe => "NO-MATCHING-PIPE",
        when Several_Pipes => "SEVERAL-PIPES",
        when Selected => "SELECTED");
   function Pipe_Name (Item : Topology.Pipe) return String is
     (case Item is
        when Topology.A => "A", when Topology.B => "B",
        when Topology.C => "C", when Topology.D => "D");
end Intel_GPU_Firmware_Output;
