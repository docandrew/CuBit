pragma Ada_2022;
with Interfaces; use Interfaces;

-- Numeric admission only. Firmware truth, mappings and device writes are
-- separate trusted adapters. No pointers, allocator, or Multiboot dependency.
package Boot_Framebuffer with SPARK_Mode, Pure is
   Page_Bytes : constant := 4096;
   type Address is range 0 .. 2 ** 48;
   subtype Extent is Positive range 1 .. 65_535;
   subtype Byte_Count is Positive range 1 .. 128 * 1024 * 1024;
   type Pixel_Format is (BGRX_8888);
   type Raw_Description is record
      Base : Unsigned_64 := 0;
      Width, Height, Pitch : Unsigned_32 := 0;
      Kind, Depth : Unsigned_8 := 0;
      Red_Position, Red_Size, Green_Position, Green_Size,
        Blue_Position, Blue_Size : Unsigned_8 := 0;
   end record;
   type Description is record
      Base, Map_First, Map_Limit : Address := 0; -- exclusive limit
      Width, Height : Extent := 1;
      Pitch, Bytes : Byte_Count := 4;
      Format : Pixel_Format := BGRX_8888;
   end record;
   function Valid (Item : Description) return Boolean is
     (Item.Base > 0 and then Item.Map_First <= Item.Base and then
      Item.Base - Item.Map_First < Page_Bytes and then
      Item.Map_First mod Page_Bytes = 0 and then
      Item.Map_Limit mod Page_Bytes = 0 and then
      Item.Map_Limit > Item.Base and then
      Item.Pitch mod 4 = 0 and then Item.Pitch >= Item.Width * 4 and then
      Address (Item.Bytes) = Address (Item.Pitch) * Address (Item.Height) and then
      Address (Item.Bytes) <= Item.Map_Limit - Item.Base and then
      Item.Map_Limit - Item.Base - Address (Item.Bytes) < Page_Bytes)
     with Ghost;
   type Status is
     (Success, Text_Mode, Unsupported_Format, Invalid_Geometry,
      Budget_Exceeded, Invalid_Address);
   type Result (State : Status := Unsupported_Format) is record
      case State is
         when Success => Value : Description;
         when others => null;
      end case;
   end record;
   function Decode (Raw : Raw_Description; Physical_Limit : Address;
                    Budget : Byte_Count) return Result
     with Post =>
       (if Decode'Result.State = Success then
          Valid (Decode'Result.Value) and then
          Decode'Result.Value.Map_Limit <= Physical_Limit and then
          Decode'Result.Value.Bytes <= Budget and then
          Unsigned_64 (Decode'Result.Value.Base) = Raw.Base and then
          Unsigned_32 (Decode'Result.Value.Width) = Raw.Width and then
          Unsigned_32 (Decode'Result.Value.Height) = Raw.Height and then
          Unsigned_32 (Decode'Result.Value.Pitch) = Raw.Pitch);

   function Overlaps (Item : Description; First, Limit : Address) return Boolean is
     (First < Limit and then Item.Map_First < Limit and then First < Item.Map_Limit);

   function Pixel_Offset (Item : Description; X, Y : Natural) return Natural
     with Pre => Valid (Item) and then X < Item.Width and then Y < Item.Height,
          Post => Pixel_Offset'Result <= Item.Bytes - 4;
end Boot_Framebuffer;
