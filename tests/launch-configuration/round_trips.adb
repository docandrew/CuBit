pragma Ada_2022;
with Interfaces; use Interfaces;
package body Round_Trips with SPARK_Mode is

   procedure Device_Manager (Value : Device_Manager_Configuration;
                             Roots : Word_Array;
                             Decoded : out Device_Manager_Configuration;
                             Valid : out Boolean) is
      E : constant Encoded := Encode_Prefix (Value);
      H : constant Word_Array := Handle_Words (Value.Handles) with Ghost;
      Length : constant Word_Index := Device_Manager_Length (Value);
      Block : Word_Array (1 .. Length) := [others => 0];
   begin
      Block (1 .. Device_Manager_Prefix_Words) := E.Words (1 .. Device_Manager_Prefix_Words);
      Block (Device_Manager_Prefix_Words + 1 .. Length) := Roots;
      pragma Assert (for all I in 1 .. Device_Manager_Prefix_Words => Block (I) = E.Words (I));
      pragma Assert (Block (Device_Roots_Word) = Value.Device_Roots);
      pragma Assert (Block (Firmware_Roots_Word) = Value.Firmware_Roots);
      pragma Assert (Block (Boot_Image_Word) = Value.Boot_Image);
      pragma Assert (Block (Framebuffer_First_Word) = Value.Framebuffer.First);
      pragma Assert (Block (Framebuffer_Bytes_Word) = Value.Framebuffer.Bytes);
      pragma Assert (for all I in 1 .. Device_Manager_Handle_Fields =>
                       Block (First_Handle_Word - 1 + I) = H (I));
      pragma Assert (for all I in First_Handle_Word .. Device_Manager_Prefix_Words =>
                       Is_Handle (Block (I)));
      pragma Assert (for all I in Device_Manager_Prefix_Words + 1 .. Length =>
                       Block (I) = Roots (I - Device_Manager_Prefix_Words));
      pragma Assert (for all I in First_Handle_Word .. Length => Is_Handle (Block (I)));
      pragma Assert (Device_Manager_Valid (Block));
      Decode (Block, Decoded, Valid);
      pragma Assert (Valid);
      pragma Assert
        (for all I in 1 .. Device_Manager_Handle_Fields =>
           Handle_Words (Decoded.Handles) (I) = H (I));
      pragma Assert (Decoded.Handles = Value.Handles);
      pragma Assert (Decoded.Device_Roots = Value.Device_Roots
                     and then Decoded.Firmware_Roots = Value.Firmware_Roots
                     and then Decoded.Boot_Image = Value.Boot_Image
                     and then Decoded.Framebuffer = Value.Framebuffer);
   end Device_Manager;

   procedure Process_Manager (Value : Process_Manager_Configuration;
                              Decoded : out Process_Manager_Configuration;
                              Valid : out Boolean) is
      E : constant Encoded := Encode (Value);
   begin
      Decode (E.Words (1 .. E.Length), Decoded, Valid);
   end Process_Manager;

   procedure Boot_Image (Value : Boot_Image_Configuration;
                         Decoded : out Boot_Image_Configuration;
                         Valid : out Boolean) is
      E : constant Encoded := Encode (Value);
   begin
      Decode (E.Words (1 .. E.Length), Decoded, Valid);
   end Boot_Image;

   procedure Controller (Value : Controller_Configuration; Kind : Configuration_Kind;
                         Decoded : out Controller_Configuration;
                         Valid : out Boolean) is
      E : constant Encoded := Encode (Value, Kind);
   begin
      Decode (E.Words (1 .. E.Length), Kind, Decoded, Valid);
   end Controller;

   procedure Queue (Value : Queue_Configuration; Kind : Configuration_Kind;
                    Decoded : out Queue_Configuration;
                    Valid : out Boolean) is
      E : constant Encoded := Encode (Value, Kind);
   begin
      Decode (E.Words (1 .. E.Length), Kind, Decoded, Valid);
   end Queue;

   procedure Network_Device (Value : Network_Device_Configuration;
                             Decoded : out Network_Device_Configuration;
                             Valid : out Boolean) is
      E : constant Encoded := Encode (Value);
   begin
      Decode (E.Words (1 .. E.Length), Decoded, Valid);
   end Network_Device;

   procedure GPU (Value : Virtio_GPU_Configuration;
                  Decoded : out Virtio_GPU_Configuration;
                  Valid : out Boolean) is
      E : constant Encoded := Encode (Value);
   begin
      Decode (E.Words (1 .. E.Length), Decoded, Valid);
   end GPU;

   procedure Kinds_Distinct (Value : Controller_Configuration; Valid : out Boolean) is
      E : constant Encoded := Encode (Value, NVMe_Controller);
      Decoded : Controller_Configuration;
   begin
      Decode (E.Words (1 .. E.Length), HDA_Controller, Decoded, Valid);
   end Kinds_Distinct;

   procedure XHCI_Distinct (Value : Controller_Configuration; Valid : out Boolean) is
      E : constant Encoded := Encode (Value, XHCI_Controller);
      Decoded : Controller_Configuration;
   begin
      Decode (E.Words (1 .. E.Length), NVMe_Controller, Decoded, Valid);
   end XHCI_Distinct;
end Round_Trips;
