pragma Ada_2022;
with Interfaces; use Interfaces;
package body Round_Trips with SPARK_Mode is

   procedure Device_Manager (Value : Device_Manager_Configuration;
                             Decoded : out Device_Manager_Configuration;
                             Valid : out Boolean) is
      E : constant Encoded := Encode (Value);
      H : constant Word_Array := Handle_Words (Value.Handles) with Ghost;
   begin
      pragma Assert (for all I in 1 .. 21 => E.Words (6 + I) = H (I));
      pragma Assert (Handles_Valid (E.Words (7 .. 27)));
      Decode (E.Words (1 .. E.Length), Decoded, Valid);
      pragma Assert (Valid);
      pragma Assert
        (for all I in 1 .. 21 => Handle_Words (Decoded.Handles) (I) = H (I));
      pragma Assert (Decoded.Handles.Root_Count = Value.Handles.Root_Count);
      pragma Assert (Decoded.Handles.Device_Roots = Value.Handles.Device_Roots);
      pragma Assert (Decoded.Handles = Value.Handles);
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
