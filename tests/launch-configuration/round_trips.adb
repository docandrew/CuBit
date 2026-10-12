pragma Ada_2022;
package body Round_Trips with SPARK_Mode is

   procedure Device_Manager (Value : Device_Manager_Configuration;
                             Decoded : out Device_Manager_Configuration;
                             Valid : out Boolean) is
      E : constant Encoded := Encode (Value);
   begin
      Decode (E.Words (1 .. E.Length), Decoded, Valid);
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
end Round_Trips;
