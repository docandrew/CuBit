--  Proved round trips: decoding an encoder's output accepts it and returns
--  the value encoded (gnatprove, run.sh --prove).
pragma Ada_2022;
with Launch_Configuration; use Launch_Configuration;

package Round_Trips with SPARK_Mode is

   procedure Device_Manager (Value : Device_Manager_Configuration;
                             Decoded : out Device_Manager_Configuration;
                             Valid : out Boolean)
   with Post => Valid and then Decoded = Value;

   procedure Process_Manager (Value : Process_Manager_Configuration;
                              Decoded : out Process_Manager_Configuration;
                              Valid : out Boolean)
   with Post => Valid and then Decoded = Value;

   procedure Boot_Image (Value : Boot_Image_Configuration;
                         Decoded : out Boot_Image_Configuration;
                         Valid : out Boolean)
   with Post => Valid and then Decoded = Value;

   procedure Controller (Value : Controller_Configuration; Kind : Configuration_Kind;
                         Decoded : out Controller_Configuration;
                         Valid : out Boolean)
   with Pre  => Kind in NVMe_Controller | HDA_Controller,
        Post => Valid and then Decoded = Value;

   procedure Queue (Value : Queue_Configuration; Kind : Configuration_Kind;
                    Decoded : out Queue_Configuration;
                    Valid : out Boolean)
   with Pre  => Kind in Virtio_Net | Virtio_Input,
        Post => Valid and then Decoded = Value;

   procedure GPU (Value : Virtio_GPU_Configuration;
                  Decoded : out Virtio_GPU_Configuration;
                  Valid : out Boolean)
   with Post => Valid and then Decoded = Value;

   --  A block for one kind never decodes as another.
   procedure Kinds_Distinct (Value : Controller_Configuration; Valid : out Boolean)
   with Post => not Valid;
end Round_Trips;
