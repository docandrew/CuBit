------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  This process's typed launch configuration (shared/launch,
--  docs/syscall-retirement.md): what its spawner installed before its first
--  instruction. A driver gets its registers and DMA addresses here, visible
--  to it alone.
--
--  @description
--  Each Get decodes the block the kernel mapped at
--  Launch_Configuration.Block_Address with the proved decoder; Valid is
--  False when there is none, or it is not a configuration of that kind.
------------------------------------------------------------------------------
pragma Ada_2022;
with Launch_Configuration;

package CuBit.Launch_Configurations is

   package LC renames Launch_Configuration;

   procedure Get (Value : out LC.Device_Manager_Configuration; Valid : out Boolean);
   procedure Get (Value : out LC.Process_Manager_Configuration; Valid : out Boolean);
   procedure Get (Value : out LC.Boot_Image_Configuration; Valid : out Boolean);
   procedure Get (Kind  : LC.Configuration_Kind;
                  Value : out LC.Controller_Configuration; Valid : out Boolean)
   with Pre => LC.Is_Controller (Kind);
   procedure Get (Kind  : LC.Configuration_Kind;
                  Value : out LC.Queue_Configuration; Valid : out Boolean)
   with Pre => Kind in LC.Virtio_Input;
   procedure Get (Value : out LC.Network_Device_Configuration; Valid : out Boolean);
   procedure Get (Value : out LC.Virtio_GPU_Configuration; Valid : out Boolean);
   procedure Get (Value : out LC.Generic_PCI_Configuration; Valid : out Boolean);

end CuBit.Launch_Configurations;
