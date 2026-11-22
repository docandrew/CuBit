with System;
with Interfaces;
-- Trusted Vulkan status adapter. Global null abstracts the exclusively owned
-- native objects referenced by Borrowed, not absence of physical side effects.
package Vulkan_Submission_FFI with SPARK_Mode is
   subtype Code is Interfaces.Unsigned_32;
   procedure Fill (Borrowed : System.Address; Width, Height, Left, Top, Right, Bottom, RGB : Code; Result : out Code) with Global => null;
   procedure Start (Borrowed : System.Address; Result : out Code) with Global => null;
   procedure Seal (Borrowed : System.Address; Result : out Code) with Global => null;
   procedure Submit (Borrowed : System.Address; Result : out Code) with Global => null;
   procedure Poll (Borrowed : System.Address; Result : out Code) with Global => null;
   procedure Cancel (Borrowed : System.Address; Result : out Code) with Global => null;
   procedure Begin_Scene (Borrowed, Pass : System.Address; Width, Height : Code; Result : out Code) with Global => null;
   procedure End_Scene (Borrowed : System.Address; Result : out Code) with Global => null;
   procedure Matches (Borrowed, Draw : System.Address; Result : out Code) with Global => null;
   -- Import 0 publishes one retained draw; 1 rejects with no retained view;
   -- any other status is uncertain. Release 0 proves view teardown only.
   procedure Import_Source (Description : System.Address; Draw : out System.Address;
      Result : out Code) with Global => null;
   procedure Release_Source (Draw : System.Address; Result : out Code) with Global => null;
end Vulkan_Submission_FFI;
