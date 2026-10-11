with System;
--  Thin binding to the glue's exported C symbols; nothing to prove here.
package body Desktop_GPU_Wake_Hook is
   use type Interfaces.Unsigned_32;
   function Wake_Arm (Token : Interfaces.Unsigned_64) return Interfaces.Unsigned_32
     with Import, Convention => C, External_Name => "cubit_gpu_wake_arm";
   function Wake_Collect (Receipt : System.Address) return Interfaces.Unsigned_32
     with Import, Convention => C, External_Name => "cubit_gpu_wake_collect";
   function Report (Buffer : System.Address; Capacity : Interfaces.Unsigned_32)
     return Interfaces.Unsigned_32
     with Import, Convention => C, External_Name => "cubit_mesa_blocking_report";
   function Jobs (Buffer : System.Address; Capacity : Interfaces.Unsigned_32)
     return Interfaces.Unsigned_32
     with Import, Convention => C, External_Name => "cubit_vulkan_job_report";
   function Hint return Interfaces.Unsigned_32
     with Import, Convention => C, External_Name => "cubit_gpu_boost_hint";
   function Boost_Hint return Boolean is (Hint = 1);
   --  Native_GPU_Queue.Wake_Arm's representation.
   Armed_Code : constant := 0;
   Idle_Code  : constant := 1;

   function Arm (Token : Interfaces.Unsigned_64) return Arm_Result is
      Code : constant Interfaces.Unsigned_32 := Wake_Arm (Token);
   begin
      return (if Code = Armed_Code then Armed elsif Code = Idle_Code then Idle else Unavailable);
   end Arm;

   function Collect (Receipt : CuBit.Messages.CompletionEntry) return Boolean is
     (Wake_Collect (Receipt'Address) = 1);

   procedure Blocking_Report (Text : out String; Last : out Natural) is
      Length : Interfaces.Unsigned_32;
   begin
      Text := [others => ' '];
      Last := Text'First - 1;
      if Text'Length < 2 then
         return;
      end if;
      --  The C side writes Length bytes and a NUL within Capacity.
      Length := Report (Text'Address, Interfaces.Unsigned_32 (Text'Length));
      if Length > 0 and then Natural (Length) < Text'Length then
         Last := Text'First + Natural (Length) - 1;
      end if;
   end Blocking_Report;
   procedure Job_Report (Text : out String; Last : out Natural) is
      Length : Interfaces.Unsigned_32;
   begin
      Text := [others => ' '];
      Last := Text'First - 1;
      if Text'Length < 2 then
         return;
      end if;
      Length := Jobs (Text'Address, Interfaces.Unsigned_32 (Text'Length));
      if Length > 0 and then Natural (Length) < Text'Length then
         Last := Text'First + Natural (Length) - 1;
      end if;
   end Job_Report;
end Desktop_GPU_Wake_Hook;
