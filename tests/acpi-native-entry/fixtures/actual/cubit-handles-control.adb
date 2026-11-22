------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  CuBit.Handles.Control - implementation
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements;
with CuBit.Handles.Kernel;

package body CuBit.Handles.Control is

   package K renames CuBit.Handles.Kernel;
   package ST renames CuBit.Startup_Tables;
   use type ST.Limit_Kind;

   --  A process name's length in the kernel (Process.ProcessName).
   Name_Bytes : constant := 16;

   function Address_Word (A : System.Address) return Unsigned_64;
   function Address_Word (A : System.Address) return Unsigned_64 is
     (Unsigned_64 (System.Storage_Elements.To_Integer (A)));

   function Spawn
     (Authority : Process;
      Image     : System.Address;
      Bytes     : Unsigned_64;
      Priority  : Unsigned_64;
      Name      : String;
      Parent    : Process_ID;
      Limit     : CuBit.Startup_Tables.Handle_Limit) return Spawn_Result
   is
      Text    : aliased String (1 .. Name_Bytes + 1) := [others => ASCII.NUL];
      Request : aliased ABI.Spawn_Request := [others => 0];
      Result  : aliased ABI.Spawn_Result := [others => 0];
      Answer  : Unsigned_64;
   begin
      for I in 1 .. Natural'Min (Name'Length, Name_Bytes) loop
         Text (I) := Name (Name'First + I - 1);
      end loop;
      Request :=
        [ABI.Image       => Address_Word (Image),
         ABI.Image_Bytes => Bytes,
         ABI.Priority    => Priority,
         ABI.Name        => (if Name'Length = 0 then 0 else Address_Word (Text'Address)),
         ABI.Parent      => CuBit.Process_IDs.To_Word (Parent),
         ABI.Limit       =>
           (if Limit.Kind = ST.Kernel_Limit
            then ABI.Limit_Kind'Enum_Rep (ABI.Kernel_Limit)
            else ABI.Limit_Kind'Enum_Rep (ABI.Bounded)),
         ABI.Limit_Count =>
           (if Limit.Kind = ST.Kernel_Limit then 0
            else Unsigned_64 (ST.Effective_Limit (Limit)))];
      Answer := K.Call (K.Spawn, K.Word (Authority),
                        Address_Word (Request'Address), Address_Word (Result'Address));
      if Answer /= 0 then
         return (Spawned => False, Why => K.Refusal_Of (Answer));
      end if;
      return
        (Spawned => True,
         Value   =>
           (Child              => CuBit.Process_IDs.From_Word (Result (ABI.Child)),
            Control            => K.To_Process (Result (ABI.Process)),
            Mailbox            => K.To_Endpoint (Result (ABI.Endpoint)),
            Space              => K.To_Cspace (Result (ABI.Cspace)),
            Child_Self         => K.To_Foreign (Result (ABI.Child_Self)),
            Child_Self_Process => K.To_Foreign (Result (ABI.Child_Self_Process))));
   end Spawn;

   function Install (Space : Cspace; Copy : Derivation) return Install_Result is
      Words  : aliased ABI.Derivation := K.Derivation_Words (Copy);
      Answer : constant Unsigned_64 :=
        K.Call (K.Install_Capability, K.Word (Space), Address_Word (Words'Address));
   begin
      if ABI.Is_Handle_Word (Answer) then
         return (Installed => True, Placed => K.To_Foreign (Answer));
      end if;
      return (Installed => False, Why => K.Refusal_Of (Answer));
   end Install;

   --  The handle word DERIVE_CAPABILITY placed, or the null word.
   function Derived_Word (Copy : Derivation; Expected : ABI.Kind) return Unsigned_64;
   function Derived_Word (Copy : Derivation; Expected : ABI.Kind) return Unsigned_64 is
      use type ABI.Kind;
      Words  : aliased ABI.Derivation := K.Derivation_Words (Copy);
      Answer : Unsigned_64;
   begin
      if Kind_Of (Copy) /= Expected then
         return ABI.No_Handle;
      end if;
      Answer := K.Call (K.Derive_Capability, Address_Word (Words'Address));
      return (if ABI.Is_Handle_Word (Answer) then Answer else ABI.No_Handle);
   end Derived_Word;

   function Derive (Copy : Derivation) return Endpoint is
     (K.To_Endpoint (Derived_Word (Copy, ABI.Endpoint)));
   function Derive (Copy : Derivation) return Notification is
     (K.To_Notification (Derived_Word (Copy, ABI.Notification)));
   function Derive (Copy : Derivation) return Device_Memory is
     (K.To_Device_Memory (Derived_Word (Copy, ABI.Device_Memory)));
   function Derive (Copy : Derivation) return IO_Ports is
     (K.To_IO_Ports (Derived_Word (Copy, ABI.IO_Ports)));
   function Derive (Copy : Derivation) return Interrupt is
     (K.To_Interrupt (Derived_Word (Copy, ABI.Interrupt)));
   function Derive (Copy : Derivation) return Scheduling is
     (K.To_Scheduling (Derived_Word (Copy, ABI.Scheduling)));
   function Derive (Copy : Derivation) return Process is
     (K.To_Process (Derived_Word (Copy, ABI.Process)));

   function Table_Word (Item : Foreign_Handle) return Unsigned_64 is (K.Word (Item));

   function Resume
     (Target : Process_ID; Authority : Process;
      Max_Frames, CPU_Quota : Unsigned_64) return Boolean is
     (K.Call (K.Resume, CuBit.Process_IDs.To_Word (Target), K.Word (Authority),
              Max_Frames, CPU_Quota) = 0);

   function Kill (Target : Process_ID; Authority : Process) return Boolean is
     (K.Call (K.Kill, CuBit.Process_IDs.To_Word (Target), K.Word (Authority)) = 0);

   function Install_Launch_Block
     (Target : Process_ID; Authority : Process;
      Block : System.Address; Bytes : Unsigned_64) return Boolean is
     (K.Call (K.Install_Launch_Arguments, CuBit.Process_IDs.To_Word (Target),
              Address_Word (Block), Bytes, K.Word (Authority)) = 0);

   function Set_CPU (Target : Process_ID; Authority : Process; CPU : Unsigned_64)
     return Boolean is
     (K.Call (K.Set_CPU, CuBit.Process_IDs.To_Word (Target), CPU, K.Word (Authority)) = 0);

   function Send_Control
     (Target : Process_ID; Kind : Unsigned_64; Authority : Process) return Unsigned_64 is
     (K.Call (K.Send_Control, CuBit.Process_IDs.To_Word (Target), Kind, K.Word (Authority)));

   function Allocate_DMA
     (Target : Process_ID; Authority : Process;
      Order, Virtual, Mode, Ceiling : Unsigned_64) return Unsigned_64 is
     (K.Call (K.Allocate_DMA, CuBit.Process_IDs.To_Word (Target), Order, Virtual,
              Mode, Ceiling, K.Word (Authority)));

   function Map_Into
     (Target : Process_ID; Authority : Process; Region : Device_Memory;
      Source, Virtual, Pages, Kind : Unsigned_64) return Boolean is
     (K.Call (K.Map_Into, CuBit.Process_IDs.To_Word (Target), Source, Virtual, Pages,
              Kind, K.Word (Authority), K.Word (Region)) = 0);

   function Process_List
     (Authority : Process; Buffer : System.Address; Bytes : Unsigned_64)
      return Unsigned_64 is
     (K.Call (K.Process_List, Address_Word (Buffer), Bytes, K.Word (Authority)));

   function Allocate_Vectors
     (Owner : Process_ID; Space : Cspace; Count, CPU : Unsigned_64;
      Authority : Interrupt) return Vector_Block
   is
      Result : aliased ABI.Vector_Allocation := [others => 0];
      Answer : constant Unsigned_64 :=
        K.Call (K.Allocate_Device_Vectors, CuBit.Process_IDs.To_Word (Owner), Count,
                K.Word (Space), CPU, K.Word (Authority), Address_Word (Result'Address));
   begin
      if Answer /= 0 then
         return (Allocated => False, Why => K.Refusal_Of (Answer));
      end if;
      return (Allocated  => True,
              Allocation => Result (ABI.Allocation),
              Owner      => K.To_Foreign (Result (ABI.Owner_Handle)));
   end Allocate_Vectors;

   function Release_Vectors (First : Unsigned_64; Authority : Interrupt) return Boolean is
     (K.Call (K.Release_Device_Vectors, First, K.Word (Authority)) = 0);

   function Read_Vector (Vector, Field : Unsigned_64; Authority : Interrupt)
     return Unsigned_64 is
     (K.Call (K.Read_Device_Vector, Vector, Field, K.Word (Authority)));

   function Enable_IRQ
     (Vector : Unsigned_64; Owner : Process_ID; Route : Unsigned_64;
      Authority : Process; Lines : Interrupt) return Boolean is
     (K.Call (K.Enable_IRQ, Vector, CuBit.Process_IDs.To_Word (Owner), Route,
              K.Word (Authority), K.Word (Lines)) = 0);

end CuBit.Handles.Control;
