pragma Ada_2022;
with Interfaces; use Interfaces;
with System.Machine_Code; use System.Machine_Code;
with BuddyAllocator;
with Config;
with Mem_mgr;
with PerCPUData;
with Util;
with Virtmem; use Virtmem;

package body Kernel_Window is
   -- 0xFFFF_FF00_0000_0000: P4 entry 510, sign-extended.
   Window_Base : constant Integer_Address :=
     16#FFFF_0000_0000_0000# + Integer_Address (Window_P4_Index) * 2 ** 39;
   CPU_Span : constant := 2 ** 21;   -- one P2 entry (one P1 table) per CPU
   pragma Compile_Time_Error (Config.MAX_CPUS > 512, "One P2 table must hold every CPU's window");

   -- The window's own tables: kernel-owned frames, mapped like all such.
   Window_P2 : PhysAddress := 0;
   type P1_Table_Array is array (0 .. Config.MAX_CPUS - 1) of PhysAddress;
   CPU_P1 : P1_Table_Array := [others => 0];
   Open_Slot : array (0 .. Config.MAX_CPUS - 1) of Boolean := [others => False];

   Window_Flags : constant Unsigned_64 := PG_PRESENT + PG_WRITABLE + PG_NXE;
   Table_Flags  : constant Unsigned_64 := PG_PRESENT + PG_WRITABLE;

   WindowException : exception;

   procedure New_Table (Frame : out PhysAddress) is
   begin
      -- Zeroed by the allocator; kernel-owned, in the kernel's map.
      BuddyAllocator.allocFrame (Frame);
      if Frame = 0 then
         raise WindowException with "No frame for the kernel window's tables";
      end if;
   end New_Table;

   procedure Setup is
      P3_Frame : PhysAddress;
   begin
      New_Table (P3_Frame);
      New_Table (Window_P2);
      declare
         P3_Table : P3 with Import, Address => To_Address (P2V (P3_Frame));
      begin
         P3_Table (0) := makePTE (addrToPFN (Window_P2), Table_Flags);
      end;
      Mem_mgr.installKernelTable (Window_P4_Index, P3_Frame);
   end Setup;

   function Slot_Address (CPU : Natural) return Integer_Address is
     (Window_Base + Integer_Address (CPU) * CPU_Span);

   procedure Open (Frame : Process_Frames.Process_Frame; Where : out System.Address) is
      CPU : Natural;
   begin
      PerCPUData.pushCLI;
      CPU := PerCPUData.getCPUNumber;
      if Open_Slot (CPU) then
         raise WindowException with "Kernel window sections do not nest";
      end if;
      if CPU_P1 (CPU) = 0 then
         -- First use on this CPU: its P1 table. A not-present P2 entry is
         -- never cached, so installing it needs no invalidation.
         New_Table (CPU_P1 (CPU));
         declare
            P2_Table : P2 with Import, Address => To_Address (P2V (Window_P2));
         begin
            P2_Table (PageTableIndex (CPU)) := makePTE (addrToPFN (CPU_P1 (CPU)), Table_Flags);
         end;
      end if;
      declare
         P1_Table : P1 with Import, Address => To_Address (P2V (CPU_P1 (CPU)));
      begin
         P1_Table (0) := makePTE
           (addrToPFN (PhysAddress (Process_Frames.Base (Frame))), Window_Flags);
      end;
      Open_Slot (CPU) := True;
      Where := To_Address (Slot_Address (CPU));
   end Open;

   procedure Close is
      CPU : constant Natural := PerCPUData.getCPUNumber;
      Slot : constant Integer_Address := Slot_Address (CPU);
   begin
      if not Open_Slot (CPU) then
         raise WindowException with "Kernel window closed without a section";
      end if;
      declare
         P1_Table : P1 with Import, Address => To_Address (P2V (CPU_P1 (CPU)));
      begin
         P1_Table (0) := EMPTY_PTE;
      end;
      Asm ("invlpg (%0)", Inputs => Integer_Address'Asm_Input ("r", Slot),
           Clobber => "memory", Volatile => True);
      Open_Slot (CPU) := False;
      PerCPUData.popCLI;
   end Close;

   procedure Write
     (Frame : Process_Frames.Process_Frame; Offset : Storage_Offset;
      Source : System.Address; Bytes : Storage_Count)
   is
      Where : System.Address;
   begin
      Open (Frame, Where);
      Util.memCopy (Where + Offset, Source, Bytes);
      Close;
   end Write;

   procedure Zero (Frame : Process_Frames.Process_Frame) is
      Where : System.Address;
      Ignore : System.Address;
   begin
      Open (Frame, Where);
      Ignore := Util.memset (Where, 0, Slot_Bytes);
      Close;
   end Zero;
end Kernel_Window;
