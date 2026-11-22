------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Machine_Code; use System.Machine_Code;
with System.Storage_Elements;
with CPU_Times_Publication.Sample;
with CuBit.Launch_Block;
pragma Elaborate_All (CuBit.Launch_Block);
with CuBit.Startup_State;
with CuBit.Startup_Tables;

package body CuBit.CPU_Times is
   Published_Page : CTP.Page
   with Import, Volatile,
        Address => System.Storage_Elements.To_Address (CTP.Page_Address);

   --  Withed for its elaboration: the startup table is loaded first.
   Loaded_First : constant Boolean := CuBit.Launch_Block.Table_Present;
   pragma Unreferenced (Loaded_First);

   --  The grant, looked up once (the startup table does not change).
   type Grant_State is (Unchecked, Granted, Not_Granted);
   Grant : Grant_State := Unchecked;

   function Is_Granted return Boolean;
   function Is_Granted return Boolean is
   begin
      if Grant = Unchecked then
         Grant :=
           (if CuBit.Startup_State.Granted (Label, CuBit.Startup_Tables.Publication)
                 = CTP.Page_Address
            then Granted else Not_Granted);
      end if;
      return Grant = Granted;
   end Is_Granted;

   function Available return Boolean is
     (Is_Granted and then Published_Page.Head.Version = CTP.Layout_Version);

   function Load_Sequence (CPU : CTP.Processor) return Unsigned_64 with Inline;
   function Load_Sequence (CPU : CTP.Processor) return Unsigned_64 is
     (Published_Page.Records (CPU).Sequence);

   function Load_Values (CPU : CTP.Processor) return CTP.Fields with Inline;
   function Load_Values (CPU : CTP.Processor) return CTP.Fields is
     (Published_Page.Records (CPU).Values);

   function Load_Counter return Unsigned_64 with Inline;
   function Load_Counter return Unsigned_64 is
      Low, High : Unsigned_32;
   begin
      Asm ("lfence; rdtsc; lfence",
           Outputs  => [Unsigned_32'Asm_Output ("=a", Low),
                        Unsigned_32'Asm_Output ("=d", High)],
           Clobber  => "memory",
           Volatile => True);
      return Shift_Left (Unsigned_64 (High), 32) or Unsigned_64 (Low);
   end Load_Counter;

   procedure Sample is new CTP.Sample (Load_Sequence, Load_Values, Load_Counter);

   procedure Read (Into : out CuBit.CPU_Load.Snapshot) is
      Count : Unsigned_64;
   begin
      Into := (others => <>);
      if not Available then
         return;
      end if;
      Count := Unsigned_64'Min (Published_Page.Head.Processors, CTP.Maximum_Processors);
      Into.Count := Count;
      for CPU in CTP.Processor loop
         exit when Unsigned_64 (CPU) >= Count;
         Sample (CPU, Into.CPUs (CPU));
      end loop;
   end Read;

   function Tick_Frequency return Unsigned_64 is
     (if Available then Published_Page.Head.Tick_Frequency else 0);

   function Free_Memory_Bytes return Unsigned_64 is
     (if Available then Published_Page.Head.Free_Memory_Bytes else 0);

   function Memory_Bytes return Unsigned_64 is
     (if Available then Published_Page.Head.Memory_Bytes else 0);
end CuBit.CPU_Times;
