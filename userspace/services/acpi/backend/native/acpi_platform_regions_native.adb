pragma Ada_2022;
with CuBit.Handles.IPC;
with CuBit.Handles.Platform_Pools;
with CuBit.Monotonic;
package body ACPI_Platform_Regions_Native is
   use type Word, CuBit.Handles.ABI.Kind, CuBit.Handles.Rights;
   function Inspect_Pool (Held : CuBit.Handles.Device_Memory) return Boolean is
      D : constant CuBit.Handles.IPC.Description := CuBit.Handles.IPC.Describe (Held);
   begin
      return D.Current and then D.Kind = CuBit.Handles.ABI.Device_Memory
        and then D.Authority_Tag = Platform_Pool_ABI.Platform_Pool_Tag
        and then D.Held = CuBit.Handles.Read_Write;
   end Inspect_Pool;
   procedure Map_Chunk (Held : CuBit.Handles.Device_Memory; Physical, Virtual, Pages : Word;
      Read_Only : Boolean; Status : out Word; Why : out Platform_Pool_ABI.Refusal_Record) is
   begin
      Status := CuBit.Handles.Platform_Pools.Map (Held,Physical,Virtual,Pages,Read_Only,Why);
   end Map_Chunk;
   procedure Read_Clock (Now : out Word; Available : out Boolean) is
      Sample : constant CuBit.Monotonic.Reading := CuBit.Monotonic.Read;
   begin
      Now := 0; Available := Sample.Available;
      if Sample.Available then Now := Sample.Microseconds; end if;
   end Read_Clock;
end ACPI_Platform_Regions_Native;
