------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Machine_Code; use System.Machine_Code;
with System.Storage_Elements;
with Clock_Publication; use Clock_Publication;
with Clock_Publication.Sample;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;

package body CuBit.Published_Clock is

   package K renames CuBit.Kernel_ABI;

   Published_Page : Page
   with Import, Volatile,
        Address => System.Storage_Elements.To_Address (Page_Address);

   function Load_Sequence return Unsigned_64 with Inline;
   function Load_Sequence return Unsigned_64 is (Published_Page.Sequence);

   function Load_Fields return Parameters with Inline;
   function Load_Fields return Parameters is (Published_Page.Fields);

   --  Ordered between the counter loads around it.
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

   procedure Sample is new Clock_Publication.Sample
     (Load_Sequence, Load_Fields, Load_Counter);

   procedure Read_Nanoseconds
     (Nanoseconds : out Unsigned_64; Published : out Boolean) renames Sample;

   function Milliseconds return Unsigned_64 is
      Nanoseconds : Unsigned_64;
      Published   : Boolean;
   begin
      Sample (Nanoseconds, Published);
      if Published then
         return Clock_Publication.Milliseconds (Nanoseconds);
      end if;
      return CuBit.Kernel_Calls.Call (K.Get_Time);
   end Milliseconds;

   procedure Microseconds (Value : out Unsigned_64; Available : out Boolean)
   is
      Nanoseconds : Unsigned_64;
   begin
      Sample (Nanoseconds, Available);
      if Available then
         Value := Clock_Publication.Microseconds (Nanoseconds);
         return;
      end if;
      Value := CuBit.Kernel_Calls.Call (K.Read_Monotonic_Microseconds);
      Available := Value /= K.Failed;
      if not Available then
         Value := 0;
      end if;
   end Microseconds;

   function Counter_Frequency return Unsigned_64 is
      Fields : constant Parameters := Load_Fields;
   begin
      return (if Valid (Fields) then Fields.Frequency else 0);
   end Counter_Frequency;

end CuBit.Published_Clock;
