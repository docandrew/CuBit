------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Machine_Code; use System.Machine_Code;
with System;
with System.Storage_Elements;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;

package body CuBit.Futex_Locks is

   package KA renames CuBit.Kernel_ABI;
   package KC renames CuBit.Kernel_Calls;

   Unlocked : constant Unsigned_32 := 0;
   Locked   : constant Unsigned_32 := 1;
   Contended : constant Unsigned_32 := 2;
   Wake_All : constant Unsigned_64 := Unsigned_64'Last;
   Wake_One : constant Unsigned_64 := 1;

   --  The word's own address (a Word parameter would be a copy).
   function Address_Word (Where : System.Address) return Unsigned_64 is
     (Unsigned_64 (System.Storage_Elements.To_Integer (Where)));

   --  Access parameters: a Word passed by value would be a copy, and the
   --  atomic operation would act on the copy.
   function CAS (W : access Word; Expected, Desired : Unsigned_32) return Unsigned_32;
   function CAS (W : access Word; Expected, Desired : Unsigned_32) return Unsigned_32 is
      Previous : Unsigned_32;
   begin
      Asm ("lock cmpxchgl %2, %1",
           Outputs => (Unsigned_32'Asm_Output ("=a", Previous),
                       Word'Asm_Output ("+m", W.all)),
           Inputs  => (Unsigned_32'Asm_Input ("r", Desired),
                       Unsigned_32'Asm_Input ("a", Expected)),
           Clobber => "memory, cc", Volatile => True);
      return Previous;
   end CAS;

   function Exchange (W : access Word; Value : Unsigned_32) return Unsigned_32;
   function Exchange (W : access Word; Value : Unsigned_32) return Unsigned_32 is
      Previous : Unsigned_32 := Value;
   begin
      Asm ("xchgl %0, %1",
           Outputs => (Unsigned_32'Asm_Output ("+r", Previous),
                       Word'Asm_Output ("+m", W.all)),
           Clobber => "memory", Volatile => True);
      return Previous;
   end Exchange;

   procedure Futex_Wait
     (Where : System.Address; Expected : Unsigned_32; Deadline : Unsigned_64; Timed_Out : out Boolean);
   procedure Futex_Wait
     (Where : System.Address; Expected : Unsigned_32; Deadline : Unsigned_64; Timed_Out : out Boolean) is
   begin
      Timed_Out := KC.Call (KA.Futex_Wait, Address_Word (Where), Unsigned_64 (Expected), Deadline)
                   = KA.Futex_Timed_Out;
   end Futex_Wait;

   procedure Futex_Wake (Where : System.Address; Count : Unsigned_64);
   procedure Futex_Wake (Where : System.Address; Count : Unsigned_64) is
      Ignore : constant Unsigned_64 := KC.Call (KA.Futex_Wake, Address_Word (Where), Count);
   begin
      null;
   end Futex_Wake;

   procedure Lock (M : in out Mutex) is
      C : Unsigned_32 := CAS (M.State'Access, Unlocked, Locked);
      Ignore : Boolean;
   begin
      if C = Unlocked then
         return;
      end if;
      if C /= Contended then
         C := Exchange (M.State'Access, Contended);
      end if;
      while C /= Unlocked loop
         Futex_Wait (M.State'Address, Contended, CuBit.Deadlines.Forever_Word, Ignore);
         C := Exchange (M.State'Access, Contended);
      end loop;
   end Lock;

   procedure Unlock (M : in out Mutex) is
   begin
      if Exchange (M.State'Access, Unlocked) = Contended then
         Futex_Wake (M.State'Address, Wake_One);
      end if;
   end Unlock;

   function Current (E : Event) return Unsigned_32 is (Unsigned_32 (E.Generation));

   function Wait (E : in out Event; Seen : Unsigned_32; Deadline : CuBit.Deadlines.Deadline)
     return Boolean
   is
      Timed_Out : Boolean;
   begin
      if Unsigned_32 (E.Generation) /= Seen then
         return True;
      end if;
      Futex_Wait (E.Generation'Address, Seen, CuBit.Deadlines.Kernel_Word (Deadline), Timed_Out);
      return not Timed_Out or else Unsigned_32 (E.Generation) /= Seen;
   end Wait;

   procedure Signal (E : in out Event) is
      Previous : Unsigned_32 := 1;
   begin
      Asm ("lock xaddl %0, %1",
           Outputs => (Unsigned_32'Asm_Output ("+r", Previous),
                       Word'Asm_Output ("+m", E.Generation)),
           Clobber => "memory, cc", Volatile => True);
      Futex_Wake (E.Generation'Address, Wake_All);
   end Signal;

end CuBit.Futex_Locks;
