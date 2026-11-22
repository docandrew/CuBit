------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Benchmark_Clock;
with CuBit.Published_Clock;
with Desktop_Logs;
with Desktop_Timing_Policy;

package body Desktop_Memory_Bench is

   use type System.Address;

   Done : Boolean := False;

   function memcpy (Target, Source : System.Address; Bytes : Storage_Count) return System.Address
     with Import, Convention => C, External_Name => "memcpy";

   type Byte_Block is array (1 .. Bench_Bytes) of Unsigned_8 with Alignment => 4096;
   type Block_Access is access Byte_Block;
   procedure Free is new Ada.Unchecked_Deallocation (Byte_Block, Block_Access);

   --  Picoseconds per byte for Ticks over Bytes, as "N.NNN" ns.
   function Ns_Per_Byte (Ticks : Unsigned_64; Bytes : Natural) return String is
      Hz : constant Unsigned_64 := CuBit.Published_Clock.Counter_Frequency;
      Ps : Unsigned_64;
   begin
      if Hz = 0 or else Bytes = 0 then
         return "unknown";
      end if;
      --  ticks * 1e12 / Hz / bytes, in two steps to stay in range.
      Ps := (Ticks * 1_000_000 / (Hz / 1_000_000)) / Unsigned_64 (Bytes);
      declare
         Whole : constant String := Unsigned_64'Image (Ps / 1000);
         Frac : constant String := Unsigned_64'Image (1000 + Ps mod 1000);
      begin
         return Whole (Whole'First + 1 .. Whole'Last) & "." & Frac (Frac'First + 2 .. Frac'Last);
      end;
   end Ns_Per_Byte;

   procedure Run_Once (Lent : System.Address; Lent_Bytes : Natural) is
      Bytes : constant Natural := Natural'Min (Bench_Bytes, Lent_Bytes);
      A, B : Block_Access;
      Ignore : System.Address;
      Best_Private, Best_Lent, Best_Read : Unsigned_64 := Unsigned_64'Last;
      T0, T1 : Unsigned_64;
      Sum : Unsigned_64 := 0;
   begin
      if Done or else not Desktop_Timing_Policy.Enabled or else Lent = System.Null_Address
        or else Bytes < 4096
      then
         return;
      end if;
      Done := True;
      A := new Byte_Block'(others => 16#5A#);
      B := new Byte_Block'(others => 16#A5#);
      for Run in 1 .. Runs loop
         T0 := CuBit.Benchmark_Clock.Read_Counter;
         Ignore := memcpy (B.all'Address, A.all'Address, Storage_Count (Bytes));
         T1 := CuBit.Benchmark_Clock.Read_Counter;
         Best_Private := Unsigned_64'Min (Best_Private, T1 - T0);
         T0 := CuBit.Benchmark_Clock.Read_Counter;
         Ignore := memcpy (Lent, A.all'Address, Storage_Count (Bytes));
         T1 := CuBit.Benchmark_Clock.Read_Counter;
         Best_Lent := Unsigned_64'Min (Best_Lent, T1 - T0);
         declare
            --  SEC-025: snapshot: a timing read; the values are summed and dropped.
            Words : array (1 .. Bytes / 8) of Unsigned_64 with Import, Volatile, Address => Lent;
         begin
            T0 := CuBit.Benchmark_Clock.Read_Counter;
            for W of Words loop
               Sum := Sum + W;
            end loop;
            T1 := CuBit.Benchmark_Clock.Read_Counter;
         end;
         Best_Read := Unsigned_64'Min (Best_Read, T1 - T0);
      end loop;
      Desktop_Logs.Write
        ("DESKTOP-MEMORY-BENCH: bytes=" & Natural'Image (Bytes) &
         " private_to_private=" & Ns_Per_Byte (Best_Private, Bytes) &
         " private_to_lent=" & Ns_Per_Byte (Best_Lent, Bytes) &
         " lent_read=" & Ns_Per_Byte (Best_Read, Bytes) &
         " (ns/byte, best of" & Natural'Image (Runs) & ")" &
         (if Sum = 1 then " " else "") & ASCII.LF);
      Free (A);
      Free (B);
   end Run_Once;

end Desktop_Memory_Bench;
