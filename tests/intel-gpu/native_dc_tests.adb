with Ada.Text_IO; with Interfaces; with Interfaces.C; with System;
with System.Storage_Elements;
with Intel_GPU_Native_DC; with Intel_GPU_Combo_PHY;
procedure Native_DC_Tests is
   use Interfaces; use System; use System.Storage_Elements;
   use type Interfaces.C.int;
   type Integer_Address_Array is array (Positive range <>) of Integer_Address;
   function Mmap (Address : System.Address; Length : Interfaces.C.size_t;
     Protection, Flags, FD : Interfaces.C.int; Offset : Interfaces.C.long) return System.Address
     with Import, Convention => C, External_Name => "mmap";
   function Munmap (Address : System.Address; Length : Interfaces.C.size_t) return Interfaces.C.int
     with Import, Convention => C, External_Name => "munmap";
   function Load (Address : Integer_Address) return Unsigned_32 is
      V : Unsigned_32 with Import, Volatile_Full_Access, Address => To_Address (Address);
   begin return V; end Load;
   procedure Store (Address : Integer_Address; Value : Unsigned_32) is
      V : Unsigned_32 with Import, Volatile_Full_Access, Address => To_Address (Address);
   begin V := Value; end Store;
   procedure Test (Mode : Natural) is
      Time : Unsigned_64 := 0;
      function Power return Boolean is
      begin
         -- Model the two CPU aliases for DC_STATE_EN. This is not a device.
         if Mode < 4 or Mode >= 7 then
            Store (16#60045504#, Load (16#61400504#));
            if Mode = 7 and Load (16#61400504#) = 16#00300000# then
               Store (16#60046000#, 1);
            end if;
         end if;
         return Mode /= 5;
      end Power;
      function Pages return Boolean is (Mode /= 4);
      function Now_Us return Unsigned_64 is
      begin
         if Mode = 1 then return 100; end if;
         Time := Time + 100; return Time;
      end Now_Us;
      package Native is new Intel_GPU_Native_DC (Power, Pages, Pages, Now_Us);
   begin
      if Mode < 4 or Mode >= 7 then
         for R of Integer_Address_Array'(16#46000#,16#46070#,16#51004#,
           16#45008#,16#44FE8#,16#44300#,16#44304#) loop
            Store (16#60000000# + R, 0);
         end loop;
         Store (16#61400504#, (if Mode = 8 then Unsigned_32'Last else 16#6030000B#));
         for Port in Intel_GPU_Combo_PHY.PHY loop
            declare
               use Intel_GPU_Combo_PHY;
               S : Snapshot := (others => 0);
               P : constant Plan := Prepare (Port, S);
            begin
               for I in 1 .. P.Count loop S (P.Writes (I).Register) := P.Writes (I).Value; end loop;
               for F in Field loop
                  Store (16#60000000# + Integer_Address (Read_Offset (Port, F)), S (F));
               end loop;
            end;
         end loop;
         if Mode = 2 then Store (16#60045008#, 16#80000000#); end if;
         if Mode = 3 then Store (16#6006C10C#, 16#1F000000#); end if;
      end if;
      declare Result : constant String := Native.Execute (Mode /= 6); begin
         if Mode = 0 then
            pragma Assert (Native.Held and Result = "ready stage=finished exit=ready");
            pragma Assert (Native.PHY_Diagnostic = "PHY=B result=ready writes= 0");
            pragma Assert (Load (16#61400504#) = 16#00300000#); -- latch bits preserved
         else
            pragma Assert (not Native.Held and Result /= "ready stage=finished exit=ready");
         end if;
         if Mode = 2 then pragma Assert (Load (16#61400504#) = 16#6030000B#); end if;
      end;
      pragma Assert (Native.Execute (True) = "already-attempted");
   end Test;
   procedure Reserve (Base : Integer_Address; Size : Interfaces.C.size_t) is
      Mapping : constant System.Address := Mmap (To_Address (Base), Size, 3, 16#100022#, -1, 0);
   begin
      if Mapping /= To_Address (Base) then
         if Mapping /= To_Address (Integer_Address'Last) then
            declare Ignored : constant Interfaces.C.int := Munmap (Mapping, Size); begin null; end;
         end if;
         raise Program_Error with "cannot reserve native DC fixture pages";
      end if;
   end Reserve;
begin
   for Mode in 4 .. 6 loop Test (Mode); end loop;
   Reserve (16#60044000#, 16#11F000#); Reserve (16#61400000#, 4096);
   for Mode in 0 .. 3 loop Test (Mode); end loop;
   for Mode in 7 .. 8 loop Test (Mode); end loop;
   pragma Assert (Munmap (To_Address (16#60044000#), 16#11F000#) = 0);
   pragma Assert (Munmap (To_Address (16#61400000#), 4096) = 0);
   Ada.Text_IO.Put_Line ("native DC PASS: mapped stores, combined verification, failure/no-replay paths");
end Native_DC_Tests;
