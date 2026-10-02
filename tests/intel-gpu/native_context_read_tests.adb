with Ada.Text_IO;
with Interfaces; use Interfaces;
with Interfaces.C;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_ADLN_Steering;
with Intel_GPU_Native_Context_Read;
procedure Native_Context_Read_Tests is
   Owned : Boolean := False;
   T : Intel_GPU_ADLN_Steering.Topology := Intel_GPU_ADLN_Steering.Decode (1, 8, 0);
   function Owner return Boolean is (Owned);
   function Topology return Intel_GPU_ADLN_Steering.Topology is (T);
   package Reader is new Intel_GPU_Native_Context_Read (Owner, Topology);
   function Mmap (Address : System.Address; Length : Interfaces.C.size_t;
                  Protection, Flags, FD : Interfaces.C.int;
                  Offset : Interfaces.C.long) return System.Address
     with Import, Convention => C, External_Name => "mmap";
   function Munmap (Address : System.Address; Length : Interfaces.C.size_t)
     return Interfaces.C.int with Import, Convention => C, External_Name => "munmap";
   use type Interfaces.C.int;
   Selector_Page : constant Integer_Address := 16#6120B000#;
   Target_Page : constant Integer_Address := 16#60005000#;
   Selector : Unsigned_32 with Import, Volatile_Full_Access,
     Address => To_Address (Selector_Page + 16#FDC#);
   WM : Unsigned_32 with Import, Volatile_Full_Access,
     Address => To_Address (Target_Page + 16#584#);
   procedure Reserve (At_Address : Integer_Address) is
      Mapping : constant System.Address :=
        Mmap (To_Address (At_Address), 4096, 3, 16#100022#, -1, 0);
   begin
      if Mapping /= To_Address (At_Address) then
         if Mapping /= To_Address (Integer_Address'Last) then
            declare Ignored : constant Interfaces.C.int := Munmap (Mapping, 4096); begin null; end;
         end if;
         raise Program_Error with "cannot reserve isolated context MMIO fixture";
      end if;
   end Reserve;
begin
   pragma Assert (Reader.Read_WM_Chicken2 = Unsigned_32'Last);
   Owned := True; T.Valid := False;
   pragma Assert (Reader.Read_WM_Chicken2 = Unsigned_32'Last);
   T.Valid := True; T.DSS_Mask := 1; -- default instance3 is not enabled
   pragma Assert (Reader.Read_WM_Chicken2 = Unsigned_32'Last);
   T := Intel_GPU_ADLN_Steering.Decode (1, 8, 0);
   Reserve (Selector_Page); Reserve (Target_Page);
   Selector := 16#12000042#; WM := 16#12345678#;
   pragma Assert (Reader.Read_WM_Chicken2 = 16#12345678#);
   pragma Assert (Selector = 16#12000042# and WM = 16#12345678#);
   WM := Unsigned_32'Last;
   pragma Assert (Reader.Read_WM_Chicken2 = Unsigned_32'Last);
   pragma Assert (Selector = 16#12000042#);
   WM := 1;
   pragma Assert (Reader.Read_WM_Chicken2 = Unsigned_32'Last); -- quarantined
   pragma Assert (Munmap (To_Address (Selector_Page), 4096) = 0);
   pragma Assert (Munmap (To_Address (Target_Page), 4096) = 0);
   Ada.Text_IO.Put_Line ("Native context read PASS (host MMIO fixture, NOT hardware)");
end Native_Context_Read_Tests;
