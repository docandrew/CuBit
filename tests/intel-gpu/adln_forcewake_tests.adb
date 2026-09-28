with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Inventory; use Intel_GPU_ADLN_Inventory;
with Intel_GPU_ADLN_Forcewake;
with Ada.Text_IO;
procedure ADLN_Forcewake_Tests is
   procedure Run (Fuse : Unsigned_32; Fail_Acquire, Fail_Release : Natural) is
      type Counts is array (Domain) of Natural;
      Sets, Clears : Counts := [others => 0];
      Ack : array (Domain) of Unsigned_32 := [others => 0];
      Clock : Unsigned_64 := 0;
      function Number (D : Domain) return Natural is (Domain'Pos (D) + 1);
      function Read_32 (Offset : Unsigned_32) return Unsigned_32 is
      begin
         for D in Domain loop
            if Offset = Ack_Register (D) then return Ack (D); end if;
         end loop;
         raise Program_Error;
      end Read_32;
      procedure Write_32 (Offset, Value : Unsigned_32) is
      begin
         for D in Domain loop
            if Offset = Request_Register (D) then
               if Value = 16#10001# then
                  Sets (D) := Sets (D) + 1;
                  if Number (D) /= Fail_Acquire then Ack (D) := 1; end if;
               else
                  pragma Assert (Value = 16#10000#);
                  Clears (D) := Clears (D) + 1;
                  if Number (D) /= Fail_Release then Ack (D) := 0; end if;
               end if;
               return;
            end if;
         end loop;
         raise Program_Error;
      end Write_32;
      procedure Pause is
      begin Clock := Clock + 1; end Pause;
      function Now return Unsigned_64 is (Clock);
      package FW is new Intel_GPU_ADLN_Forcewake (Read_32, Write_32, Pause, Now);
      use type FW.Ownership_State;
      Required : constant Domain_Set := Decode (16#8086#, 16#46D2#, Fuse).Domains;
      OK : Boolean;
      Failed : Natural := 0;
   begin
      FW.Acquire (0, 16#46D2#, Fuse, OK);
      pragma Assert (not OK and Sets = Counts'[others => 0] and FW.State = FW.Idle);
      FW.Acquire (16#8086#, 16#46D2#, Unsigned_32'Last, OK);
      pragma Assert (not OK and Sets = Counts'[others => 0]);
      FW.Acquire (16#8086#, 16#46D2#, Fuse, OK);
      for D in Domain loop
         if Required (D) and Failed = 0 then
            pragma Assert (Sets (D) = 1);
            if Number (D) = Fail_Acquire then Failed := Number (D); end if;
         else pragma Assert (Sets (D) = 0); end if;
      end loop;
      pragma Assert (OK = (Failed = 0));
      if OK then
         pragma Assert (FW.State = FW.Held);
         FW.Release (OK);
         pragma Assert (OK = (Fail_Release = 0 or else
           not Required (Domain'Val (Fail_Release - 1))));
         pragma Assert (FW.State = (if OK then FW.Idle else FW.Faulted));
      else pragma Assert (FW.State = FW.Faulted); end if;
      for D in Domain loop
         pragma Assert (Clears (D) =
           (if Required (D) and (Failed = 0 or Number (D) <= Failed) then 1 else 0));
         pragma Assert (FW.Uncertain (D) =
           (Required (D) and (Number (D) = Failed or
             (Number (D) = Fail_Release and (Failed = 0 or Number (D) < Failed)))));
      end loop;
      declare
         Before : constant Counts := Clears;
      begin
         FW.Release (OK);
         pragma Assert (not OK and Clears = Before);
      end;
   end Run;
begin
   for Mask in Unsigned_32 range 0 .. 7 loop
      for A in 0 .. 5 loop
         for R in 0 .. 5 loop
            Run ((Mask and 1) or Shift_Left (Mask and 2, 1) or
                 Shift_Left (Mask and 4, 14), A, R);
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("PASS: 288 combined ADL-N forcewake failure cases");
end ADLN_Forcewake_Tests;
