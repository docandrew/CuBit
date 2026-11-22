with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Inventory; use Intel_GPU_ADLN_Inventory;
with Intel_GPU_Handoff;
with Ada.Text_IO;
procedure Handoff_Tests is
   procedure Run (Fuse : Unsigned_32; Stop_Failure, Prep_Failure, Cleanup_Failure : Natural;
                  Power_OK, Reset_OK : Boolean) is
      Required : constant Engine_Set := Decode (16#8086#, 16#46D2#, Fuse).Engines;
      Stopped, Prepared, Cancelled : Engine_Set := [others => False];
      Holds, Resets, Calls : Natural := 0;
      function Number (E : Engine) return Natural is (Engine'Pos (E) + 1);
      procedure Hold (OK : out Boolean) is
      begin Holds := Holds + 1; Calls := Calls + 1; OK := Power_OK; end;
      procedure Stop (E : Engine; OK : out Boolean) is
      begin
         pragma Assert (Holds = 1 and Power_OK and Required (E));
         pragma Assert (Prepared = Engine_Set'[others => False]);
         Stopped (E) := True; Calls := Calls + 1; OK := Number (E) /= Stop_Failure;
      end;
      procedure Prepare (E : Engine; OK : out Boolean) is
      begin
         pragma Assert (Stopped = Required and Required (E));
         Prepared (E) := True; Calls := Calls + 1; OK := Number (E) /= Prep_Failure;
      end;
      procedure Reset (OK : out Boolean) is
      begin
         pragma Assert (Prepared = Required and Stopped = Required);
         Resets := Resets + 1; Calls := Calls + 1; OK := Reset_OK;
      end;
      procedure Cancel (E : Engine; OK : out Boolean) is
      begin
         pragma Assert (Required (E) and not Cancelled (E));
         Cancelled (E) := True; Calls := Calls + 1; OK := Number (E) /= Cleanup_Failure;
      end;
      package Handoff is new Intel_GPU_Handoff (Hold, Stop, Prepare, Reset, Cancel);
      use type Handoff.Result;
      use type Handoff.Phase;
      Object : Handoff.Attempt;
      Status, Expected : Handoff.Result;
      function Hits (N : Natural) return Boolean is
        (N /= 0 and then Required (Engine'Val (N - 1)));
      Before : Natural;
   begin
      Handoff.Execute (Object, 0, 16#46D2#, Fuse, Status);
      pragma Assert (Status = Handoff.Rejected and Calls = 0);
      Handoff.Execute (Object, 16#8086#, 16#46D2#, Fuse, Status);
      if not Power_OK then Expected := Handoff.Forcewake_Failed;
      elsif Hits (Stop_Failure) then Expected := Handoff.Stop_Failed;
      elsif Hits (Cleanup_Failure) then Expected := Handoff.Cleanup_Failed;
      elsif Hits (Prep_Failure) then Expected := Handoff.Prepare_Failed;
      elsif not Reset_OK then Expected := Handoff.Reset_Failed;
      else Expected := Handoff.Complete; end if;
      pragma Assert (Status = Expected);
      pragma Assert (Resets = (if Power_OK and not Hits (Stop_Failure) and
                      not Hits (Prep_Failure) then 1 else 0));
      pragma Assert (Cancelled =
        (if Power_OK and not Hits (Stop_Failure) then Required else Engine_Set'[others => False]));
      pragma Assert (Handoff.State (Object) =
        (if Status = Handoff.Complete then Handoff.Reset_Held else Handoff.Quarantined));
      Before := Calls;
      Handoff.Execute (Object, 16#8086#, 16#46D2#, Fuse, Status);
      pragma Assert (Status = Handoff.Rejected and Calls = Before);
   end Run;
begin
   for Mask in Unsigned_32 range 0 .. 7 loop
      for S in 0 .. 5 loop
         for P in 0 .. 5 loop
            for C in 0 .. 5 loop
               for Power in Boolean loop
                  for Reset in Boolean loop
                     Run ((Mask and 1) or Shift_Left (Mask and 2, 1) or
                          Shift_Left (Mask and 4, 14), S, P, C, Power, Reset);
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("PASS: 6912 handoff selection/failure combinations");
end Handoff_Tests;
