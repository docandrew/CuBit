with Interfaces; use Interfaces;
with System;
with Client_Frame_Pair;
with CuBit.Desktop_Protocol;
with CuBit.Desktop_Messages;
with CuBit.Messages; use CuBit.Messages;
procedure Desktop_Frame_Pair (Passed : out Boolean) is
   package P renames Client_Frame_Pair;
   package DP renames CuBit.Desktop_Protocol;
   use type DP.Status_Code, P.Debt.Box, System.Address;
   O : P.Owner;
   Created : DP.Creation_Result;
   Config : P.Pub.Configuration_Result;
   Full, Repair, Patch : P.Debt.Box;
   OK : Boolean;
   Limit : Natural;
   function Send (Wire : DP.Wire_Message) return DP.Wire_Message is
      M : Message := CuBit.Desktop_Messages.From_Wire (Wire);
   begin
      M.tag := capCall (CAP_SLOT_DESKTOP, M);
      return CuBit.Desktop_Messages.To_Wire (M);
   end Send;
   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Passed := False;
         debugPrint ("TEST: FAIL frame pair " & Name & ASCII.LF);
      end if;
   end Check;
begin
   Passed := True;
   Created := DP.Decode_Creation_Result (Send (DP.Encode_Create ((320, 200, DP.Window_Surface))));
   Check (Created.Status = DP.Success, "create");
   if Created.Status /= DP.Success then return; end if;
   for Frame in 1 .. 12 loop
      if Frame = 7 then
         Check (DP.Decode_Resize_Result (Send (DP.Encode_Resize ((Created.Surface, 400, 280)))).Status = DP.Success,
                "resize");
      end if;
      P.Configure (O, Created.Surface, OK);
      Check (OK, "configuration");
      if not OK then exit; end if;
      Config := P.Configuration (O);
      Full := (0, 0, Natural (Config.Value.Width), Natural (Config.Value.Height));
      Patch := (7, 9, 20, 20);
      Limit := ((Natural (DP.Byte_Length (Config.Value.Layout)) - 1) / 4096 + 1) * 4096;
      Check (P.Address (O) = System.Null_Address, "no pointer outside paint");
      P.Begin_Paint (O, (if Frame in 1 | 7 then Full else Patch), Repair, OK);
      Check (OK, "candidate writable after exact retirement");
      if not OK then exit; end if;
      Check (P.Allocated_Bytes (O) <= 2 * Limit, "two-buffer allocation bound");
      if Frame in 1 | 2 | 7 | 8 then
         Check (Repair = Full, "new configuration repairs both buffers");
      else
         Check (Repair = Patch, "repair only stale patch");
      end if;
      if Frame = 4 then
         P.Cancel_Paint (O);
         Check (P.Address (O) = System.Null_Address and P.Pending (O), "cancel retains debt");
         P.Begin_Paint (O, P.Debt.Empty, Repair, OK);
         Check (OK and Repair = Patch, "cancel retry");
      end if;
      if Frame = 5 then
         P.Publish (O, (7, 9, 8, 10), OK);
         Check (not OK and P.Pending (O) and P.Address (O) = System.Null_Address,
                "incomplete repair cannot publish");
         P.Begin_Paint (O, P.Debt.Empty, Repair, OK);
         Check (OK and Repair = Patch, "rejected repair retry");
      end if;
      declare
         Pixels : array (Natural range 0 .. Limit / 4 - 1) of Unsigned_32
           with Import, Address => P.Address (O);
      begin
         -- Test fixture is unit scale. Fractional toolkit pixels are exercised
         -- separately by Desktop_Density_Text until App configuration adoption.
         Check (Config.Value.Numerator = Config.Value.Denominator, "unit fixture");
         for Y in Repair.Top .. Repair.Bottom - 1 loop
            for X in Repair.Left .. Repair.Right - 1 loop
               Pixels (Y * (Config.Value.Layout.Pitch / 4) + X) :=
                 (if X in 7 .. 19 and Y in 9 .. 19 then Unsigned_32 (Frame) else 16#2468AC#);
            end loop;
         end loop;
         for Y in 0 .. Full.Bottom - 1 loop
            for X in 0 .. Full.Right - 1 loop
               Check (Pixels (Y * (Config.Value.Layout.Pitch / 4) + X) =
                 (if X in 7 .. 19 and Y in 9 .. 19 then Unsigned_32 (Frame) else 16#2468AC#),
                  "retained pixels after repair");
            end loop;
         end loop;
      end;
      P.Publish (O, Repair, OK);
      Check (OK and not P.Pending (O) and P.Address (O) = System.Null_Address, "publish withdraws pointer");
   end loop;
   P.Close (O, OK);
   Check (not OK and P.Allocated_Bytes (O) > 0, "visible loan prevents close reclamation");
   P.Configure (O, Created.Surface, OK);
   Check (not OK, "closing owner cannot resume rendering");
   Check (DP.Decode_Status (Send (DP.Encode_Destroy ((Surface => Created.Surface))), DP.Destroy_Surface) = DP.Success,
          "destroy");
   P.Close (O, OK);
   Check (OK and P.Allocated_Bytes (O) = 0, "confirmed retirement frees both allocations");
   P.Close (O, OK);
   Check (OK, "idempotent close");
end Desktop_Frame_Pair;
