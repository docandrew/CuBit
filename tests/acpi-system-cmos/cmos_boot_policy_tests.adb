pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with Devmgr_Port_Claims;
with ACPI_CMOS_Handover_Protocol;
with CCL.Setting_Codecs.ACPI_Firmware_Profile;

procedure CMOS_Boot_Policy_Tests is
   package PC renames Devmgr_Port_Claims;
   package W renames ACPI_CMOS_Handover_Protocol;
   package C renames CCL.Setting_Codecs.ACPI_Firmware_Profile;
   use type PC.CMOS_Handover_Result, PC.Take_Result, PC.Holder, PC.Claim, PC.Claim_Array;
   use type Interfaces.Unsigned_32, Interfaces.Unsigned_64;
   use type W.Receipt, C.ACPI_Firmware_Profile_Value, C.CMOS_Profile_Value;
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin
      if not Good then raise Program_Error with "CMOS boot policy regression"; end if;
      Checks := Checks + 1;
   end Check;
begin
   for Extended in Boolean loop
      for Aliases in Boolean loop
         for First in 0 .. 255 loop
            for Size in 1 .. 8 loop
               for Holder in PC.Holder loop
                  declare
                     Claims : PC.Claim_Array (1 .. 16);
                     Used, Original_Count : Natural;
                     Result : PC.CMOS_Handover_Result;
                     Taken : PC.Take_Result;
                     Grant_End : constant Natural := (if Extended then 16#74# else 16#72#);
                     Reserve_End : constant Natural := (if Aliases then 16#78# else 16#74#);
                  begin
                     PC.Initialize (Claims, Used);
                     Used := Used + 1;
                     Claims (Used) := (Interfaces.Unsigned_32 (First), Interfaces.Unsigned_32 (First + Size),
                       Holder, (if Holder = PC.Driver then 1 else PC.No_Owner));
                     Original_Count := Used;
                     declare Before : constant PC.Claim_Array := Claims; begin
                        PC.Handover_CMOS (Claims, Used, Extended, Aliases, Result);
                        if First < Reserve_End and then 16#70# < First + Size then
                           Check (Result = PC.Handover_Refused and Used = Original_Count and Claims = Before);
                        else
                           Check (Result = PC.Transferred);
                           Check (Used = Original_Count + (if Grant_End < Reserve_End then 1 else 0));
                           Check (Claims (3) = (16#70#, Interfaces.Unsigned_32 (Grant_End), PC.ACPI, PC.No_Owner));
                           for K in 1 .. Original_Count loop
                              if K /= 3 then Check (Claims (K) = Before (K)); end if;
                           end loop;
                           declare After : constant PC.Claim_Array := Claims; begin
                              PC.Handover_CMOS (Claims, Used, Extended, Aliases, Result);
                              Check (Result = PC.Already_Held and Claims = After);
                           end;
                           if Grant_End < Reserve_End then
                              for Port in Grant_End .. Reserve_End - 1 loop
                                 for By in PC.Holder range PC.Device_Manager .. PC.ACPI loop
                                    PC.Take (Claims, Used, Interfaces.Unsigned_64 (Port), 1, By,
                                      (if By = PC.Driver then 1 else PC.No_Owner), Taken);
                                    Check (Taken = PC.Refused);
                                 end loop;
                              end loop;
                           end if;
                        end if;
                     end;
                  end;
               end loop;
            end loop;
         end loop;
         declare
            Small : PC.Claim_Array (1 .. PC.Seed_Claims);
            Used : Natural;
            Result : PC.CMOS_Handover_Result;
         begin
            PC.Initialize (Small, Used);
            declare Before : constant PC.Claim_Array := Small; begin
               PC.Handover_CMOS (Small, Used, Extended, Aliases, Result);
               if not Extended or Aliases then
                  Check (Result = PC.Handover_Needs_Room and Small = Before and Used = PC.Seed_Claims);
               else Check (Result = PC.Transferred); end if;
            end;
         end;
      end loop;
   end loop;

   for Profile in W.Profile range W.Legacy_64 .. W.Intel_256 loop
      for NMI in Boolean loop
         declare
            Receipt : constant W.Receipt := (Profile, NMI);
            Frame : constant W.Words := W.Encode (Receipt);
            Decoded : constant W.Decoded := W.Decode (Frame);
         begin
            Check (Decoded.Valid and then Decoded.Value = Receipt);
            for At_Word in Frame'Range loop
               for Bit in 0 .. 63 loop
                  declare
                     Mutated : W.Words := Frame;
                  begin
                     Mutated (At_Word) := Mutated (At_Word) xor Interfaces.Shift_Left (W.Word'(1), Bit);
                     declare Result : constant W.Decoded := W.Decode (Mutated); begin
                        -- The NMI bit is a semantic field, not an integrity tag.
                        if At_Word = 2 and Bit = 1 then
                           Check (Result.Valid and then Result.Value.NMI_Disabled /= NMI);
                        else Check (not Result.Valid); end if;
                     end;
                  end;
               end loop;
            end loop;
         end;
      end loop;
   end loop;

   for Kind in C.ACPI_Profile_Value loop
      for User_Profile in C.CMOS_Profile_Value loop
         for User_Known in Boolean loop
            for User_NMI in Boolean loop
               declare User : constant C.ACPI_Firmware_Profile_Value := (Kind, User_Profile, User_Known, User_NMI); begin
                  for Trusted in C.CMOS_Profile_Value loop
                     for Known in Boolean loop
                        for NMI in Boolean loop
                           for Approved in Boolean loop
                              declare
                                 Value : constant C.ACPI_Firmware_Profile_Value := C.Bind_CMOS (User, Trusted, Known, NMI, Approved);
                                 Other : constant C.ACPI_Firmware_Profile_Value := C.Bind_CMOS
                                   ((Kind, C.Intel_256, not User_Known, not User_NMI), Trusted, Known, NMI, Approved);
                                 Decoded : C.ACPI_Firmware_Profile_Value;
                                 Good : Boolean;
                              begin
                                 Check (Value = Other);
                                 C.Decode (C.Encode (Value), Decoded, Good);
                                 Check (Good and Decoded = Value);
                                 Check (Value.cmos_known = (Approved and Known and Trusted /= C.No_CMOS));
                                 Check (Value.cmos_nmi_disabled = (Value.cmos_known and NMI));
                              end;
                           end loop;
                        end loop;
                     end loop;
                  end loop;
               end;
            end loop;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("CMOS boot policy checks " & Checks'Image & " PASS");
end CMOS_Boot_Policy_Tests;
