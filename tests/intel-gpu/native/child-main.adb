with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Grant_References;
procedure Main is
   reterr : constant Unsigned_64 := Unsigned_64'Last;
   Source : ProcessID;
   Msg : Message;
   Result : Unsigned_64;
   Observed : Unsigned_64;
   Alias_Value : Unsigned_64 with Import, Volatile,
     Address => To_Address (16#5100_0000#);
begin
   receive (Source, Msg);
   if Msg.tag.label = 16#4D51# or else Msg.tag.label = 16#4D53# then
      debugPrint ("dma-retention: child received request" & ASCII.LF);
      declare
         Ref : CuBit.Memory_Grants.Grant_Reference;
         OK : Boolean;
         Large : constant Boolean := Msg.tag.label = 16#4D53#;
         Grant_Address : constant Integer_Address :=
           (if Large then 16#761F_F000# else 16#7600_0000#);
         Value : Unsigned_64 with Import, Volatile,
           Address => To_Address (Grant_Address);
      begin
         if Large then
            -- Large-page fixture owns sixteen contiguous CPU extents backed
            -- by independent order9 blocks. Check every 4 KiB offset twice,
            -- so aliased backing cannot pass a write/immediate-read test.
            for Page in 0 .. 8191 loop
               declare
                  Word : Unsigned_64 with Import, Volatile,
                    Address => To_Address (16#7600_0000# +
                      Integer_Address (Page) * 4096);
               begin
                  Word := 16#D0A0_0000# + Unsigned_64 (Page);
               end;
            end loop;
            for Page in 0 .. 8191 loop
               declare
                  Word : Unsigned_64 with Import, Volatile,
                    Address => To_Address (16#7600_0000# +
                      Integer_Address (Page) * 4096);
               begin
                  if Word /= 16#D0A0_0000# + Unsigned_64 (Page) then
                     debugPrint ("dma-retention: FAIL large CPU backing" & ASCII.LF);
                     return;
                  end if;
               end;
            end loop;
            debugPrint ("dma-retention: large CPU8192 PASS" & ASCII.LF);
         end if;
         Value := 16#D0A0_1234#;
         CuBit.Memory_Grants.Create_Via_Capability
           (5, To_Address (Grant_Address), 1, False, Ref, OK);
         Msg := NULL_MESSAGE;
         Msg.tag := (16#4D52#, 1, 0, 0);
         if OK then Msg.words (0) := CuBit.Grant_References.Encode (Ref); end if;
         if not capSubmit (5, Msg, NO_COMPLETION_TOKEN) then
            debugPrint ("dma-retention: child reply rejected" & ASCII.LF);
         end if;
         loop
            receive (Source, Msg);
            if Large and then Msg.tag.label = 16#4D55# then
               declare
                  Self_Ref : CuBit.Memory_Grants.Grant_Reference;
                  Mapped : System.Address;
               begin
                  CuBit.Memory_Grants.Create_Via_Capability
                    (6, To_Address (Grant_Address), 1, False, Self_Ref, OK);
                  if not OK then
                     debugPrint ("dma-retention: FAIL readonly self grant" & ASCII.LF); return;
                  end if;
                  CuBit.Memory_Grants.Acquire
                    (Self_Ref, ProcessID (syscall (SYSCALL_GETPID)), 0, 8,
                     CuBit.Memory_Grants.Read_Access, Mapped, OK);
                  if not OK then
                     debugPrint ("dma-retention: FAIL readonly self acquire" & ASCII.LF); return;
                  end if;
                  declare
                     Read_Only : Unsigned_64 with Import, Volatile, Address => Mapped;
                  begin
                     if Read_Only /= 16#D0A0_1234# then
                        debugPrint ("dma-retention: FAIL readonly self value" & ASCII.LF); return;
                     end if;
                     debugPrint ("dma-retention: readonly large alias read PASS; attempting write" & ASCII.LF);
                     Read_Only := 0;
                     debugPrint ("dma-retention: FAIL readonly large alias write returned" & ASCII.LF);
                     return;
                  end;
               end;
            end if;
            if Large and then Msg.tag.label = 16#4D54# then
               CuBit.Memory_Grants.Revoke (Ref, OK);
               if not OK or else not CuBit.Memory_Grants.Retirement_Confirmed (Ref) then
                  debugPrint ("dma-retention: FAIL live retirement" & ASCII.LF); return;
               end if;
               for Page in 0 .. 8191 loop
                  declare
                     Word : Unsigned_64 with Import, Volatile,
                       Address => To_Address (16#7600_0000# + Integer_Address (Page) * 4096);
                     Expected : constant Unsigned_64 :=
                       (if Page = 511 then 16#D0A0_1234# else 16#D0A0_0000# + Unsigned_64 (Page));
                  begin
                     if Word /= Expected then
                        debugPrint ("dma-retention: FAIL owner mapping after revoke" & ASCII.LF); return;
                     end if;
                  end;
               end loop;
               debugPrint ("dma-retention: live revoke owner8192 intact PASS" & ASCII.LF);
               CuBit.Memory_Grants.Create_Via_Capability
                 (5, To_Address (Grant_Address), 1, False, Ref, OK);
               Msg := NULL_MESSAGE;
               Msg.tag := (16#4D52#, 1, 0, 0);
               if OK then Msg.words (0) := CuBit.Grant_References.Encode (Ref); end if;
               if not capSubmit (5, Msg, NO_COMPLETION_TOKEN) then return; end if;
            end if;
         end loop;
      end;
   end if;
   if Msg.tag.label /= 16#4D50# then return; end if;
   -- Fixture is granted READ only on exactly this RAM page.
   Result := syscall (SYSCALL_MAP_DEVICE, Msg.words (0), 16#5100_0000#, 1, 0);
   if Result /= reterr then
      debugPrint ("map-check: FAIL writable request admitted" & ASCII.LF); return;
   end if;
   Result := syscall (SYSCALL_MAP_DEVICE, Msg.words (0), 16#5100_0000#, 1, 2);
   if Result /= reterr then
      debugPrint ("map-check: FAIL unknown access admitted" & ASCII.LF); return;
   end if;
   Result := syscall (SYSCALL_MAP_DEVICE, Msg.words (0), 16#5100_0000#, 1, 1);
   if Result /= 0 then
      debugPrint ("map-check: FAIL readonly mapping rejected" & ASCII.LF); return;
   end if;
   Observed := Alias_Value;
   debugPrint ("map-check: observed" & Unsigned_64'Image (Observed) & ASCII.LF);
   debugPrint ("map-check: read PASS; attempting forbidden write at 51000000" & ASCII.LF);
   Alias_Value := 0;
   debugPrint ("map-check: FAIL forbidden write returned" & ASCII.LF);
end Main;
