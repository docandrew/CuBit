with Interfaces; use Interfaces;
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
   if Msg.tag.label = 16#4D51# then
      debugPrint ("dma-retention: child received request" & ASCII.LF);
      declare
         Ref : CuBit.Memory_Grants.Grant_Reference;
         OK : Boolean;
         Value : Unsigned_64 with Import, Volatile,
           Address => To_Address (16#7600_0000#);
      begin
         Value := 16#D0A0_1234#;
         CuBit.Memory_Grants.Create_Via_Capability
           (5, To_Address (16#7600_0000#), 1, False, Ref, OK);
         Msg := NULL_MESSAGE;
         Msg.tag := (16#4D52#, 1, 0, 0);
         if OK then Msg.words (0) := CuBit.Grant_References.Encode (Ref); end if;
         if not capSubmit (5, Msg, NO_COMPLETION_TOKEN) then
            debugPrint ("dma-retention: child reply rejected" & ASCII.LF);
         end if;
         loop receive (Source, Msg); end loop;
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
