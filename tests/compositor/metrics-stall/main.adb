with Interfaces; use Interfaces;
with System;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Metric_Records;
with CuBit.Metric_Protocol;
-- Test-only collector: holds its first real acquisition, then resumes.
procedure Main is
   package P renames CuBit.Metric_Protocol;
   package R renames CuBit.Metric_Records;
   package G renames CuBit.Memory_Grants;
   From : Process_ID;
   Request, Response : Message;
   Ignore, Batches : Unsigned_64 := 0;
   Ref : G.Grant_Reference;
   Address : System.Address;
   Acquired, Returned : Boolean;
   Snapshot : R.Page_Words := [others => 0];
   Header : R.Decoded_Header;
   procedure Fail (Reason : String) is
   begin
      debugPrint ("TEST: FAIL metrics-stall " & Reason & ASCII.LF);
      Ignore := syscall (SYSCALL_EXIT, 1);
   end Fail;
begin
   Ignore := registerDriver (P.Publisher_Service_Role);
   if Ignore = Unsigned_64'Last then Fail ("registration"); return; end if;
   loop
      receive (From, Request);
      if Request.tag.label /= P.Operation'Enum_Rep (P.Publish_Batch) or else
        not P.Is_Publisher (Request.authorityTag) or else
        From /= Process_ID (getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_DESKTOP)) or else
        Request.words (2) not in 128 .. R.Page_Bytes or else
        Request.words (2) mod R.Slot_Bytes /= 0
      then Fail ("unexpected request"); return; end if;
      Ref := (Request.words (0), Request.words (1));
      G.Acquire (Ref, From, 0, Request.words (2), G.Read_Access, Address, Acquired);
      if not Acquired then Fail ("acquire"); return; end if;
      Batches := Batches + 1;
      declare
         Shared : R.Page_Words with Import, Volatile, Address => Address;
         Used : constant R.Page_Word_Index := Natural (Request.words (2)) / 8 - 1;
      begin
         Snapshot (0 .. Used) := Shared (0 .. Used);
         Header := R.Decode_Header (R.Slot (Snapshot, 0), Request.words (2));
         if not Header.Success then Fail ("header"); return; end if;
         if Batches = 1 then
            debugPrint ("TEST: metrics-stall grant held" & ASCII.LF);
            for Tick in 1 .. 600 loop
               Ignore := syscall (SYSCALL_SLEEP, 50);
               for I in 0 .. Used loop
                  if Shared (I) /= Snapshot (I) then
                     Fail ("held page mutated"); return;
                  end if;
               end loop;
            end loop;
            debugPrint ("TEST: metrics-stall held page unchanged checks=600" & ASCII.LF);
         end if;
      end;
      G.Return_Acquisition (Ref, Returned);
      if not Returned then Fail ("return"); return; end if;
      Response := NULL_MESSAGE;
      Response.tag := (P.Status'Enum_Rep (P.OK), P.Message_Words, 0, 0);
      Response.words := [Unsigned_64 (Header.Value.Records), 0, 0, 0];
      Ignore := reply (From, Response);
      if Batches > 2 and Header.Value.Producer_Dropped > 0 then
         debugPrint ("TEST: PASS metrics-stall resumed batches=" & Batches'Image &
           " dropped=" & Header.Value.Producer_Dropped'Image & ASCII.LF);
      end if;
   end loop;
end Main;
