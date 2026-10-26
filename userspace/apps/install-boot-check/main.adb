------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The installed system's boot check (SYS-001, tests/headless
--  install-reboot), started from the installed root. It reads the
--  persistent log (@ufs:0/logstore.log, which logstore appends to), counts
--  the boots recorded there and the markers earlier boots of this check
--  published, then publishes this boot's marker through logstore, so the
--  next boot finds it in the file.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Filesystems; use CuBit.Filesystems;
with CuBit.Logging;
with CuBit.Log_Records;
with Startup;

procedure Main is
   Log_Path : constant String := "@ufs:0/logstore.log";
   Marker : constant String := "install-boot-check: boot marker";
   Boot_Header : constant String := "== logstore: boot";
   Loan_Pages : constant := 16;
   Loan_Bytes : constant := Loan_Pages * CuBit.Memory_Grants.PAGE_SIZE;
   --  How long the log may take to appear: logstore opens it once the
   --  root attaches and it has records to write.
   Wait_Milliseconds : constant := 20_000;
   Poll_Milliseconds : constant := 250;

   Writer : CuBit.Logging.Publisher := CuBit.Logging.Bound_To (Startup.Logstore);
   Loan : CuBit.Memory_Grants.Grant_Reference;
   Buffer : System.Address;
   Granted : Boolean;
   Raw : Unsigned_64;
   Handle : File_Handle := 0;
   Request : Message;
   Offset : Unsigned_64 := 0;
   Boots, Markers : Natural := 0;
   Opened : Boolean := False;
   Started : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
   Ignore : Unsigned_64;

   function Call (Item : in out Message) return Boolean is
   begin
      Item.tag := capCall (Startup.Filesystem, Item, Wait_Forever);
      return Item.tag.label = REPLY_OK;
   end Call;

   function Count (Text, Pattern : String) return Natural is
      Result : Natural := 0;
   begin
      if Text'Length < Pattern'Length then
         return 0;
      end if;
      for I in Text'First .. Text'Last - Pattern'Length + 1 loop
         if Text (I .. I + Pattern'Length - 1) = Pattern then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Count;

   procedure Emit (Text : String) is
      Made : constant CuBit.Log_Records.Decoded := CuBit.Log_Records.Make (Text);
      Submitted : Boolean;
   begin
      debugPrint (Text & ASCII.LF);
      if Made.Success then
         CuBit.Logging.Emit (Writer, Made.Value, Submitted);
      end if;
   end Emit;
begin
   Raw := syscall (SYSCALL_SBRK, (Loan_Pages + 1) * CuBit.Memory_Grants.PAGE_SIZE);
   Buffer := To_Address (Integer_Address
     ((Raw + CuBit.Memory_Grants.PAGE_SIZE - 1) and not (CuBit.Memory_Grants.PAGE_SIZE - 1)));
   CuBit.Memory_Grants.Create_Via_Capability (Startup.Filesystem, Buffer, Loan_Pages, True, Loan, Granted);
   while Granted and then not Opened loop
      declare
         Path : String (1 .. Log_Path'Length) with Import, Address => Buffer;
      begin
         Path := Log_Path;
      end;
      Request := Open_Request (Loan, Log_Path'Length);
      Opened := Call (Request);
      exit when Opened or else syscall (SYSCALL_GETTIME) - Started > Wait_Milliseconds;
      Ignore := syscall (SYSCALL_SLEEP, Poll_Milliseconds);
   end loop;
   if not Opened then
      Emit ("install-boot-check: the persistent log is not readable FAIL");
      return;
   end if;
   Handle := File_Handle (Request.words (0));
   --  Count across chunks; a pattern split by a chunk boundary is missed,
   --  which only undercounts (the check needs at least one).
   loop
      Request := Read_At_Request (Handle, Loan, Loan_Bytes, Offset);
      exit when not Call (Request) or else Request.words (0) = 0;
      declare
         Got : constant Natural := Natural (Unsigned_64'Min (Request.words (0), Loan_Bytes));
         Text : String (1 .. Got) with Import, Address => Buffer;
      begin
         Boots := Boots + Count (Text, Boot_Header);
         Markers := Markers + Count (Text, Marker);
         Offset := Offset + Unsigned_64 (Got);
      end;
   end loop;
   Request := Close_Request (Handle);
   if not Call (Request) then
      null;
   end if;
   Emit ("install-boot-check: persistent log" & Offset'Image & " bytes," & Boots'Image &
         " boots," & Markers'Image & " markers from earlier boots");
   if Markers > 0 then
      Emit ("install-boot-check: an earlier boot's records are readable PASS");
   else
      Emit ("install-boot-check: first boot of the installed system");
   end if;
   Emit (Marker);
end Main;
