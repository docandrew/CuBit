------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Filesystems; use CuBit.Filesystems;
with CuBit.Log_Protocol;
with CuBit.Log_Records;
with Startup;

package body Log_Persistence is

   package Logs renames CuBit.Log_Records;

   File_Name : constant String := "logstore.log";
   Maximum_Volume : constant := 32;
   Loan_Pages : constant := 4;
   Loan_Bytes : constant := Loan_Pages * CuBit.Memory_Grants.PAGE_SIZE;
   --  A line: time, source, severity, text (one record's text is bounded
   --  by the log protocol), newline.
   Maximum_Line : constant := 1024;
   Line_Feed : constant Character := ASCII.LF;

   Volume_Name : String (1 .. Maximum_Volume) := [others => ' '];
   Volume_Length : Natural := 0;

   Loan : CuBit.Memory_Grants.Grant_Reference;
   Buffer : System.Address := System.Null_Address;
   Loan_Ready : Boolean := False;
   Handle : File_Handle := 0;
   Opened : Boolean := False;
   Last_Attempt, Last_Flush : Unsigned_64 := 0;
   Attempted : Boolean := False;
   Unflushed : Boolean := False;
   Failed : Boolean := False;

   procedure Configure (Volume : String) is
   begin
      Volume_Length := Natural'Min (Volume'Length, Maximum_Volume);
      Volume_Name (1 .. Volume_Length) := Volume (Volume'First .. Volume'First + Volume_Length - 1);
   end Configure;

   function Active return Boolean is (Opened);

   function Call (Request : in out Message) return Boolean is
   begin
      Request.tag := capCall (Startup.Filesystem, Request, Wait_Forever);
      return Request.tag.label = REPLY_OK;
   end Call;

   function Decimal (Value : Unsigned_64) return String is
      Image : constant String := Unsigned_64'Image (Value);
   begin
      return Image (Image'First + 1 .. Image'Last);
   end Decimal;

   function Severity_Text (Item : Logs.Severity) return String is
     (case Item is
        when Logs.Trace => "trace", when Logs.Debug => "debug",
        when Logs.Information => "info", when Logs.Warning => "warning",
        when Logs.Error => "error", when Logs.Critical => "critical");

   procedure Try_Open (Now_Ms : Unsigned_64) is
      Path : constant String := "@" & Volume_Name (1 .. Volume_Length) & "/" & File_Name;
      Request : Message;
      Raw : Unsigned_64;
   begin
      Last_Attempt := Now_Ms;
      Attempted := True;
      if not Loan_Ready then
         Raw := syscall (SYSCALL_SBRK, (Loan_Pages + 1) * CuBit.Memory_Grants.PAGE_SIZE);
         if Raw = 0 or else Raw = Unsigned_64'Last then
            return;
         end if;
         Buffer := To_Address (Integer_Address
           ((Raw + CuBit.Memory_Grants.PAGE_SIZE - 1) and not (CuBit.Memory_Grants.PAGE_SIZE - 1)));
         CuBit.Memory_Grants.Create_Via_Capability
           (Startup.Filesystem, Buffer, Loan_Pages, True, Loan, Loan_Ready);
         if not Loan_Ready then
            return;
         end if;
      end if;
      declare
         Text : String (1 .. Path'Length) with Import, Address => Buffer;
      begin
         Text := Path;
      end;
      Request := Open_Request (Loan, Path'Length, OPEN_READ_WRITE + OPEN_CREATE);
      if not Call (Request) then
         return;   --  the volume is not attached yet: try again later
      end if;
      Handle := File_Handle (Request.words (0));
      Request := Seek_Request (Handle, 0, From_End);
      if not Call (Request) then
         Request := Close_Request (Handle);
         if not Call (Request) then
            debugPrint ("logstore: the persistent log did not close" & Line_Feed);
         end if;
         return;
      end if;
      Opened := True;
      Last_Flush := Now_Ms;
   end Try_Open;

   --  Appends Text to the file. False: the write failed (persistence stops,
   --  and says so once).
   function Append (Text : String) return Boolean is
      Request : Message;
   begin
      if Text'Length = 0 then
         return True;
      end if;
      declare
         Target : String (1 .. Text'Length) with Import, Address => Buffer;
      begin
         Target := Text;
      end;
      Request := Write_Request (Handle, Loan, Unsigned_64 (Text'Length));
      Unflushed := True;
      return Call (Request) and then Request.words (0) = Unsigned_64 (Text'Length);
   end Append;

   procedure Flush (Now_Ms : Unsigned_64) is
      Request : Message := Flush_Request (Handle);
      Ignore : Boolean;
   begin
      Ignore := Call (Request);
      Unflushed := False;
      Last_Flush := Now_Ms;
   end Flush;

   procedure Pump (Store : in out Log_Fanout.Broker; Now_Ms : Unsigned_64) is
      Value : CuBit.Log_Protocol.Event;
      Found : Boolean := True;
      Lost : Unsigned_64;
      Batch : String (1 .. Loan_Bytes);
      Used : Natural := 0;
   begin
      if Volume_Length = 0 or else Failed then
         return;
      end if;
      if not Opened then
         if Attempted and then Now_Ms - Last_Attempt < Retry_Milliseconds then
            return;
         end if;
         Try_Open (Now_Ms);
         if not Opened then
            return;
         end if;
         declare
            Header : constant String :=
              "== logstore: boot, monotonic ms " & Decimal (Now_Ms) & " ==" & Line_Feed;
         begin
            if not Append (Header) then
               Failed := True;
               return;
            end if;
         end;
      end if;
      while Found loop
         Log_Fanout.Take_Persistent (Store, Value, Found, Lost);
         if Lost > 0 then
            declare
               Note : constant String :=
                 "== logstore: " & Decimal (Lost) & " records dropped before this one ==" & Line_Feed;
            begin
               if Used + Note'Length > Batch'Length then
                  Failed := not Append (Batch (1 .. Used));
                  Used := 0;
               end if;
               Batch (Used + 1 .. Used + Note'Length) := Note;
               Used := Used + Note'Length;
            end;
         end if;
         exit when not Found or else Failed;
         declare
            Text : constant String := Logs.Text (Value.Data);
            Line : constant String :=
              Decimal (Value.Monotonic_Ms) & " " & Decimal (Value.Source) & " " &
              Severity_Text (Logs.Level (Value.Data)) & " " &
              Text (Text'First .. Text'First + Natural'Min (Text'Length, Maximum_Line) - 1) & Line_Feed;
         begin
            if Used + Line'Length > Batch'Length then
               Failed := not Append (Batch (1 .. Used));
               Used := 0;
            end if;
            if Line'Length <= Batch'Length then
               Batch (Used + 1 .. Used + Line'Length) := Line;
               Used := Used + Line'Length;
            end if;
         end;
      end loop;
      if Used > 0 and then not Failed then
         Failed := not Append (Batch (1 .. Used));
      end if;
      if Failed then
         debugPrint ("logstore: the persistent log could not be written; persistence stopped" & Line_Feed);
         return;
      end if;
      if Unflushed and then Now_Ms - Last_Flush >= Flush_Milliseconds then
         Flush (Now_Ms);
      end if;
   end Pump;

end Log_Persistence;
