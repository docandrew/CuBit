------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles;
with CuBit.Handles.Startup;
with CuBit.Startup_Tables;
with CuBit.Memory_Grants;
with CuBit.Grant_References;
with CuBit.Display_Capture;
with CuBit.Logging;
with SPARKNaCl;
with SPARKTLSCrypto.Hashing.SHA256;
with CCL_Native_Bindings;
with Remote_Activity;
with Remote_Files;
with Remote_Pairing;
with Remote_PNG;

package body Remote_Capture is
   package DC renames CuBit.Display_Capture;
   package MG renames CuBit.Memory_Grants;
   package GR renames CuBit.Grant_References;
   package SHA renames SPARKTLSCrypto.Hashing.SHA256;
   use type W.Status, W.Capture_Result;
   use type DC.Status;

   Page_Bytes : constant := 4_096;
   --  Named ceiling: the largest frame taken (an 8K output, BGRA).
   Maximum_Frame_Bytes : constant := 7_680 * 4_320 * DC.Bytes_Per_Pixel;
   --  How long a capture call waits for Desktop (it answers in one turn).
   Call_Ms : constant := 5_000;

   function Find return CuBit.Handles.Endpoint is
      Found : constant CuBit.Handles.Startup.Optional_Endpoint :=
        CuBit.Handles.Startup.Optional_Endpoint_Named ("display-capture", CuBit.Startup_Tables.Endpoint);
   begin
      return (if Found.Present then Found.Value else CuBit.Handles.No_Endpoint);
   end Find;
   Endpoint : constant CuBit.Handles.Endpoint := Find;

   function Available return Boolean is (CuBit.Handles.Valid (Endpoint));

   --  The frame buffer: this process's own pages, lent to Desktop per capture.
   Buffer : Unsigned_64 := 0;
   Capacity : Unsigned_64 := 0;
   Last_Loan : GR.Reference;
   Lent_Once : Boolean := False;

   function Now return Unsigned_64 is (syscall (SYSCALL_GETTIME));

   procedure Log (Text : String) is
      Ignore : Boolean;
   begin
      debugPrint ("ccl-remote: " & Text & ASCII.LF);
      if CCL_Native_Bindings.Logstore.Present then
         CuBit.Logging.Announce ("ccl-remote: " & Text, Ignore, CCL_Native_Bindings.Logstore.Value);
      end if;
   end Log;

   function Reason (Item : W.Capture_Result) return String is
     (case Item is
         when W.Captured => "captured",
         when W.No_Capability =>
           "no display-capture capability (the startup entry needs (display-capture approve-declared))",
         when W.Capture_Off => DC.Reason (DC.Capture_Off),
         when W.Rate_Limited => DC.Reason (DC.Rate_Limited),
         when W.No_Session => DC.Reason (DC.No_Session),
         when W.No_Frame => DC.Reason (DC.No_Frame),
         when W.Bad_Output => DC.Reason (DC.Bad_Output),
         when W.Bad_Area => DC.Reason (DC.Bad_Area),
         when W.Capture_Failed => "the capture failed");

   procedure Note (Argument : String; Result : W.Capture_Result) is
      Outcome : constant Remote_Activity.Outcome :=
        (case Result is
            when W.Captured => Remote_Activity.OK,
            when W.No_Capability | W.Capture_Off | W.Rate_Limited | W.No_Session => Remote_Activity.Refused,
            when others => Remote_Activity.Failed);
   begin
      Remote_Activity.Add
        (Now, Remote_Activity.Capture,
         Argument & (if Result = W.Captured then "" else " (" & Reason (Result) & ")"), Outcome);
      Log ("host " & Remote_Activity.Line (Remote_Activity.Entry_At (Remote_Activity.Count)));
      Remote_Pairing.Activity_Changed;
   end Note;

   procedure Announce (State : Session_State; Peer : String) is
      Request : Message := NULL_MESSAGE;
      Tag : MessageTag;
      Text : constant String := Peer (Peer'First .. Peer'First - 1 + Natural'Min (Peer'Length, DC.Peer_Text_Bytes));
   begin
      if not Available then
         return;
      end if;
      Request.tag := (label => DC.Announce_Label, length => DC.Request_Words, protocol => 0, ipc => <>,
                      reserved => 0);
      Request.words (0) := (case State is when Ended => 0, when Listening => 1, when Paired => 2);
      if State = Paired then
         for I in Text'Range loop
            declare
               K : constant Natural := I - Text'First;
            begin
               Request.words (1 + K / 8) := Request.words (1 + K / 8) or
                 Shift_Left (Unsigned_64 (Character'Pos (Text (I))), 8 * (K mod 8));
            end;
         end loop;
      end if;
      Tag := capCall (Endpoint, Request, Deadline_After (Call_Ms));
      if Tag.label /= DC.Status'Enum_Rep (DC.Captured) then
         Log ("Desktop did not take the session announcement (display-capture)");
      end if;
   end Announce;

   --  Room for Bytes, page-rounded; the old pages go back once Desktop's
   --  loan of them is retired (never while it might still map them).
   procedure Reserve (Bytes : Unsigned_64; OK : out Boolean) is
      Wanted : constant Unsigned_64 := (Bytes + Page_Bytes - 1) / Page_Bytes * Page_Bytes;
      Raw, Ignore : Unsigned_64;
   begin
      OK := Capacity >= Bytes and then Buffer /= 0;
      if OK then
         return;
      end if;
      if Bytes = 0 or else Bytes > Maximum_Frame_Bytes then
         return;
      end if;
      if Buffer /= 0 then
         if Lent_Once and then not MG.Retirement_Confirmed (Last_Loan) then
            Log ("the previous capture buffer is still lent; keeping it (a capture of this size waits)");
            return;
         end if;
         Ignore := syscall (SYSCALL_RELEASE_OWNED_MEMORY, Buffer, Capacity);
         Buffer := 0;
         Capacity := 0;
      end if;
      Raw := syscall (SYSCALL_ALLOCATE_OWNED_MEMORY, Wanted);
      if Raw = 0 or else Raw = Unsigned_64'Last or else Raw mod Page_Bytes /= 0 then
         return;
      end if;
      Buffer := Raw;
      Capacity := Wanted;
      OK := True;
   end Reserve;

   procedure Take
     (Output : W.Output_Number; Where : Area; Result : out W.Capture_Result;
      Width, Height : out Natural; Pixels : out System.Address)
   is
      Request : Message;
      Tag : MessageTag;
      OK, Revoked : Boolean;
      Loan : GR.Reference;
      Answer : DC.Status_Decoding;
      Size_Word, Bytes_Word : Unsigned_64;
   begin
      Result := W.Capture_Failed;
      Width := 0;
      Height := 0;
      Pixels := System.Null_Address;
      if not Available then
         Result := W.No_Capability;
         return;
      end if;
      Reserve (Page_Bytes, OK);
      if not OK then
         return;
      end if;
      --  At most twice: the first answer may say the buffer is too small
      --  (and how large the frame is).
      for Attempt in 1 .. 2 loop
         MG.Create_Via_Capability
           (Endpoint, To_Address (Integer_Address (Buffer)), Natural (Capacity / Page_Bytes), True, Loan, OK);
         if not OK then
            return;
         end if;
         Last_Loan := Loan;
         Lent_Once := True;
         Request := NULL_MESSAGE;
         Request.tag := (label => DC.Capture_Label, length => DC.Request_Words, protocol => 0, ipc => <>,
                         reserved => 0);
         Request.words := [0 => GR.Encode (Loan), 1 => Capacity, 2 => Unsigned_64 (Output),
                           3 => DC.Encode_Area ((Where.X, Where.Y, Where.Width, Where.Height))];
         Tag := capCall (Endpoint, Request, Deadline_After (Call_Ms));
         MG.Revoke (Loan, Revoked);
         Answer := DC.Decode_Status (Tag.label);
         Size_Word := Request.words (0);
         Bytes_Word := Request.words (1);
         if not Answer.Valid then
            return;
         end if;
         case Answer.Value is
            when DC.Captured =>
               if DC.Width_Of (Size_Word) = 0 or else DC.Height_Of (Size_Word) = 0
                 or else Bytes_Word /= DC.Width_Of (Size_Word) * DC.Height_Of (Size_Word) * DC.Bytes_Per_Pixel
                 or else Bytes_Word > Capacity
               then
                  return;
               end if;
               Width := Natural (DC.Width_Of (Size_Word));
               Height := Natural (DC.Height_Of (Size_Word));
               Pixels := To_Address (Integer_Address (Buffer));
               Result := W.Captured;
               return;
            when DC.Buffer_Too_Small =>
               exit when Attempt = 2 or else Bytes_Word <= Capacity;
               Reserve (Bytes_Word, OK);
               exit when not OK;
            when DC.Not_Authorized => Result := W.No_Capability; return;
            when DC.Capture_Off => Result := W.Capture_Off; return;
            when DC.Rate_Limited => Result := W.Rate_Limited; return;
            when DC.No_Session => Result := W.No_Session; return;
            when DC.No_Frame => Result := W.No_Frame; return;
            when DC.Bad_Output => Result := W.Bad_Output; return;
            when DC.Bad_Area => Result := W.Bad_Area; return;
            when DC.Buffer_Refused | DC.Invalid_Request => return;
         end case;
      end loop;
   end Take;

   procedure Save_PNG
     (Name : String; Width, Height : Positive; Pixels : System.Address;
      Size : out Unsigned_64; Hash : out W.Digest; Result : out W.Status)
   is
      type Frame_Bytes is array (Natural range 0 .. Width * Height * DC.Bytes_Per_Pixel - 1) of Unsigned_8;
      Frame : Frame_Bytes with Import, Address => Pixels;
      Pending : W.Byte_Array (1 .. W.Maximum_Chunk_Bytes);
      Used : Natural := 0;
      Offset : Unsigned_64 := 0;
      Context : SHA.Context;
      Output : SHA.Digest;
      Failed : Boolean := False;

      function Source (Index : Natural) return Unsigned_8 is (Frame (Index));

      procedure Flush is
         Put_Result : W.Status;
      begin
         if Used = 0 then
            return;
         end if;
         if not Failed and then Offset + Unsigned_64 (Used) > W.Maximum_File_Bytes then
            Failed := True;
         end if;
         if not Failed then
            Remote_Files.Put (W.Captures, Name, Offset, Pending (1 .. Used), Put_Result);
            Failed := Put_Result /= W.OK;
         end if;
         if Failed then
            Used := 0;   --  the rest is encoded and dropped; the capture fails
            return;
         end if;
         declare
            Seq : SPARKNaCl.Byte_Seq (0 .. SPARKNaCl.N32 (Used) - 1);
         begin
            for I in 1 .. Used loop
               Seq (SPARKNaCl.N32 (I - 1)) := SPARKNaCl.Byte (Pending (I));
            end loop;
            SHA.Update (Context, Seq);
         end;
         Offset := Offset + Unsigned_64 (Used);
         Used := 0;
      end Flush;

      procedure Emit (Data : W.Byte_Array) is
      begin
         for B of Data loop
            if Used = Pending'Last then
               Flush;
            end if;
            Used := Used + 1;
            Pending (Used) := B;
         end loop;
      end Emit;

      procedure Encode is new Remote_PNG.Encode (Source, Emit);
   begin
      Size := 0;
      Hash := [others => 0];
      Result := W.Failed;
      if Width > Remote_PNG.Maximum_Side or else Height > Remote_PNG.Maximum_Side then
         Result := W.Too_Large;
         return;
      end if;
      SHA.Init (Context);
      Encode (Width, Height);
      Flush;
      if Failed then
         return;
      end if;
      SHA.Final (Context, Output);
      for I in W.Digest_Index loop
         Hash (I) := Output (SPARKNaCl.N32 (I) - 1);
      end loop;
      Size := Offset;
      --  The staging file becomes the name only if what is on disk is what
      --  was written (Remote_Files.Commit reads it back and compares).
      Remote_Files.Commit (W.Captures, Name, Size, Hash, Result);
   end Save_PNG;
end Remote_Capture;
