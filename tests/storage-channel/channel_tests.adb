with Ada.Text_IO;
with System;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Filesystems; use CuBit.Filesystems;
with Storage_Channel; use Storage_Channel;

procedure Channel_Tests is
   use type System.Address;
   package Grants renames CuBit.Memory_Grants;
   Cases : Natural := 0;
   type Positive_Array is array (Positive range <>) of Positive;
   function Answer (Token, Value : Unsigned_64; Code : Unsigned_32 := REPLY_OK;
                    Words : Unsigned_8 := 1) return CompletionEntry is
     (requestId => Token + 100, token => Token,
      msg => ((Code, Words, 0, 0), 0, [0 => Value, others => 0]),
      from => 42, status => COMPLETION_OK, valid => True);

   procedure Check_Operation (Op : Operation) is
      Object : Channel;
      Ok : Boolean;
      Sent : Submission;
      Done : Completion_Result;
      Code : Unsigned_32;
      Value : Unsigned_64;
      Output : String (Integer'Last - 3 .. Integer'Last) := "????";
      Input : String (17 .. 20) := "abcd";
      Reply : CompletionEntry;
   begin
      Initialize (Object, 12, Ok);
      pragma Assert (Ok and Status (Object) = Ready);
      declare
         Page : String (1 .. 4096) with Import, Address => Grants.Mapping;
      begin
         Submit (Object, Op, 23, 123,
           (if Op in Open_Existing | Storage_Channel.Open_Create | Write_Data then Input else ""),
           (if Op = Read_Data then 4 else 0), 1, Sent);
         pragma Assert (Sent = Submitted and Status (Object) = Waiting);
         pragma Assert (Last_Endpoint = 12 and Last_Token = 1);
         if Op in Open_Existing | Storage_Channel.Open_Create | Write_Data then
            Input := "xxxx";
            pragma Assert (Page (1 .. 4) = "abcd");
         end if;
         if Op in Read_Data | Write_Data then
            pragma Assert (Last_Request.words (3) = 123);
         end if;
         Submit (Object, Flush, 23, 0, "", 0, 2, Sent);
         pragma Assert (Sent = Busy and Pending_Token (Object) = 1);
         Complete (Object, Answer (2, 0), Done);
         pragma Assert (Done = Ignored and Status (Object) = Waiting);
         Complete (Object, NULL_COMPLETION, Done);
         pragma Assert (Done = Ignored);
         if Op = Read_Data then Page (1 .. 4) := "read"; end if;
         Reply := Answer (1,
           (case Op is
               when Open_Existing | Storage_Channel.Open_Create => 23,
               when Read_Data | Write_Data => 4,
               when Size => 999,
               when others => 0),
            Words => (if Op in Open_Existing | Storage_Channel.Open_Create then 2 else 1));
         Complete (Object, Reply, Done);
         pragma Assert (Done = Completed and Status (Object) = Result_Ready);
         Complete (Object, Reply, Done);
         pragma Assert (Done = Ignored);
         Submit (Object, Flush, 23, 0, "", 0, 2, Sent);
         pragma Assert (Sent = Busy);
         if Op = Read_Data then
            Take_Result (Object, Output (Output'First .. Output'First), Code, Value, Ok);
            pragma Assert (not Ok and Status (Object) = Result_Ready);
         end if;
         Take_Result (Object, Output, Code, Value, Ok);
         pragma Assert (Ok and Code = REPLY_OK and Value = Reply.msg.words (0));
         pragma Assert (Output = (if Op = Read_Data then "read" else "????"));
         pragma Assert (Status (Object) = Ready and Pending_Token (Object) = 0);
         Submit (Object, Flush, 23, 0, "", 0, 1, Sent);
         pragma Assert (Sent = Invalid_Request);
         Retire (Object, Ok);
         pragma Assert (Ok and Status (Object) = Retired);
         Initialize (Object, 12, Ok);
         pragma Assert (not Ok);
      end;
      Cases := Cases + 1;
   end Check_Operation;

   procedure Check_Failure (Kind : Positive) is
      Object : Channel;
      Ok : Boolean;
      Sent : Submission;
      Done : Completion_Result;
      Reply : CompletionEntry := Answer (1, 4);
      Output : String := "safe";
      Code : Unsigned_32;
      Value : Unsigned_64;
   begin
      Initialize (Object, 1, Ok);
      pragma Assert (Ok);
      Submit (Object, Read_Data, 1, 0, "", 4, 1, Sent);
      pragma Assert (Sent = Submitted);
      case Kind is
         when 1 => Reply.status := COMPLETION_TARGET_DIED;
         when 2 => Reply.status := COMPLETION_CANCELLED;
         when 3 => Reply.status := COMPLETION_QUEUE_OVERFLOW;
         when 4 => Reply.status := Unsigned_64'Last;
         when 5 => Reply.requestId := 0;
         when 6 => Reply.msg.tag.length := 2;
         when 7 => Reply.msg.tag.flags := 1;
         when 8 => Reply.msg.tag.reserved := 1;
         when 9 => Reply.msg.words (0) := 5;
         when 10 => Reply.msg.tag.label := REPLY_IO_ERROR;
         when others => Reply.msg.tag.label := REPLY_NOT_FOUND;
      end case;
      Complete (Object, Reply, Done);
      pragma Assert (Done = Completed);
      Take_Result (Object, Output, Code, Value, Ok);
      pragma Assert (Ok and Code /= REPLY_OK and Value = 0 and Output = "safe");
      pragma Assert (Status (Object) = Failed);
      Submit (Object, Read_Data, 1, 0, "", 4, 2, Sent);
      pragma Assert (Sent = Unavailable);
      Submit (Object, Close, 1, 0, "", 0, 2, Sent);
      pragma Assert (Sent = Submitted);
      Complete (Object, Answer (2, 0), Done);
      Take_Result (Object, Output, Code, Value, Ok);
      pragma Assert (Ok and Code = REPLY_OK and Status (Object) = Failed);
      Retire (Object, Ok);
      pragma Assert (Ok);
      Cases := Cases + 1;
   end Check_Failure;

   procedure Check_Denial (Label : Unsigned_32) is
      Object : Channel;
      Ok : Boolean;
      Sent : Submission;
      Done : Completion_Result;
      Empty : String := "";
      Code : Unsigned_32;
      Value : Unsigned_64;
   begin
      Initialize (Object, 1, Ok);
      Submit (Object, Open_Existing, 0, 0, "db", 0, 1, Sent);
      pragma Assert (Sent = Submitted);
      Complete (Object, Answer (1, 0, Label), Done);
      Take_Result (Object, Empty, Code, Value, Ok);
      pragma Assert (Ok and Code = Label and Status (Object) = Ready);
      Retire (Object, Ok);
      pragma Assert (Ok);
      Cases := Cases + 1;
   end Check_Denial;

   procedure Check_Malformed_Success (Op : Operation) is
      Object : Channel;
      Ok : Boolean;
      Sent : Submission;
      Done : Completion_Result;
      Output : String := "safe";
      Code : Unsigned_32;
      Value : Unsigned_64;
      Reply : CompletionEntry := Answer (1, 5);
   begin
      Initialize (Object, 1, Ok);
      Submit (Object, Op, 1, 0,
        (if Op in Open_Existing | Storage_Channel.Open_Create | Write_Data then "data" else ""),
        (if Op = Read_Data then 4 else 0), 1, Sent);
      pragma Assert (Sent = Submitted);
      case Op is
         when Open_Existing | Storage_Channel.Open_Create =>
            Reply.msg.tag.length := 2;
            Reply.msg.words (0) := 0;
         when Size => Reply.msg.tag.flags := 1;
         when others => null;
      end case;
      Complete (Object, Reply, Done);
      Take_Result (Object, Output, Code, Value, Ok);
      pragma Assert (Ok and Code = REPLY_IO_ERROR and Status (Object) = Failed);
      pragma Assert (Value = 0 and Output = "safe");
      Retire (Object, Ok);
      pragma Assert (Ok);
      Cases := Cases + 1;
   end Check_Malformed_Success;

   procedure Check_Transfer (Length : Positive) is
      Object : Channel;
      Ok, Taken : Boolean;
      Sent : Submission;
      Done : Completion_Result;
      Code : Unsigned_32;
      Value : Unsigned_64;
      Input : String (17 .. 16 + Length) := [others => 'W'];
      Output : String (23 .. 24 + Length) := [others => '?'];
   begin
      Initialize (Object, 12, Ok);
      pragma Assert (Ok);
      declare
         Page : String (1 .. Transfer_Bytes) with Import, Address => Grants.Mapping;
      begin
         Submit (Object, Write_Data, 23, 17, Input, 0, 1, Sent);
         pragma Assert (Sent = Submitted and Last_Request.words (2) = Unsigned_64 (Length));
         Input := [others => 'X'];
         pragma Assert (for all C of Page (1 .. Length) => C = 'W');
         Complete (Object, Answer (1, Unsigned_64 (Length)), Done);
         Take_Result (Object, Output, Code, Value, Taken);
         pragma Assert (Taken and Code = REPLY_OK and Value = Unsigned_64 (Length));
         pragma Assert (for all C of Output => C = '?');
         Submit (Object, Read_Data, 23, 17, "", Length, 2, Sent);
         pragma Assert (Sent = Submitted);
         for I in 1 .. Length loop Page (I) := Character'Val (I mod 251); end loop;
         Complete (Object, Answer (2, Unsigned_64 (Length)), Done);
         Take_Result (Object, Output (23 .. 21 + Length), Code, Value, Taken);
         pragma Assert (not Taken and Status (Object) = Result_Ready);
         pragma Assert (for all C of Output => C = '?');
         Take_Result (Object, Output, Code, Value, Taken);
         pragma Assert (Taken and Code = REPLY_OK and Value = Unsigned_64 (Length));
         pragma Assert (for all I in 1 .. Length => Output (22 + I) = Character'Val (I mod 251));
         pragma Assert (Output (23 + Length .. 24 + Length) = "??");
      end;
      Retire (Object, Ok);
      pragma Assert (Ok);
      Cases := Cases + 1;
   end Check_Transfer;
begin
   Grants.Expected_Pages := Transfer_Bytes / 4096;
   declare
      Object : Channel;
      Filled : Natural := 0;
      Ok, Taken : Boolean;
      Sent : Submission;
      Done : Completion_Result;
      Code : Unsigned_32;
      Value : Unsigned_64;
      Empty : String := "";
      procedure Fill (Buffer : out Transfer_Buffer) is
      begin
         pragma Assert (Buffer'Address = Grants.Mapping);
         pragma Assert (Buffer'Length = 4097);
         Filled := Filled + 1;
         Buffer (Buffer'First .. Buffer'First + 23) := [others => 'H'];
         Buffer (Buffer'First + 24 .. Buffer'Last) := [others => 'P'];
      end Fill;
      procedure Send is new Submit_With_Payload (Fill);
   begin
      Send (Object, Write_Data, 23, 0, 4097, 0, 1, Sent);
      pragma Assert (Sent = Unavailable and Filled = 0);
      Initialize (Object, 12, Ok); pragma Assert (Ok);
      Send (Object, Write_Data, 0, 0, 4097, 0, 1, Sent);
      pragma Assert (Sent = Invalid_Request and Filled = 0);
      Send (Object, Write_Data, 23, 0, Transfer_Bytes + 1, 0, 1, Sent);
      pragma Assert (Sent = Invalid_Request and Filled = 0);
      Accept_Submission := False;
      Send (Object, Write_Data, 23, 0, 4097, 0, 1, Sent);
      pragma Assert (Sent = Not_Submitted and Filled = 1);
      Accept_Submission := True;
      Send (Object, Write_Data, 23, 0, 4097, 0, 1, Sent);
      pragma Assert (Sent = Invalid_Request and Filled = 1);
      Send (Object, Write_Data, 23, 0, 4097, 0, 2, Sent);
      pragma Assert (Sent = Submitted and Filled = 2);
      declare
         View : String (1 .. 4097) with Import, Address => Grants.Mapping;
      begin
         pragma Assert ((for all C of View (1 .. 24) => C = 'H') and
                        (for all C of View (25 .. View'Last) => C = 'P'));
      end;
      Send (Object, Write_Data, 23, 0, 4097, 0, 3, Sent);
      pragma Assert (Sent = Busy and Filled = 2);
      Complete (Object, Answer (2, 4097), Done);
      Send (Object, Write_Data, 23, 0, 4097, 0, 3, Sent);
      pragma Assert (Sent = Busy and Filled = 2);
      Take_Result (Object, Empty, Code, Value, Taken);
      pragma Assert (Taken and Code = REPLY_OK and Value = 4097);
      Retire (Object, Ok); pragma Assert (Ok);
      Send (Object, Write_Data, 23, 0, 4097, 0, 3, Sent);
      pragma Assert (Sent = Unavailable and Filled = 2);
      Cases := Cases + 1;
   end;
   for Length of Positive_Array'([1, 4095, 4096, 4097, Transfer_Bytes - 1, Transfer_Bytes]) loop
      Check_Transfer (Length);
   end loop;
   for Op in Operation loop Check_Operation (Op); end loop;
   for Op in Operation loop Check_Malformed_Success (Op); end loop;
   for Kind in 1 .. 11 loop Check_Failure (Kind); end loop;
   Check_Denial (REPLY_NOT_FOUND);
   Check_Denial (REPLY_ACCESS_DENIED);
   Check_Denial (REPLY_SHARING_VIOLATION);
   Check_Denial (REPLY_NO_SPACE);
   Check_Denial (REPLY_READ_ONLY);
   declare
      Object : Channel;
      Ok : Boolean;
      Sent : Submission;
      Done : Completion_Result;
   begin
      Initialize (Object, 1, Ok);
      pragma Assert (Ok);
      Accept_Submission := False;
      Submit (Object, Flush, 1, 0, "", 0, 1, Sent);
      pragma Assert (Sent = Not_Submitted and Status (Object) = Ready);
      Accept_Submission := True;
      Submit (Object, Flush, 1, 0, "", 0, 1, Sent);
      pragma Assert (Sent = Invalid_Request);
      Submit (Object, Flush, 1, 0, "", 0, NO_COMPLETION_TOKEN, Sent);
      pragma Assert (Sent = Invalid_Request);
      Submit (Object, Read_Data, 1, 0, "", Transfer_Bytes + 1, 2, Sent);
      pragma Assert (Sent = Invalid_Request);
      Submit (Object, Write_Data, 1, 0, String'([1 .. Transfer_Bytes + 1 => 'X']), 0, 2, Sent);
      pragma Assert (Sent = Invalid_Request and Status (Object) = Ready);
      Submit (Object, Flush, 0, 0, "", 0, 2, Sent);
      pragma Assert (Sent = Invalid_Request);
      Submit (Object, Flush, 1, 0, "oops", 0, 2, Sent);
      pragma Assert (Sent = Invalid_Request);
      Submit (Object, Flush, 1, 0, "", 0, 2, Sent);
      pragma Assert (Sent = Submitted);
      Grants.Is_Retired := False;
      Retire (Object, Ok);
      pragma Assert (not Ok and Status (Object) = Retired);
      Complete (Object, Answer (2, 0), Done);
      pragma Assert (Done = Ignored);
      Submit (Object, Flush, 1, 0, "", 0, 3, Sent);
      pragma Assert (Sent = Unavailable);
      Grants.Is_Retired := True;
      --  Once the final acquisition returns, the kernel's repeated Revoke
      --  may say inactive/not-current; its retirement query is definitive.
      Grants.Allow_Revoke := False;
      Retire (Object, Ok);
      pragma Assert (Ok);
      Grants.Allow_Revoke := True;
      Cases := Cases + 1;
   end;
   declare
      Object : Channel;
      Ok : Boolean;
      Sent : Submission;
   begin
      Grants.Allow_Grant := False;
      Initialize (Object, 1, Ok);
      pragma Assert (not Ok and Status (Object) = Failed);
      Submit (Object, Close, 1, 0, "", 0, 1, Sent);
      pragma Assert (Sent = Unavailable);
      Retire (Object, Ok);
      pragma Assert (Ok);
      Grants.Allow_Grant := True;
      Cases := Cases + 1;
   end;
   declare
      Object : Channel;
      Ok : Boolean;
      Sent : Submission;
      Done : Completion_Result;
      Old_Reply : CompletionEntry := Answer (1, 0);
      Empty : String := "";
      Code : Unsigned_32;
      Value : Unsigned_64;
      Last : constant Unsigned_64 := NO_COMPLETION_TOKEN - 1;
   begin
      Initialize (Object, 1, Ok);
      Submit (Object, Flush, 1, 0, "", 0, 1, Sent);
      Complete (Object, Answer (1, 0), Done);
      Take_Result (Object, Empty, Code, Value, Ok);
      pragma Assert (Ok and Code = REPLY_OK);
      Submit (Object, Flush, 1, 0, "", 0, Last, Sent);
      pragma Assert (Sent = Submitted);
      Old_Reply.status := COMPLETION_TARGET_DIED;
      Complete (Object, Old_Reply, Done);
      pragma Assert (Done = Ignored and Status (Object) = Waiting);
      Complete (Object, Answer (Last, 0), Done);
      Take_Result (Object, Empty, Code, Value, Ok);
      pragma Assert (Ok and Code = REPLY_OK and Status (Object) = Ready);
      Submit (Object, Flush, 1, 0, "", 0, NO_COMPLETION_TOKEN, Sent);
      pragma Assert (Sent = Invalid_Request);
      Submit (Object, Flush, 1, 0, "", 0, 0, Sent);
      pragma Assert (Sent = Invalid_Request);
      Retire (Object, Ok);
      pragma Assert (Ok);
      Cases := Cases + 1;
   end;
   declare
      Object : Channel;
      Ok : Boolean;
      Before : constant Natural := Grants.Revocations;
   begin
      Initialize (Object, 1, Ok);
      pragma Assert (Ok);
      Grants.Allow_Revoke := False;
      Grants.Is_Retired := False;
      Retire (Object, Ok);
      pragma Assert (not Ok and Grants.Revocations = Before + 1);
      Grants.Allow_Revoke := True;
      Retire (Object, Ok);
      pragma Assert (not Ok and Grants.Revocations = Before + 2);
      --  Accepted-but-pending retirement only queries on subsequent calls.
      Retire (Object, Ok);
      pragma Assert (not Ok and Grants.Revocations = Before + 2);
      Grants.Is_Retired := True;
      Retire (Object, Ok);
      pragma Assert (Ok and Grants.Revocations = Before + 2);
      Cases := Cases + 1;
   end;
   Ada.Text_IO.Put_Line ("STORAGE-CHANNEL: PASS" & Cases'Image & " scenarios");
end Channel_Tests;
