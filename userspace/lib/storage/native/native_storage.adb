with CuBit.Messages; use CuBit.Messages;
with CuBit.Filesystems; use CuBit.Filesystems;
with Storage_Channel;

package body Native_Storage is
   use type System.Address;
   use type Storage_Channel.Operation;
   use type Storage_Channel.Submission;
   use type Storage_Channel.Completion_Result;
   Object : Storage_Channel.Channel;
   Next_Token : Unsigned_64 := 1;

   function Transfer_Capacity return Unsigned_64 is
     (Storage_Channel.Transfer_Bytes);

   function Initialize (Endpoint : Unsigned_64) return Unsigned_32 is
      Ok : Boolean;
   begin
      if Endpoint > 63 then return REPLY_ERR; end if;
      Storage_Channel.Initialize (Object, CapabilitySlot (Endpoint), Ok);
      return (if Ok then REPLY_OK else REPLY_ACCESS_DENIED);
   end Initialize;

   function Finish
     (Submitted : Storage_Channel.Submission; Output : in out String;
      Value : not null access Unsigned_64) return Unsigned_32
   is
      Finished : Storage_Channel.Completion_Result;
      Completion : aliased CompletionEntry;
      Count : Unsigned_64;
      Code : Unsigned_32;
      Taken : Boolean;
   begin
      Next_Token := Next_Token + 1;
      if Submitted /= Storage_Channel.Submitted then
         return (if Submitted = Storage_Channel.Invalid_Request then REPLY_ERR
                 else REPLY_RECOVERY_REQUIRED);
      end if;
      -- Dedicated single-owner worker only: park, never poll. Config's outer
      -- dispatcher must route completions rather than call this wrapper.
      loop
         Completion := NULL_COMPLETION;
         Count := waitCompletion (Completion'Address, 1, 1);
         if Count /= 1 then
            Storage_Channel.Retire (Object, Taken);
            return REPLY_RECOVERY_REQUIRED;
         end if;
         Storage_Channel.Complete (Object, Completion, Finished);
         exit when Finished = Storage_Channel.Completed;
      end loop;
      Storage_Channel.Take_Result (Object, Output, Code, Value.all, Taken);
      if not Taken then return REPLY_RECOVERY_REQUIRED; end if;
      return Code;
   end Finish;

   function Execute
     (Operation : Unsigned_32; Handle, Position : Unsigned_64;
      Data : System.Address; Length : Unsigned_64;
      Value : access Unsigned_64) return Unsigned_32
   is
      Op : Storage_Channel.Operation;
      Submitted : Storage_Channel.Submission;
      Empty : String := "";
   begin
      if Value = null then return REPLY_ERR; end if;
      Value.all := 0;
      if Operation > 7 then return REPLY_ERR; end if;
      Op := Storage_Channel.Operation'Enum_Val (Operation);
      if Op in Storage_Channel.Open_Existing | Storage_Channel.Open_Create |
        Storage_Channel.Read_Data | Storage_Channel.Write_Data
      then
         if Length = 0 or else Length > Storage_Channel.Transfer_Bytes or else Data = System.Null_Address then
            return REPLY_ERR;
         end if;
      elsif Length /= 0 then return REPLY_ERR;
      end if;
      if Next_Token = NO_COMPLETION_TOKEN then return REPLY_RECOVERY_REQUIRED; end if;
      if Op in Storage_Channel.Open_Existing | Storage_Channel.Open_Create |
        Storage_Channel.Write_Data
      then
         declare
            Input : String (1 .. Natural (Length)) with Import, Address => Data;
         begin
            Storage_Channel.Submit
              (Object, Op, Handle, Position, Input, 0, Next_Token, Submitted);
         end;
      else
         Storage_Channel.Submit
           (Object, Op, Handle, Position, "",
            (if Op = Storage_Channel.Read_Data then Natural (Length) else 0),
            Next_Token, Submitted);
      end if;
      if Op = Storage_Channel.Read_Data then
         declare
            Output : String (1 .. Natural (Length)) with Import, Address => Data;
         begin
            return Finish (Submitted, Output, Value);
         end;
      else
         return Finish (Submitted, Empty, Value);
      end if;
   end Execute;

   function Write_Vector
     (Handle, Position : Unsigned_64; Parts : System.Address; Count : Unsigned_64;
      Value : access Unsigned_64) return Unsigned_32
   is
      type Part is record
         Data : System.Address;
         Length : Unsigned_64;
      end record with Convention => C;
      type Part_Array is array (Positive range <>) of Part with Convention => C;
      Total : Natural := 0;
      Submitted : Storage_Channel.Submission;
      Empty : String := "";
   begin
      if Value = null then return REPLY_ERR; end if;
      Value.all := 0;
      if Parts = System.Null_Address or Count = 0 or Count > Storage_Channel.Transfer_Bytes then
         return REPLY_ERR;
      end if;
      if Next_Token = NO_COMPLETION_TOKEN then return REPLY_RECOVERY_REQUIRED; end if;
      declare
         Borrowed : Part_Array (1 .. Natural (Count)) with Import, Address => Parts;
         Descriptors : constant Part_Array := Borrowed;
         procedure Fill (Buffer : out Storage_Channel.Transfer_Buffer) is
            Cursor : Natural := 0;
         begin
            for Segment of Descriptors loop
               declare
                  Length : constant Natural := Natural (Segment.Length);
                  Input : String (1 .. Length) with Import, Address => Segment.Data;
               begin
                  Buffer (Buffer'First + Cursor .. Buffer'First + Cursor + Length - 1) :=
                    Storage_Channel.Transfer_Buffer (Input);
                  Cursor := Cursor + Length;
               end;
            end loop;
         end Fill;
         procedure Send is new Storage_Channel.Submit_With_Payload (Fill);
      begin
         for Segment of Descriptors loop
            if Segment.Data = System.Null_Address or else Segment.Length = 0 or else
              Segment.Length > Unsigned_64 (Storage_Channel.Transfer_Bytes - Total)
            then return REPLY_ERR; end if;
            Total := Total + Natural (Segment.Length);
         end loop;
         if Position > Unsigned_64'Last - Unsigned_64 (Total) then return REPLY_ERR; end if;
         Send (Object, Storage_Channel.Write_Data, Handle, Position, Total, 0, Next_Token, Submitted);
      end;
      return Finish (Submitted, Empty, Value);
   end Write_Vector;

   function Shutdown return Unsigned_32 is
      Retired : Boolean;
   begin
      Storage_Channel.Retire (Object, Retired);
      return (if Retired then REPLY_OK else REPLY_RECOVERY_REQUIRED);
   end Shutdown;
end Native_Storage;
