with Interfaces;
with CuBit.Handles;
with CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Llm_Protocol;
with CuBit.Secrets_Protocol;
with CCL_Native_Bindings;

--  secrets.* on CuBit: each call lends secrets.svc one page holding the
--  name (and, for a store, the value typed at the console), then revokes
--  the loan and wipes the page.
package body CCL_Secrets is
   use Interfaces;
   use type CuBit.Messages.MessageTag;
   package S_P renames CuBit.Secrets_Protocol;

   Page_Bytes : constant := 4_096;
   Call_Deadline_MS : constant := 2_000;
   Page : String (1 .. Page_Bytes) := [others => ' '] with Alignment => Page_Bytes;

   Pending : Boolean := False;
   Pending_Name : String (1 .. MAXIMUM_NAME) := [others => ' '];
   Pending_Length : Natural range 0 .. MAXIMUM_NAME := 0;

   function Granted return Boolean is (CuBit.Handles.Valid (CCL_Native_Bindings.Secrets));

   --  One call with the page lent; Label/words as the protocol says.
   procedure Call
     (Label : Unsigned_32; Name_Length, Value_Length : Natural;
      Tag : out CuBit.Messages.MessageTag; Answer : out CuBit.Messages.MessageWords; Lent : out Boolean)
   is
      Loan : CuBit.Memory_Grants.Grant_Reference;
      Revoked : Boolean;
      M : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
   begin
      Answer := [others => 0];
      Tag := (label => 0, length => 0, protocol => 0, ipc => <>, reserved => 0);
      CuBit.Memory_Grants.Create_Via_Capability
        (CCL_Native_Bindings.Secrets, Page'Address, 1, True, Loan, Lent);
      if Lent then
         M.tag := (label => Label, length => 4, protocol => 0, ipc => <>, reserved => 0);
         M.words := [Loan.slot, Loan.generation, Unsigned_64 (Name_Length), Unsigned_64 (Value_Length)];
         Tag := CuBit.Messages.capCall
           (CCL_Native_Bindings.Secrets, M, CuBit.Messages.Deadline_After (Call_Deadline_MS));
         Answer := M.words;
         CuBit.Memory_Grants.Revoke (Loan, Revoked);
      end if;
      Page := [others => ' '];
   end Call;

   function Store_Result_Of (Answer : CuBit.Messages.MessageWords) return Store_Result is
   begin
      for R in S_P.Refusal loop
         if Answer (0) = Unsigned_64 (S_P.Refusal'Enum_Rep (R)) then
            return (case R is
                       when S_P.Bad_Name => Bad_Name,
                       when S_P.Bad_Value => Bad_Value,
                       when S_P.Full => Full,
                       when others => Unavailable);
         end if;
      end loop;
      return Unavailable;
   end Store_Result_Of;

   procedure Request_Value (Name : String; Result : out Store_Result) is
   begin
      Pending := False;
      if not Granted then
         Result := Not_Granted;
      elsif not Valid_Name (Name) then
         Result := Bad_Name;
      else
         Pending_Name (1 .. Name'Length) := Name;
         Pending_Length := Name'Length;
         Pending := True;
         Result := Stored;
      end if;
   end Request_Value;

   function Value_Requested return Boolean is (Pending);
   function Requested_Name return String is (Pending_Name (1 .. Pending_Length));

   procedure Cancel_Value is
   begin
      Pending := False;
      Pending_Length := 0;
   end Cancel_Value;

   procedure Store_Value (Value : String; Result : out Store_Result) is
      Tag : CuBit.Messages.MessageTag;
      Answer : CuBit.Messages.MessageWords;
      Lent : Boolean;
   begin
      if not Pending then
         Result := Not_Requested;
         return;
      elsif Value'Length not in 1 .. MAXIMUM_VALUE then
         Cancel_Value;
         Result := Bad_Value;
         return;
      end if;
      Page (1 .. Pending_Length) := Pending_Name (1 .. Pending_Length);
      Page (Pending_Length + 1 .. Pending_Length + Value'Length) := Value;
      Call (S_P.Store_Operation, Pending_Length, Value'Length, Tag, Answer, Lent);
      Cancel_Value;
      if not Lent then
         Result := Unavailable;
      elsif Tag.label = S_P.Reply_OK then
         Result := Stored;
      elsif Tag.label = S_P.Reply_Error then
         Result := Store_Result_Of (Answer);
      else
         Result := Unavailable;
      end if;
   end Store_Value;

   procedure Simple (Label : Unsigned_32; Name : String; Result : out Call_Result;
                     Answer : out CuBit.Messages.MessageWords) is
      Tag : CuBit.Messages.MessageTag;
      Lent : Boolean;
   begin
      Answer := [others => 0];
      if not Granted then
         Result := Not_Granted;
         return;
      elsif not Valid_Name (Name) then
         Result := Bad_Name;
         return;
      end if;
      Page (1 .. Name'Length) := Name;
      Call (Label, Name'Length, 0, Tag, Answer, Lent);
      Result := (if Lent and then Tag.label = S_P.Reply_OK then Done else Unavailable);
   end Simple;

   procedure Forget (Name : String; Result : out Call_Result) is
      Answer : CuBit.Messages.MessageWords;
   begin
      Simple (S_P.Forget_Operation, Name, Result, Answer);
   end Forget;

   procedure Status
     (Name : String; Stored : out Boolean; Holder : out String; Holder_Length : out Natural;
      Result : out Call_Result)
   is
      Answer : CuBit.Messages.MessageWords;
      Llm_Name : constant String := "llm";
   begin
      Holder := [others => ' '];
      Holder_Length := 0;
      Simple (S_P.Status_Operation, Name, Result, Answer);
      Stored := Result = Done and then Answer (0) = 1;
      if Result = Done and then Answer (1) = CuBit.Llm_Protocol.Service_Role
        and then Holder'Length >= Llm_Name'Length
      then
         Holder (Holder'First .. Holder'First + Llm_Name'Length - 1) := Llm_Name;
         Holder_Length := Llm_Name'Length;
      end if;
   end Status;

   procedure Revoke_Use (Name : String; Result : out Call_Result) is
      Answer : CuBit.Messages.MessageWords;
   begin
      Simple (S_P.Revoke_Use_Operation, Name, Result, Answer);
   end Revoke_Use;
end CCL_Secrets;
