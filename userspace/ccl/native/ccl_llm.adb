with System.Storage_Elements;
with CuBit.Handles;
with CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Llm_Protocol;
with CCL_Native_Bindings;
with CCL_Llm_Link;

--  llm.* on CuBit: Ask lends llm.svc one window per exchange (the encoded
--  request, then the pulled text and the summary) and answers at once;
--  CCL_Llm_Link follows the exchange with submitted pulls.
package body CCL_Llm is
   use Interfaces;
   use System.Storage_Elements;
   use type System.Address;
   package P renames CuBit.Llm_Protocol;
   use type P.Refusal;
   package L renames CCL_Llm_Link;
   subtype Address is System.Address;

   Call_Deadline_MS : constant := 5_000;
   Loans : array (L.Slot_Index) of CuBit.Memory_Grants.Grant_Reference;

   procedure Put (Base : System.Address; Offset : Natural; Value : Unsigned_64; Width : Positive) is
      Field : array (0 .. Width - 1) of Unsigned_8 with Import, Address => Base + Storage_Offset (Offset);
      V : Unsigned_64 := Value;
   begin
      for I in Field'Range loop
         Field (I) := Unsigned_8 (V and 16#FF#);
         V := Shift_Right (V, 8);
      end loop;
   end Put;

   function Get (Base : System.Address; Offset : Natural; Width : Positive) return Unsigned_64 is
      Field : array (0 .. Width - 1) of Unsigned_8 with Import, Address => Base + Storage_Offset (Offset);
      V : Unsigned_64 := 0;
   begin
      for I in reverse Field'Range loop
         V := Shift_Left (V, 8) or Unsigned_64 (Field (I));
      end loop;
      return V;
   end Get;

   procedure Put_Text (Base : System.Address; Offset : Natural; Text : String) is
      Field : String (1 .. Text'Length) with Import, Address => Base + Storage_Offset (Offset);
   begin
      Field := Text;
   end Put_Text;

   procedure Say (Text : String; Why : out String; Why_Length : out Natural) is
      N : constant Natural := Natural'Min (Text'Length, Why'Length);
   begin
      Why := [others => ' '];
      Why (Why'First .. Why'First + N - 1) := Text (Text'First .. Text'First + N - 1);
      Why_Length := N;
   end Say;

   function Slot_Of (Exchange : Unsigned_64) return Integer is
   begin
      for S in L.Slot_Index loop
         if L.State (S).Used and then L.State (S).Exchange = Exchange then
            return S;
         end if;
      end loop;
      return -1;
   end Slot_Of;

   function Following (Exchange : Unsigned_64) return Boolean is (Slot_Of (Exchange) >= 0);

   --  The loan is revoked before the window is wiped and freed.
   procedure Let_Go (Slot : L.Slot_Index; Lent : Boolean := True) is
      Revoked : Boolean;
   begin
      if Lent then
         CuBit.Memory_Grants.Revoke (Loans (Slot), Revoked);
      end if;
      L.Close_Window (Slot);
      L.Set_State (Slot, (others => <>));
   end Let_Go;

   procedure Ask
     (Model, Mission, System, Tools, Conversation : String;
      Cache, Capture : Boolean;
      Max_Tokens, Deadline_Ms : Positive;
      Ceiling : Ceiling_Kind; Ceiling_Nano_Usd : Unsigned_64;
      Exchange : out Unsigned_64; Result : out Ask_Result;
      Why : out String; Why_Length : out Natural)
   is
      Slot : Integer := -1;
      Request_Bytes : constant Natural :=
        P.Request_Header_Bytes + Model'Length + Mission'Length + System'Length + Tools'Length +
        Conversation'Length;
      Lent, Opened : Boolean;
      M : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
      Tag : CuBit.Messages.MessageTag;
   begin
      Exchange := 0;
      Why_Length := 0;
      Why := [others => ' '];
      if not CuBit.Handles.Valid (CCL_Native_Bindings.Llm) then
         Result := Not_Granted;
         Say ("the program's manifest must request the llm service (service ""llm"" ""llm"")", Why, Why_Length);
         return;
      elsif Request_Bytes > P.Maximum_Ask_Bytes then
         Result := Capacity;
         Say ("capacity: the request's" & Natural'Image (Request_Bytes) & " bytes pass the ceiling of" &
              Natural'Image (P.Maximum_Ask_Bytes), Why, Why_Length);
         return;
      end if;
      for S in L.Slot_Index loop
         if not L.State (S).Used then
            Slot := S;
            exit;
         end if;
      end loop;
      if Slot < 0 then
         Result := Refused;
         Say ("this program already follows" & Natural'Image (MAXIMUM_EXCHANGES) & " exchanges",
              Why, Why_Length);
         return;
      end if;
      --  The exchange's window: the request now, text and content later.
      L.Open_Window (Slot, Natural'Max (Request_Bytes, P.Minimum_Window_Bytes), Opened);
      if not Opened then
         Result := Unavailable;
         Say ("no memory for the exchange's window", Why, Why_Length);
         return;
      end if;
      declare
         Base : constant Address := L.Window_Address (Slot);
         Offset : Natural := P.Request_Header_Bytes;
      begin
         Put (Base, P.Request_Max_Tokens_Offset, Unsigned_64 (Max_Tokens), 4);
         Put (Base, P.Request_Deadline_Offset, Unsigned_64 (Deadline_Ms), 4);
         Put (Base, P.Request_Ceiling_Kind_Offset,
              (if Ceiling = Nano_Usd then Unsigned_64 (P.Ceiling_Kind'Enum_Rep (P.Nano_Usd))
               else Unsigned_64 (P.Ceiling_Kind'Enum_Rep (P.Unlimited))), 1);
         Put (Base, P.Request_Flags_Offset,
              (if Cache then P.Flag_Cache else 0) + (if Capture then P.Flag_Capture else 0), 1);
         Put (Base, P.Request_Ceiling_Offset, Ceiling_Nano_Usd, 8);
         Put (Base, P.Request_Model_Length_Offset, Unsigned_64 (Model'Length), 4);
         Put (Base, P.Request_Mission_Length_Offset, Unsigned_64 (Mission'Length), 4);
         Put (Base, P.Request_System_Length_Offset, Unsigned_64 (System'Length), 4);
         Put (Base, P.Request_Tools_Length_Offset, Unsigned_64 (Tools'Length), 4);
         Put (Base, P.Request_Conversation_Length_Offset, Unsigned_64 (Conversation'Length), 4);
         Put_Text (Base, Offset, Model);
         Offset := Offset + Model'Length;
         Put_Text (Base, Offset, Mission);
         Offset := Offset + Mission'Length;
         Put_Text (Base, Offset, System);
         Offset := Offset + System'Length;
         Put_Text (Base, Offset, Tools);
         Offset := Offset + Tools'Length;
         Put_Text (Base, Offset, Conversation);
         CuBit.Memory_Grants.Create_Via_Capability
           (CCL_Native_Bindings.Llm, Base, L.Window_Size (Slot) / 4_096, True, Loans (Slot), Lent);
      end;
      if not Lent then
         Let_Go (Slot, Lent => False);
         Result := Unavailable;
         Say ("llm.svc could not be lent the exchange's window", Why, Why_Length);
         return;
      end if;
      M.tag := (label => P.Ask_Operation, length => 4, protocol => 0, ipc => <>, reserved => 0);
      M.words := [Loans (Slot).slot, Loans (Slot).generation, Unsigned_64 (Request_Bytes),
                  Unsigned_64 (L.Window_Size (Slot))];
      Tag := CuBit.Messages.capCall
        (CCL_Native_Bindings.Llm, M, CuBit.Messages.Deadline_After (Call_Deadline_MS));
      if Tag.label = P.Reply_OK then
         Exchange := M.words (0);
         L.Set_State (Slot, (Used => True, Exchange => Exchange, others => <>));
         L.Pull (Slot);
         Result := Started;
         return;
      end if;
      Let_Go (Slot);
      if Tag.label = P.Reply_Error and then M.words (0) in 1 .. Unsigned_64 (P.Refusal'Enum_Rep (P.Refusal'Last)) then
         Result := Refused;
         for R in P.Refusal loop
            if Unsigned_64 (P.Refusal'Enum_Rep (R)) = M.words (0) then
               Say ("llm.svc: " & P.Name (R), Why, Why_Length);
               if R = P.Request_Too_Large then
                  Result := Capacity;
               end if;
            end if;
         end loop;
      else
         Result := Unavailable;
         Say ("llm.svc did not answer", Why, Why_Length);
      end if;
   end Ask;

   procedure Take_Text (Exchange : Unsigned_64; Into : out String; Last : out Natural) is
      Slot : constant Integer := Slot_Of (Exchange);
   begin
      Into := [others => ' '];
      Last := Into'First - 1;
      if Slot < 0 then
         return;
      end if;
      declare
         S : L.Slot_State := L.State (Slot);
         N : constant Natural := Natural'Min (S.Text_Ready, Into'Length);
         Window : String (1 .. L.Window_Size (Slot)) with Import, Address => L.Window_Address (Slot);
      begin
         if N > 0 then
            Into (Into'First .. Into'First + N - 1) := Window (1 .. N);
            Last := Into'First + N - 1;
            if N < S.Text_Ready then
               --  Keep the rest at the window's start.
               Window (1 .. S.Text_Ready - N) := Window (N + 1 .. S.Text_Ready);
            end if;
            S.Text_Ready := S.Text_Ready - N;
            L.Set_State (Slot, S);
         end if;
         L.Pull (Slot);
      end;
   end Take_Text;

   function Short (Text : String) return Short_Text is
      N : constant Natural := Natural'Min (Text'Length, MAXIMUM_SHORT);
      Result : Short_Text;
   begin
      Result.Data (1 .. N) := Text (Text'First .. Text'First + N - 1);
      Result.Length := N;
      return Result;
   end Short;

   procedure Take_Summary
     (Exchange : Unsigned_64; Result : out Summary;
      Content : out Text_Access; Ended : out Boolean)
   is
      Slot : constant Integer := Slot_Of (Exchange);
   begin
      Result := (others => <>);
      Ended := False;
      Content := null;
      if Slot < 0 or else not L.State (Slot).Done or else L.State (Slot).Text_Ready > 0 then
         return;
      end if;
      declare
         Base : constant System.Address := L.Window_Address (Slot);
         Stop_Code : constant Unsigned_64 := Get (Base, P.Summary_Stop_Offset, 1);
         Error_Code : constant Unsigned_64 := Get (Base, P.Summary_Error_Offset, 1);
         Problem_Code : constant Unsigned_64 := Get (Base, P.Summary_Problem_Offset, 1);
         Model_Length : constant Natural :=
           Natural (Unsigned_64'Min (Get (Base, P.Summary_Model_Length_Offset, 4), P.Maximum_Model_Bytes));
         Detail_Length : constant Natural :=
           Natural (Unsigned_64'Min (Get (Base, P.Summary_Detail_Length_Offset, 4), P.Maximum_Detail_Bytes));
         Model : String (1 .. Model_Length)
           with Import, Address => Base + Storage_Offset (P.Summary_Model_Offset);
         Detail : String (1 .. Detail_Length)
           with Import, Address => Base + Storage_Offset (P.Summary_Detail_Offset);
      begin
         for R in P.Stop_Reason loop
            if Unsigned_64 (P.Stop_Reason'Enum_Rep (R)) = Stop_Code then
               Result.Ended_As := Stop'Val (P.Stop_Reason'Pos (R));
            end if;
         end loop;
         for E in P.Error_Kind loop
            if Unsigned_64 (P.Error_Kind'Enum_Rep (E)) = Error_Code then
               Result.Error := Short (P.Image (E));
            end if;
         end loop;
         for Q in P.Problem loop
            if Unsigned_64 (P.Problem'Enum_Rep (Q)) = Problem_Code then
               Result.Problem := Short (P.Image (Q));
            end if;
         end loop;
         Result.Model := Short (Model);
         Result.Detail := Short (Detail);
         Result.Status := Natural (Unsigned_64'Min (Get (Base, P.Summary_Status_Offset, 4), 999));
         Result.Input := Get (Base, P.Summary_Input_Offset, 8);
         Result.Output := Get (Base, P.Summary_Output_Offset, 8);
         Result.Cache_Read := Get (Base, P.Summary_Cache_Read_Offset, 8);
         Result.Cache_Write := Get (Base, P.Summary_Cache_Write_Offset, 8);
         Result.Priced := Get (Base, P.Summary_Priced_Offset, 1) = 1;
         Result.Cost_Nano_Usd := Get (Base, P.Summary_Cost_Offset, 8);
         Result.Duration_Ms := Get (Base, P.Summary_Duration_Offset, 8);
         Result.Text_Bytes := Get (Base, P.Summary_Text_Bytes_Offset, 8);
         Result.Tool_Calls := Natural (Unsigned_64'Min (Get (Base, P.Summary_Tool_Calls_Offset, 4), 1_000_000));
         declare
            Announced : constant Unsigned_64 := Get (Base, P.Summary_Content_Length_Offset, 4);
            Received : Text_Access;
            Length : Natural;
         begin
            L.Take_Content (Slot, Received, Length);
            if L.Content_Lost (Slot) or else Announced /= Unsigned_64 (Length) then
               --  Never a silent cut: the turn did not arrive whole.
               Free (Received);
               Result.Content_Lost := True;
            elsif Received /= null and then Length > 0 then
               if Length = Received'Length then
                  Content := Received;
               else
                  Content := new String'(Received (1 .. Length));
                  Free (Received);
               end if;
            else
               Free (Received);
            end if;
         end;
      end;
      Let_Go (Slot);
      Ended := True;
   end Take_Summary;

   procedure Cancel (Exchange : Unsigned_64; Done : out Boolean) is
      M : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
      Tag : CuBit.Messages.MessageTag;
   begin
      Done := False;
      if Slot_Of (Exchange) < 0 then
         return;
      end if;
      M.tag := (label => P.Cancel_Operation, length => 1, protocol => 0, ipc => <>, reserved => 0);
      M.words := [Exchange, 0, 0, 0];
      Tag := CuBit.Messages.capCall
        (CCL_Native_Bindings.Llm, M, CuBit.Messages.Deadline_After (Call_Deadline_MS));
      Done := Tag.label = P.Reply_OK;
   end Cancel;
end CCL_Llm;
