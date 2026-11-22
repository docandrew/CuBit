pragma Ada_2022;

with CuBit.Grant_References;
with CuBit.Kernel_ABI;
with CuBit.Memory_Grants;
with CuBit.Process_IDs.Text;
with CuBit.Handles.IPC;

package body CuBit.Child_Starts is
   use CuBit.Messages;
   package LA renames CuBit.Launch_Arguments;
   use type CS.Refusal;

   Page_Bytes : constant := 4096;
   Request_Pages : constant := (CS.Maximum_Spec_Bytes + Page_Bytes - 1) / Page_Bytes;
   type Request_Area is array (1 .. Request_Pages * Page_Bytes) of Unsigned_8;
   --  Lent to procmgr read-only on first use.
   Request : Request_Area with Alignment => Page_Bytes;
   Lent : Boolean := False;
   Lent_Reference : CuBit.Memory_Grants.Grant_Reference;

   procedure Put_Field (Into : out CS.Field_Text; Text : String);
   procedure Prepare (Through : Starter; Spec : CS.Spec; Msg : out Message; Ready : out Boolean);
   function Decode_Reply (Label : Unsigned_32; Words : Message) return Receipt;
   function Release_Message
     (Child : CuBit.Process_IDs.Process_ID; Decision : CS.Release_Decision) return Message;
   function Sink_Of (Reply_Message : Message) return CuBit.Handles.Endpoint;
   function Release_Answer (Label : Unsigned_32; Reply_Message : Message) return Release_Receipt;

   procedure Put_Field (Into : out CS.Field_Text; Text : String) is
   begin
      Into := (others => <>);
      Into.Length := Text'Length;
      Into.Data (1 .. Text'Length) := Text;
   end Put_Field;

   function Child
     (Program    : String;
      Restart    : CS.Restart_Kind;
      Stop_Grace : CS.Grace_Ms;
      Priority   : CS.Priority_Value;
      Kind       : CS.Child_Kind := CS.Worker;
      Intensity  : CS.Intensity_Count := 0;
      Window     : CS.Window_Ms := 0;
      Stage      : CS.Stop_Stage := CS.Services) return CS.Spec
   is
      S : CS.Spec;
   begin
      S.Restart := Restart;
      S.Stage := Stage;
      S.Kind := Kind;
      S.Supervisor := CS.Caller;
      S.Priority := Priority;
      S.Stop_Grace := Stop_Grace;
      S.Intensity := Intensity;
      S.Window := Window;
      S.Name_Bytes := Program'Length;
      S.Name (1 .. Program'Length) := Program;
      return S;
   end Child;

   procedure Give_Value (S : in out CS.Spec; Value : String) is
   begin
      S.Source := CS.Given;
      S.Value_Bytes := Value'Length;
      S.Value (1 .. Value'Length) := Value;
   end Give_Value;

   procedure Give_Class (S : in out CS.Spec; Class : String) is
   begin
      S.Source := CS.Device_Class;
      S.Value_Bytes := Class'Length;
      S.Value (1 .. Class'Length) := Class;
   end Give_Class;

   procedure Add_Fact (S : in out CS.Spec; Field : String; Value : Integer_64) is
   begin
      S.Facts := S.Facts + 1;
      Put_Field (S.Fact_List (S.Facts).Field, Field);
      S.Fact_List (S.Facts).Value := Value;
   end Add_Fact;

   procedure Add_Binding
     (S : in out CS.Spec; Label : String; Badge : Unsigned_64;
      Provider : Unsigned_64 := CS.No_Provider) is
   begin
      S.Bindings := S.Bindings + 1;
      Put_Field (S.Binding_List (S.Bindings).Label, Label);
      S.Binding_List (S.Bindings).Badge := Badge;
      S.Binding_List (S.Bindings).Provider := Provider;
   end Add_Binding;

   procedure Hold (S : in out CS.Spec) is
   begin
      S.Start := CS.Held;
   end Hold;

   procedure Want_Sink (S : in out CS.Spec; Badge : Unsigned_64) is
   begin
      S.Sink := CS.Write_Only_Sink;
      S.Sink_Badge := Badge;
   end Want_Sink;

   procedure Add_Dependency (S : in out CS.Spec; Sibling : CuBit.Process_IDs.Process_ID) is
   begin
      S.Dependencies := S.Dependencies + 1;
      S.Dependency_List (S.Dependencies) := CuBit.Process_IDs.To_Word (Sibling);
   end Add_Dependency;

   --  Encode Spec into the lent buffer and make the request; False when the
   --  spec is not valid or the buffer cannot be lent.
   procedure Prepare (Through : Starter; Spec : CS.Spec; Msg : out Message; Ready : out Boolean) is
      Bytes : CS.Byte_Array (1 .. CS.Maximum_Spec_Bytes);
      Length : CS.Spec_Length;
      Success : Boolean;
   begin
      Msg := NULL_MESSAGE;
      Ready := False;
      if not CS.Valid (Spec) or else not CuBit.Handles.Valid (Through.Service) then
         return;
      end if;
      if not Lent then
         CuBit.Memory_Grants.Create_Via_Capability
           (Through.Service, Request'Address, Request_Pages, False, Lent_Reference, Success);
         if not Success then
            return;
         end if;
         Lent := True;
      end if;
      CS.Encode (Spec, Bytes, Length);
      for K in 1 .. Length loop
         Request (K) := Bytes (K);
      end loop;
      Msg.tag := (label => CS.Start_Operation, length => CS.Request_Words,
                  protocol => 0, ipc => <>, reserved => 0);
      Msg.words := [0 => CuBit.Grant_References.Encode (Lent_Reference),
                    1 => Unsigned_64 (Length), others => 0];
      Ready := True;
   end Prepare;

   --  The receipt in a reply message.
   function Decode_Reply (Label : Unsigned_32; Words : Message) return Receipt is
      Result : Receipt;
   begin
      if Label = CuBit.Kernel_ABI.Reply_OK
        and then CuBit.Process_IDs.Is_Process (CuBit.Process_IDs.From_Word (Words.words (0)))
      then
         Result.Kind := Started;
         Result.Child := CuBit.Process_IDs.From_Word (Words.words (0));
      elsif Label = CuBit.Kernel_ABI.Reply_Error and then CS.Is_Refusal (Words.words (0)) then
         Result.Kind := Refused;
         Result.Why := CS.Refusal_Of (Words.words (0));
         if LA.Is_Failure (Words.words (1)) then
            Result.Failure := LA.Failure_Of (Words.words (1));
         end if;
      else
         Result.Kind := No_Answer;
      end if;
      return Result;
   end Decode_Reply;

   --  The sink a started receipt carries, if any (anything else is closed).
   function Sink_Of (Reply_Message : Message) return CuBit.Handles.Endpoint is
      Received : constant CuBit.Handles.IPC.Carried := CuBit.Handles.IPC.Carried_By (Reply_Message);
      Taken : constant CuBit.Handles.IPC.Accepted_Endpoint := CuBit.Handles.IPC.Accept_Endpoint (Received);
      Ignore : CuBit.Handles.IPC.Close_Result;
   begin
      if Taken.OK then
         return Taken.Value;
      end if;
      Ignore := CuBit.Handles.IPC.Close (Received);
      return CuBit.Handles.No_Endpoint;
   end Sink_Of;

   procedure Start (Through : Starter; Spec : CS.Spec; Deadline : Unsigned_64; Result : out Receipt) is
      use type CS.Sink_Kind;
      Msg : Message;
      Ready : Boolean;
      Tag : MessageTag;
      Received : CuBit.Handles.IPC.Carried;
   begin
      Result := (others => <>);
      Prepare (Through, Spec, Msg, Ready);
      if not Ready then
         return;
      end if;
      if Spec.Sink = CS.Write_Only_Sink then
         Tag := CuBit.Handles.IPC.Call_Accepting (Through.Service, Msg, Deadline, Received);
      else
         Tag := capCall (Through.Service, Msg, Deadline);
      end if;
      Result := Decode_Reply (Tag.label, Msg);
      if Spec.Sink = CS.Write_Only_Sink and then Result.Kind = Started then
         Result.Sink := Sink_Of (Msg);
      end if;
   end Start;

   procedure Submit (Through : Starter; Spec : CS.Spec; Token : Unsigned_64; Sent : out Boolean) is
      use type CS.Sink_Kind;
      Msg : Message;
      Ready : Boolean;
   begin
      Prepare (Through, Spec, Msg, Ready);
      Sent := Ready and then
        (if Spec.Sink = CS.Write_Only_Sink
         then CuBit.Handles.IPC.Submit_Accepting (Through.Service, Msg, Token)
         else capSubmit (Through.Service, Msg, Token));
   end Submit;

   function Receipt_Of (Completion : CuBit.Messages.CompletionEntry) return Receipt is
      Result : Receipt;
   begin
      if Completion.status /= COMPLETION_OK then
         return (Kind => No_Answer, others => <>);
      end if;
      Result := Decode_Reply (Completion.msg.tag.label, Completion.msg);
      if Result.Kind = Started then
         Result.Sink := Sink_Of (Completion.msg);
      else
         declare
            Ignore : constant CuBit.Handles.IPC.Close_Result :=
              CuBit.Handles.IPC.Close (CuBit.Handles.IPC.Carried_By (Completion.msg));
         begin
            null;
         end;
      end if;
      return Result;
   end Receipt_Of;

   procedure Submit_Release
     (Through : Starter; Child : CuBit.Process_IDs.Process_ID; Decision : CS.Release_Decision;
      Token : Unsigned_64; Sent : out Boolean)
   is
   begin
      Sent := False;
      if not CuBit.Handles.Valid (Through.Service) or else not CuBit.Process_IDs.Is_Process (Child) then
         return;
      end if;
      Sent := capSubmit (Through.Service, Release_Message (Child, Decision), Token);
   end Submit_Release;

   function Release_Message
     (Child : CuBit.Process_IDs.Process_ID; Decision : CS.Release_Decision) return Message
   is
      Msg : Message := NULL_MESSAGE;
   begin
      Msg.tag := (label => CS.Release_Operation, length => CS.Release_Words,
                  protocol => 0, ipc => <>, reserved => 0);
      Msg.words := [0 => CuBit.Process_IDs.To_Word (Child),
                    1 => CS.Release_Decision'Enum_Rep (Decision), others => 0];
      return Msg;
   end Release_Message;

   function Release_Answer (Label : Unsigned_32; Reply_Message : Message) return Release_Receipt is
     (if Label = CuBit.Kernel_ABI.Reply_OK then (Kind => Released, others => <>)
      elsif Label = CuBit.Kernel_ABI.Reply_Error and then CS.Is_Release_Refusal (Reply_Message.words (0))
      then (Kind => Release_Refused, Why => CS.Release_Refusal_Of (Reply_Message.words (0)))
      else (Kind => Release_No_Answer, others => <>));

   procedure Release
     (Through : Starter; Child : CuBit.Process_IDs.Process_ID; Decision : CS.Release_Decision;
      Deadline : Unsigned_64; Result : out Release_Receipt)
   is
      Msg : Message;
      Tag : MessageTag;
   begin
      Result := (others => <>);
      if not CuBit.Handles.Valid (Through.Service) or else not CuBit.Process_IDs.Is_Process (Child) then
         return;
      end if;
      Msg := Release_Message (Child, Decision);
      Tag := capCall (Through.Service, Msg, Deadline);
      Result := Release_Answer (Tag.label, Msg);
   end Release;

   function Release_Receipt_Of (Completion : CuBit.Messages.CompletionEntry) return Release_Receipt is
   begin
      if Completion.status /= COMPLETION_OK then
         return (Kind => Release_No_Answer, others => <>);
      end if;
      return Release_Answer (Completion.msg.tag.label, Completion.msg);
   end Release_Receipt_Of;

   function Image (Item : Release_Receipt) return String is
     (case Item.Kind is
        when Released          => "released",
        when Release_Refused   => "release refused " & CS.Release_Refusal_Name (Item.Why),
        when Release_No_Answer => "release not answered",
        when Release_Not_Sent  => "release not sent");

   function Image (Item : Receipt) return String is
     (case Item.Kind is
        when Started   => "started pid " & CuBit.Process_IDs.Text.Image (Item.Child),
        when Refused   => "refused " & CS.Refusal_Name (Item.Why) &
                          (if Item.Why = CS.Launch_Refused
                           then " (launch failure" & Unsigned_64'Image (LA.Launch_Failure'Enum_Rep (Item.Failure)) & ")"
                           else ""),
        when Not_Sent  => "not sent",
        when No_Answer => "no answer");
end CuBit.Child_Starts;
