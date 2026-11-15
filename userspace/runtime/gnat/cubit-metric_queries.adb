pragma Ada_2022;
with CuBit.Grant_References;
with CuBit.Memory_Grants;
with CuBit.Channel_Protocol;
with CuBit.Messages;
with CuBit.Metric_Updates;

package body CuBit.Metric_Queries is
   use CuBit.Messages;
   package Protocol renames CuBit.Metric_Protocol;

   type Lent_Page is new Pages.Page with Alignment => Pages.Page_Bytes;

   function Bound_To (Target : CuBit.Handles.Startup.Optional_Endpoint) return Reader is
     (Slot => (if Target.Present then Target.Value else CuBit.Handles.No_Endpoint));
   function Available (Item : Reader) return Boolean is (CuBit.Handles.Valid (Item.Slot));

   procedure Put_Selector (Into : in out Pages.Page; Match : Name_Match; Name, Source : String);
   procedure Put_Selector (Into : in out Pages.Page; Match : Name_Match; Name, Source : String) is
   begin
      Into (Pages.Select_Match) := Unsigned_64 (Name_Match'Enum_Rep (Match));
      Pages.Put_Text (Into, Pages.Select_Name, Name, Pages.Text_Bytes);
      Pages.Put_Text (Into, Pages.Select_Source, Source, Pages.Text_Bytes);
   end Put_Selector;

   --  Lend Lent, call Op with Words (word 0 becomes the loan), take it back.
   procedure Call
     (Item : Reader; Op : Protocol.Operation; Lent : in out Lent_Page; W1, W2, W3 : Unsigned_64;
      Reply : out MessageWords; Result : out Status);
   procedure Call
     (Item : Reader; Op : Protocol.Operation; Lent : in out Lent_Page; W1, W2, W3 : Unsigned_64;
      Reply : out MessageWords; Result : out Status)
   is
      Loan : CuBit.Memory_Grants.Grant_Reference;
      Granted, Revoked : Boolean;
      Msg : Message := NULL_MESSAGE;
      Tag : MessageTag;
   begin
      Reply := [others => 0];
      Result := Protocol.Unavailable;
      if not CuBit.Handles.Valid (Item.Slot) then
         return;
      end if;
      CuBit.Memory_Grants.Create_Via_Capability (Item.Slot, Lent'Address, 1, True, Loan, Granted);
      if not Granted then
         return;
      end if;
      Msg.tag := (label => Protocol.Operation'Enum_Rep (Op), length => Protocol.Message_Words,
                  protocol => 0, ipc => <>, reserved => 0);
      Msg.words := [CuBit.Grant_References.Encode (Loan), W1, W2, W3];
      Tag := capCall (Item.Slot, Msg, Deadline_After (Call_Wait_Ms));
      CuBit.Memory_Grants.Revoke (Loan, Revoked);
      if Ending_Of (Tag) /= Call_Answered or else not Revoked then
         return;
      end if;
      for Candidate in Protocol.Status loop
         if Tag.label = Protocol.Status'Enum_Rep (Candidate) then
            Result := Candidate;
         end if;
      end loop;
      Reply := Msg.words;
   end Call;

   procedure List
     (Item : Reader; Match : Name_Match; Name, Source : String; Skip : Unsigned_64;
      Answer : out Pages.Page; Rows, Total : out Unsigned_64; Result : out Status)
   is
      Lent : aliased Lent_Page := [others => 0];
      Reply : MessageWords;
   begin
      Put_Selector (Pages.Page (Lent), Match, Name, Source);
      Call (Item, Protocol.List_Series, Lent, Skip, 0, 0, Reply, Result);
      Answer := Pages.Page (Lent);
      Rows := Unsigned_64'Min (Reply (0), Pages.List_Rows);
      Total := Reply (1);
   end List;

   procedure Read_Range
     (Item : Reader; Id, From_Us, To_Us, Skip, Step_Us : Unsigned_64;
      Answer : out Pages.Page; Rows, Total, Gaps, Resolution_Us : out Unsigned_64; Result : out Status)
   is
      Lent : aliased Lent_Page := [others => 0];
      Reply : MessageWords;
   begin
      Lent (Pages.Range_Skip) := Skip;
      Lent (Pages.Range_Step) := Step_Us;
      Call (Item, Protocol.Read_Range, Lent, Id, From_Us, To_Us, Reply, Result);
      Answer := Pages.Page (Lent);
      Rows := Unsigned_64'Min (Reply (0), (if Step_Us = 0 then Pages.Raw_Rows else Pages.Window_Rows));
      Total := Reply (1);
      Gaps := Unsigned_64'Min (Reply (2), Pages.Range_Gap_Rows);
      Resolution_Us := Reply (3);
   end Read_Range;

   procedure Read_Window
     (Item : Reader; Id : Unsigned_64; Match : Name_Match; Name, Source : String;
      From_Us, To_Us : Unsigned_64; Answer : out Pages.Page; Merged : out Unsigned_64; Result : out Status)
   is
      Lent : aliased Lent_Page := [others => 0];
      Reply : MessageWords;
   begin
      Put_Selector (Pages.Page (Lent), Match, Name, Source);
      Call (Item, Protocol.Read_Window, Lent, Id, From_Us, To_Us, Reply, Result);
      Answer := Pages.Page (Lent);
      Merged := Reply (0);
   end Read_Window;

   procedure Subscribe
     (Item : Reader; Match : Name_Match; Name, Source : String; Link : in out CuBit.Channels.Channel;
      Result : out Status)
   is
      use type CuBit.Channels.Open_Result;
      use type Protocol.Status;
      Lent : aliased Lent_Page := [others => 0];
      Reply : MessageWords;
      Opened : CuBit.Channels.Open_Result;
      Ignore_Refusal : CuBit.Channel_Protocol.Open_Refusal;
   begin
      Result := Protocol.Unavailable;
      if not CuBit.Handles.Valid (Item.Slot) then
         return;
      end if;
      CuBit.Channels.Open (Item.Slot, CuBit.Metric_Updates.CONTRACT, CuBit.Channels.Consuming, Link, Opened,
                           Ignore_Refusal, Deadline => Deadline_After (Open_Wait_Ms));
      if Opened /= CuBit.Channels.Opened then
         return;
      end if;
      Put_Selector (Pages.Page (Lent), Match, Name, Source);
      Call (Item, Protocol.Subscribe, Lent, Link.Peer_Number, 0, 0, Reply, Result);
      if Result /= Protocol.OK then
         CuBit.Channels.Close (Link);
      end if;
   end Subscribe;
end CuBit.Metric_Queries;
