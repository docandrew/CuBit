------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System;
with CuBit.Async_Requests;
with CuBit.Handles;
with CuBit.Kernel_ABI;
with CuBit.Memory_Grants;
with CuBit.Grant_References;
with CuBit.Net_Manager_Control;
with Desktop_Endpoints;
with Desktop_Status_Machines;

package body Desktop_Network_Refresh is
   use Interfaces;
   use CuBit.Messages;
   package AR renames CuBit.Async_Requests;
   package SM renames Desktop_Status_Machines;
   package WR renames CuBit.Wifi_Report;
   package NMR renames CuBit.Net_Manager_Report;
   package NMC renames CuBit.Net_Manager_Control;
   package MG renames CuBit.Memory_Grants;
   package TF renames CuBit.UI.Editor;
   use type AR.Phase, SM.Loan_State, SM.Loan_Action, SM.Watch_Action, SM.Outcome, SM.Watch_Phase;

   Reply_OK  : constant Unsigned_32 := CuBit.Kernel_ABI.Reply_OK;
   Reply_Err : constant Unsigned_32 := CuBit.Kernel_ABI.Reply_Error;
   Page_Bytes : constant := 4_096;
   pragma Compile_Time_Error
     (C.Page_Bytes /= Page_Bytes or else NMC.Page_Bytes /= Page_Bytes, "one page each");

   ---------------------------------------------------------------------------
   --  The lent pages, each a whole page of its own (nothing else of
   --  Desktop's shares it). The watch pages are lent read-write, the secret
   --  and apply pages read-only, each for one request.
   ---------------------------------------------------------------------------
   type Wifi_Words is array (WR.Word_Index) of Unsigned_32 with Volatile_Components;
   type Wifi_Page_Type is record
      Words : Wifi_Words;
   end record with Alignment => Page_Bytes, Size => Page_Bytes * 8;
   type Net_Page_Type is record
      Words : NMR.Words;
   end record with Alignment => Page_Bytes, Size => Page_Bytes * 8, Volatile;
   type Text_Page_Type is record
      Text : String (1 .. Page_Bytes);
   end record with Alignment => Page_Bytes, Size => Page_Bytes * 8, Volatile;

   Wifi_Page   : Wifi_Page_Type;
   Net_Page    : Net_Page_Type;
   Secret_Page : Text_Page_Type := (Text => [others => ASCII.NUL]);
   Apply_Page  : Text_Page_Type := (Text => [others => ASCII.NUL]);

   --  A page's grant and its Loan machine.
   type Loan is record
      State     : SM.Loan_State := SM.Free;
      Reference : MG.Grant_Reference;
   end record;
   Secret_Loan, Apply_Loan : Loan;

   procedure Lend (To : CuBit.Handles.Endpoint; At_Address : System.Address; Writable : Boolean;
                   L : in out Loan; Lent : out Boolean) is
      Act : SM.Loan_Action;
   begin
      Lent := False;
      if L.State = SM.Free then
         MG.Create_Via_Capability (To, At_Address, 1, Writable, L.Reference, Lent);
         SM.Step (L.State, (if Lent then SM.Lend_Done else SM.Lend_Failed), Act);
      end if;
   end Lend;

   procedure Request_Over (L : in out Loan) is
      Act : SM.Loan_Action;
      Ignore : Boolean;
   begin
      SM.Step (L.State, SM.Request_Over, Act);
      if Act = SM.Revoke then
         MG.Revoke (L.Reference, Ignore);
      end if;
   end Request_Over;

   procedure Check_Retired (L : in out Loan) is
      Act : SM.Loan_Action;
   begin
      if L.State = SM.Retiring and then MG.Retirement_Confirmed (L.Reference) then
         SM.Step (L.State, SM.Retired, Act);
      end if;
   end Check_Retired;

   ---------------------------------------------------------------------------
   --  Flights (CuBit.Async_Requests: any completion returns one to Idle).
   ---------------------------------------------------------------------------
   function Token_Of (F : AR.Tracker) return Unsigned_64 is
     (if AR.State (F) = AR.In_Flight then AR.Pending_Token (F) else 0);

   function Submit (Flight : in out AR.Tracker; Service : CuBit.Handles.Endpoint; Msg : Message;
                    Sequence : in out Unsigned_64) return Boolean is
      Accepted : Boolean;
   begin
      if Sequence >= Unsigned_64'Last - 1 or else not CuBit.Handles.Valid (Service) then
         return False;
      end if;
      Sequence := Sequence + 1;
      AR.Reserve (Flight, Sequence, Accepted);
      if not Accepted then
         return False;
      end if;
      Accepted := capSubmit (Service, Msg, Sequence);
      AR.Submitted (Flight, Accepted);
      return Accepted;
   end Submit;

   procedure Drain (Flight : in out AR.Tracker; E : CompletionEntry) is
      Captured : Boolean;
   begin
      AR.Capture (Flight, E.token, True, Captured);
      if Captured then
         AR.Release (Flight);
      end if;
   end Drain;

   function Request (Label : Unsigned_32; Length : Natural; W0, W1, W2, W3 : Unsigned_64 := 0)
     return Message is
     ((tag => (label => Label, length => Unsigned_8 (Length), protocol => 0, ipc => <>, reserved => 0),
       authorityTag => 0, words => [W0, W1, W2, W3]));

   ---------------------------------------------------------------------------
   --  Watches
   ---------------------------------------------------------------------------
   type Watch is record
      Machine   : SM.Watch_State;
      Flight    : AR.Tracker;
      Reference : MG.Grant_Reference;
      Seen      : Unsigned_32 := 0;
      Fresh     : Boolean := False;
   end record;
   Wifi_Watch, Net_Watch : Watch;
   Wifi_Value : WR.Report;
   Net_Value  : NMR.Report;
   Now_Ms : Unsigned_64 := 0;

   function Wifi_Endpoint return CuBit.Handles.Endpoint is (Desktop_Endpoints.Wifi_Service);
   function Net_Endpoint return CuBit.Handles.Endpoint is (Desktop_Endpoints.Net_Manager);

   function Wifi_Granted return Boolean is (CuBit.Handles.Valid (Wifi_Endpoint));
   function Net_Granted return Boolean is (CuBit.Handles.Valid (Net_Endpoint));

   --  Carry out a watch machine's action (and feed back what happened).
   procedure Perform (W : in out Watch; Act : SM.Watch_Action; Service : CuBit.Handles.Endpoint;
                      Label : Unsigned_32; Page : System.Address; Sequence : in out Unsigned_64) is
      Again : SM.Watch_Action;
      Lent : Boolean := True;
      Ignore : Boolean;
   begin
      case Act is
         when SM.None =>
            return;
         when SM.Revoke =>
            MG.Revoke (W.Reference, Ignore);
            return;
         when SM.Lend_And_Submit =>
            MG.Create_Via_Capability (Service, Page, 1, True, W.Reference, Lent);
            if not Lent then
               SM.Step (W.Machine, (Kind => SM.Lend_Failed, Now => Now_Ms), Again);
               return;
            end if;
            W.Seen := 0;   --  a new page holds no report yet
         when SM.Submit =>
            null;
      end case;
      if not Submit (W.Flight, Service, Request (Label, 2, CuBit.Grant_References.Encode (W.Reference),
                                                 Unsigned_64 (W.Seen)), Sequence)
      then
         SM.Step (W.Machine, (Kind => SM.Done, Now => Now_Ms, Result => SM.Not_Delivered), Again);
         Perform (W, Again, Service, Label, Page, Sequence);
      end if;
   end Perform;

   procedure Keep (W : in out Watch; Service : CuBit.Handles.Endpoint; Label : Unsigned_32;
                   Page : System.Address; Sequence : in out Unsigned_64) is
      Act : SM.Watch_Action;
   begin
      if not CuBit.Handles.Valid (Service) then
         return;
      end if;
      if W.Machine.Loan = SM.Retiring and then MG.Retirement_Confirmed (W.Reference) then
         SM.Step (W.Machine, (Kind => SM.Retired, Now => Now_Ms), Act);
      end if;
      SM.Step (W.Machine, (Kind => SM.Tick, Now => Now_Ms), Act);
      Perform (W, Act, Service, Label, Page, Sequence);
   end Keep;

   procedure Start (Now : Unsigned_64; Sequence : in out Unsigned_64) is
   begin
      Now_Ms := Now;
      Keep (Wifi_Watch, Wifi_Endpoint, C.Label (C.Watch), Wifi_Page'Address, Sequence);
      Keep (Net_Watch, Net_Endpoint, NMC.Label (NMC.Watch), Net_Page'Address, Sequence);
      Check_Retired (Secret_Loan);
      Check_Retired (Apply_Loan);
   end Start;

   ---------------------------------------------------------------------------
   --  Wi-Fi commands
   ---------------------------------------------------------------------------
   Wifi_Flight, Net_Flight : AR.Tracker;
   Wifi_Doing : Wifi_Command := None;
   Wifi_Done  : Wifi_Result;

   type Net_Step is (Idle, Applying, Setting_DNS);
   Net_Doing : Net_Step := Idle;
   DNS_Pending : Boolean := False;
   Pending_DNS_1, Pending_DNS_2 : Unsigned_32 := 0;
   Net_Done : Net_Result;

   function Matches (Token : Unsigned_64) return Boolean is
     (Token /= 0 and then
      (Token = Token_Of (Wifi_Watch.Flight) or else Token = Token_Of (Net_Watch.Flight)
       or else Token = Token_Of (Wifi_Flight) or else Token = Token_Of (Net_Flight)));

   --  The secret page is free too: a passphrase is never written into a
   --  page wifi.svc may still map.
   function Wifi_Free return Boolean is
     (Wifi_Granted and then AR.State (Wifi_Flight) = AR.Idle and then Secret_Loan.State = SM.Free);

   procedure Send_Wifi (What : Wifi_Command; Msg : Message; Sequence : in out Unsigned_64) is
   begin
      if Submit (Wifi_Flight, Wifi_Endpoint, Msg, Sequence) then
         Wifi_Doing := What;
      else
         Wifi_Done := (Done => What, Delivered => False, OK => False, Why_Not => C.Malformed);
      end if;
   end Send_Wifi;

   procedure Store_Passphrase (Name : C.SSID; Field : in out TF.Edit_State; Sequence : in out Unsigned_64) is
      Length : constant Natural := TF.Length (Field);
      Lent : Boolean;
   begin
      if not Wifi_Free or else Length not in C.Passphrase_Length or else not C.Normal (Name) then
         TF.Wipe (Field);
         Wifi_Done := (Done => Store_Passphrase, Delivered => False, OK => False, Why_Not => C.Malformed);
         return;
      end if;
      for I in 1 .. Name.Length loop
         Secret_Page.Text (I) := Character'Val (Name.Octets (I));
      end loop;
      Secret_Page.Text (Name.Length + 1 .. Name.Length + Length) := TF.Content (Field);
      TF.Wipe (Field);
      Lend (Wifi_Endpoint, Secret_Page'Address, False, Secret_Loan, Lent);
      if not Lent then
         Secret_Page.Text := [others => ASCII.NUL];
         Wifi_Done := (Done => Store_Passphrase, Delivered => False, OK => False, Why_Not => C.Malformed);
         return;
      end if;
      Send_Wifi (Store_Passphrase,
                 Request (C.Label (C.Set_Passphrase), 3, Secret_Loan.Reference.slot,
                          Secret_Loan.Reference.generation, C.Lengths_Word (Name.Length, Length)),
                 Sequence);
      if Wifi_Doing /= Store_Passphrase then
         Request_Over (Secret_Loan);
         Secret_Page.Text := [others => ASCII.NUL];
      end if;
   end Store_Passphrase;

   procedure Named (What : Wifi_Command; Label : Unsigned_32; Name : C.SSID; Sequence : in out Unsigned_64) is
   begin
      if not Wifi_Free or else not C.Normal (Name) then
         Wifi_Done := (Done => What, Delivered => False, OK => False, Why_Not => C.Malformed);
         return;
      end if;
      declare
         W : constant C.Words4 := C.Encode (Name);
      begin
         Send_Wifi (What, Request (Label, C.SSID_Words, W (0), W (1), W (2), W (3)), Sequence);
      end;
   end Named;

   procedure Join (Name : C.SSID; Sequence : in out Unsigned_64) is
   begin
      Named (Join, C.Label (C.Join), Name, Sequence);
   end Join;

   procedure Forget (Name : C.SSID; Sequence : in out Unsigned_64) is
   begin
      Named (Forget, C.Label (C.Forget), Name, Sequence);
   end Forget;

   procedure Refresh (Sequence : in out Unsigned_64) is
   begin
      if Wifi_Free then
         Send_Wifi (Refresh, Request (C.Label (C.Refresh), 0), Sequence);
      end if;
   end Refresh;

   procedure Take_Wifi_Result (Result : out Wifi_Result) is
   begin
      Result := Wifi_Done;
      Wifi_Done := (others => <>);
   end Take_Wifi_Result;

   ---------------------------------------------------------------------------
   --  netmgr commands
   ---------------------------------------------------------------------------
   function Net_Free return Boolean is
     (Net_Granted and then AR.State (Net_Flight) = AR.Idle and then Net_Doing = Idle
      and then Apply_Loan.State = SM.Free);

   procedure Send_DNS (Sequence : in out Unsigned_64) is
   begin
      DNS_Pending := False;
      if Submit (Net_Flight, Net_Endpoint,
                 Request (NMC.Label (NMC.Set_DNS), 3, Unsigned_64 (Pending_DNS_1), Unsigned_64 (Pending_DNS_2),
                          Unsigned_64 (NMC.DNS_Choice'Enum_Rep
                            (if Pending_DNS_1 = 0 then NMC.Use_Settings else NMC.Use_These))),
                 Sequence)
      then
         Net_Doing := Setting_DNS;
      else
         Net_Doing := Idle;
         Net_Done := (Fresh => True, Delivered => False, OK => False);
      end if;
   end Send_DNS;

   procedure Apply (Name, Source : String; DNS_Changed : Boolean; DNS_1, DNS_2 : Unsigned_32;
                    Sequence : in out Unsigned_64) is
      Lent : Boolean;
   begin
      if not Net_Free then
         Net_Done := (Fresh => True, Delivered => False, OK => False);
         return;
      end if;
      Apply_Page.Text (1 .. Name'Length) := Name;
      Apply_Page.Text (Name'Length + 1 .. Name'Length + Source'Length) := Source;
      DNS_Pending := DNS_Changed;
      Pending_DNS_1 := DNS_1;
      Pending_DNS_2 := DNS_2;
      Lend (Net_Endpoint, Apply_Page'Address, False, Apply_Loan, Lent);
      if Lent
        and then Submit (Net_Flight, Net_Endpoint,
                         Request (NMC.Label (NMC.Apply), 2, CuBit.Grant_References.Encode (Apply_Loan.Reference),
                                  NMC.Lengths_Word (Name'Length, Source'Length)),
                         Sequence)
      then
         Net_Doing := Applying;
      else
         Request_Over (Apply_Loan);
         DNS_Pending := False;
         Net_Done := (Fresh => True, Delivered => False, OK => False);
      end if;
   end Apply;

   procedure Take_Net_Result (Result : out Net_Result) is
   begin
      Result := Net_Done;
      Net_Done := (others => <>);
   end Take_Net_Result;

   ---------------------------------------------------------------------------
   --  Completions
   ---------------------------------------------------------------------------
   --  Decode the page into the report; Answered when it did.
   function Read_Wifi (W : in out Watch) return Boolean is
      Copy : WR.Words;
   begin
      for I in Copy'Range loop
         Copy (I) := Wifi_Page.Words (I);
      end loop;
      declare
         D : constant WR.Decoded_Report := WR.Decode (Copy);
      begin
         if D.Valid then
            Wifi_Value := D.Item;
            W.Seen := D.Item.Generation;
         end if;
         return D.Valid;
      end;
   end Read_Wifi;

   function Read_Net (W : in out Watch) return Boolean is
      Copy : constant NMR.Words := Net_Page.Words;
      D : constant NMR.Decoded_Report := NMR.Decode (Copy);
   begin
      if D.Valid then
         Net_Value := D.Item;
         W.Seen := D.Item.Generation;
      end if;
      return D.Valid;
   end Read_Net;

   procedure Collect_Watch (W : in out Watch; E : CompletionEntry; Is_Wifi : Boolean;
                            Sequence : in out Unsigned_64) is
      Delivered : constant Boolean := E.valid and then E.status = COMPLETION_OK;
      Result : SM.Outcome;
      Act : SM.Watch_Action;
   begin
      Drain (W.Flight, E);
      if not Delivered then
         Result := SM.Not_Delivered;
      elsif E.msg.tag.label /= Reply_OK then
         Result := SM.Refused;
      elsif (if Is_Wifi then Read_Wifi (W) else Read_Net (W)) then
         Result := SM.Answered;
      else
         Result := SM.Refused;
      end if;
      SM.Step (W.Machine, (Kind => SM.Done, Now => Now_Ms, Result => Result), Act);
      W.Fresh := True;
      if Is_Wifi then
         Perform (W, Act, Wifi_Endpoint, C.Label (C.Watch), Wifi_Page'Address, Sequence);
      else
         Perform (W, Act, Net_Endpoint, NMC.Label (NMC.Watch), Net_Page'Address, Sequence);
      end if;
   end Collect_Watch;

   procedure Collect (C_Entry : CompletionEntry; Sequence : in out Unsigned_64) is
      Delivered : constant Boolean := C_Entry.valid and then C_Entry.status = COMPLETION_OK;
      OK : constant Boolean := Delivered and then C_Entry.msg.tag.label = Reply_OK;
   begin
      if C_Entry.token = Token_Of (Wifi_Watch.Flight) then
         Collect_Watch (Wifi_Watch, C_Entry, True, Sequence);
      elsif C_Entry.token = Token_Of (Net_Watch.Flight) then
         Collect_Watch (Net_Watch, C_Entry, False, Sequence);
      elsif C_Entry.token = Token_Of (Wifi_Flight) then
         Drain (Wifi_Flight, C_Entry);
         if Wifi_Doing = Store_Passphrase then
            --  Wiped whatever the reply; the page is lent again only once
            --  the kernel confirms the grant retired.
            Secret_Page.Text := [others => ASCII.NUL];
            Request_Over (Secret_Loan);
         end if;
         Wifi_Done := (Done => Wifi_Doing, Delivered => Delivered, OK => OK,
                       Why_Not => (if Delivered and then C_Entry.msg.tag.label = Reply_Err
                                     and then C_Entry.msg.words (0) in 1 .. 7
                                   then C.Refusal'Enum_Val (C_Entry.msg.words (0)) else C.Malformed));
         Wifi_Doing := None;
      elsif C_Entry.token = Token_Of (Net_Flight) then
         Drain (Net_Flight, C_Entry);
         if Net_Doing = Applying then
            Request_Over (Apply_Loan);
            if OK and then DNS_Pending then
               Send_DNS (Sequence);
               return;
            end if;
         end if;
         Net_Doing := Idle;
         Net_Done := (Fresh => True, Delivered => Delivered, OK => OK);
      end if;
   end Collect;

   procedure Take_Wifi (Report : out WR.Report; Have, Fresh : out Boolean) is
   begin
      Report := Wifi_Value;
      Have := Wifi_Watch.Machine.Shown;
      Fresh := Wifi_Watch.Fresh;
      Wifi_Watch.Fresh := False;
   end Take_Wifi;

   procedure Take_Net (Report : out NMR.Report; Have, Fresh : out Boolean) is
   begin
      Report := Net_Value;
      Have := Net_Watch.Machine.Shown;
      Fresh := Net_Watch.Fresh;
      Net_Watch.Fresh := False;
   end Take_Net;
end Desktop_Network_Refresh;
