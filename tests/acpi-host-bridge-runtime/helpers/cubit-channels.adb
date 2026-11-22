package body CuBit.Channels is
   use type CuBit.Control_Events.Event_Kind;
   procedure Open (Endpoint : CuBit.Handles.Endpoint; Item : CC.Contract; This_Side : Side;
      C : out Channel; Result : out Open_Result; Refusal : out CP.Open_Refusal;
      Connector : Unsigned_16 := 0; Deadline : Unsigned_64 := Unsigned_64'Last) is
      pragma Unreferenced (Endpoint, Connector);
   begin
      Open_Calls := Open_Calls + 1; Last_Deadline := Deadline;
      C := (Active => True, Opener => True, Item => Item, This_Side => This_Side,
        Own_Base => Own_Base, Own_Bytes => Unsigned_64 (Item.Pages)*4096, Peer_Base => Peer_Base,
        Own_Grant => Next_Own_Reference, Peer_Grant => Next_Peer_Reference, Has_Own => True, Has_Peer => True,
        others => <>);
      Result := Opened; Refusal := CP.Unsupported;
      case Mode is
         when Good => null;
         when Refuse_Open | Fail_Open =>
            C.Active := False; C.Has_Own := False; C.Has_Peer := False;
            C.Closed_Own_Present := True; C.Closed_Own_Grant := Next_Own_Reference;
            Result := (if Mode = Refuse_Open then Refused else Failed);
         when Bad_Alignment => C.Own_Base := C.Own_Base + 1;
         when Bad_Contract => C.Item.Element.Wire_Size := C.Item.Element.Wire_Size + 1;
         when Missing_Peer => C.Has_Peer := False;
         when Bad_Own_Length => C.Own_Bytes := C.Own_Bytes - 1;
         when Aliased_Regions => C.Peer_Base := C.Own_Base;
         when Overlapping_Regions => C.Peer_Base := C.Own_Base + 4096;
      end case;
   end Open;
   procedure Begin_Open
     (Endpoint : CuBit.Handles.Endpoint; Item : CC.Contract; This_Side : Side;
      Token : Unsigned_64; C : out Channel; Submitted : out Boolean;
      Connector : Unsigned_16 := 0) is
      Saved : constant Model_Mode := Mode;
      Status : Open_Result;
      Refusal : CP.Open_Refusal;
   begin
      Mode := Good;
      Open (Endpoint, Item, This_Side, C, Status, Refusal, Connector);
      Mode := Saved; Last_Token := Token;
      C.Active := False; C.Has_Peer := False;
      Submitted := Submit_OK;
      if not Submitted then Close (C); end if;
   end Begin_Open;
   procedure Finish_Open
     (C : in out Channel; Reply : CuBit.Messages.Message;
      Result : out Open_Result; Refusal : out CP.Open_Refusal) is
      pragma Unreferenced (Reply);
   begin
      Finish_Calls := Finish_Calls + 1;
      Result := Opened; Refusal := CP.Unsupported;
      C.Active := True; C.Has_Peer := True;
      case Mode is
         when Good => null;
         when Refuse_Open | Fail_Open =>
            Close (C); Result := (if Mode = Refuse_Open then Refused else Failed);
         when Bad_Alignment => C.Own_Base := C.Own_Base + 1;
         when Bad_Contract => C.Item.Element.Wire_Size := C.Item.Element.Wire_Size + 1;
         when Missing_Peer => C.Has_Peer := False;
         when Bad_Own_Length => C.Own_Bytes := C.Own_Bytes - 1;
         when Aliased_Regions => C.Peer_Base := C.Own_Base;
         when Overlapping_Regions => C.Peer_Base := C.Own_Base + 4096;
      end case;
   end Finish_Open;
   procedure Signal (C : Channel; Sync : Boolean; Result : out Signal_Result) is
   begin Signals := Signals + 1; Last_Sync := Sync; Last_Signalled := C.Own_Grant; Result := Signal_Answer; end Signal;
   function Ended (C : Channel; Event : CuBit.Control_Events.Event) return Boolean is
     (C.Active and then
       ((Event.Kind = CuBit.Control_Events.Grant_Revoked and then C.Has_Peer
          and then Event.Slot = C.Peer_Grant.slot and then Event.Generation = C.Peer_Grant.generation)
        or else (Event.Kind = CuBit.Control_Events.Grant_Returned and then C.Has_Own
          and then Event.Slot = C.Own_Grant.slot and then Event.Generation = C.Own_Grant.generation)));
   procedure Close (C : in out Channel) is
   begin
      Close_Calls := Close_Calls + 1;
      if C.Has_Own then C.Closed_Own_Present := True; C.Closed_Own_Grant := C.Own_Grant; end if;
      C.Active := False; C.Has_Own := False; C.Has_Peer := False;
   end Close;
end CuBit.Channels;
