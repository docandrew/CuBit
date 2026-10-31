pragma Ada_2022;
with Interfaces;
generic
   Maximum_Contacts : Positive;
   type Contact is private;
   Empty_Contact : Contact;
   with function Identity (Item : Contact) return Interfaces.Unsigned_32;
package HID_Contact_Frames with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_16;
   subtype Scan_Time is Interfaces.Unsigned_16;
   subtype Declared_Count is Interfaces.Unsigned_32;
   type Contact_Count is new Natural range 0 .. Maximum_Contacts;
   subtype Contact_Limit is Contact_Count range 1 .. Contact_Count'Last;
   type Contact_Array is array (Positive range <>) of Contact;
   subtype Frame_Items is Contact_Array (1 .. Maximum_Contacts);
   type Frame is record
      Count : Contact_Count := 0;
      Time : Scan_Time := 0;
      Items : Frame_Items := [others => Empty_Contact];
   end record;
   type Status is (Frame_Complete, Frame_Pending, Not_Configured,
                   Count_Refused, Invalid_Slots, Scan_Mismatch, Duplicate_Contact);
   type Report is record
      Outcome : Status := Not_Configured;
      Discarded_Partial : Boolean := False;
   end record;
   function Unique (Items : Frame_Items; Count : Contact_Count) return Boolean with Ghost;
   type State is limited private with Default_Initial_Condition => Valid (State);
   function Valid (S : State) return Boolean with Ghost;
   function Maximum_Admitted (S : State) return Contact_Count;
   procedure Configure (S : in out State; Limit : Contact_Limit)
     with Pre => Valid (S), Post => Valid (S) and then Configured (S)
       and then Maximum_Admitted (S) = Limit and then not Pending (S);
   -- Explicit admitted Contact Count Maximum, bounded by caller-chosen storage.
   -- Reconfiguration discards partial input; no hardware feature is guessed.
   procedure Reset (S : in out State)
     with Pre => Valid (S), Post => Valid (S) and then not Pending (S)
       and then Configured (S) = Configured (S)'Old
       and then Maximum_Admitted (S) = Maximum_Admitted (S)'Old;
   -- Transport loss/reset discards partial input, retaining admitted maximum.
   function Configured (S : State) return Boolean;
   function Pending (S : State) return Boolean;
   function Expected (S : State) return Contact_Count;
   function Collected (S : State) return Contact_Count;
   function Pending_Time (S : State) return Scan_Time with Pre => Pending (S);
   procedure Feed
     (S : in out State; Time : Scan_Time; Declared : Declared_Count;
      Slots : Contact_Array; Complete : out Frame; Info : out Report)
     with Pre => Valid (S), Post => Valid (S)
       and then Configured (S) = Configured (S)'Old
       and then Maximum_Admitted (S) = Maximum_Admitted (S)'Old
       and then (if Info.Outcome = Frame_Complete then
          Complete.Count <= Maximum_Admitted (S) and then Complete.Time = Time
          and then Unique (Complete.Items, Complete.Count) and then not Pending (S)
        else Complete.Count = 0 and then Complete.Time = 0
          and then (for all K in Complete.Items'Range => Complete.Items (K) = Empty_Contact))
       and then (if Info.Outcome = Frame_Pending then
          Pending (S) and then Collected (S) > 0 and then Collected (S) < Expected (S)
        else not Pending (S));
   -- Slots contain successfully decoded contacts, including tip-up contacts.
   -- Positive Declared starts a frame; zero continues a pending frame with the
   -- same scan time, or completes an empty frame when idle. Consume only the
   -- required slots; remaining descriptor slots are padding. Parallel, serial
   -- and hybrid packets share this rule. Duplicate identities invalidate the
   -- entire frame. No pending or rejected contacts escape in Complete.
   -- New positive count replaces an incomplete frame, recording that loss.
   -- Identity must be pure/deterministic. Caller owns ordered per-device
   -- delivery, report decoding, timeout/reset and desktop pointer policy.
private
   type State is limited record
      Is_Configured : Boolean := False;
      Limit : Contact_Count := 0;
      Wanted, Used : Contact_Count := 0;
      Time : Scan_Time := 0;
      Items : Frame_Items := [others => Empty_Contact];
   end record;
   function Configured (S : State) return Boolean is (S.Is_Configured);
   function Pending (S : State) return Boolean is (S.Wanted /= 0);
   function Expected (S : State) return Contact_Count is (S.Wanted);
   function Collected (S : State) return Contact_Count is (S.Used);
   function Pending_Time (S : State) return Scan_Time is (S.Time);
   function Maximum_Admitted (S : State) return Contact_Count is (S.Limit);
   function Unique (Items : Frame_Items; Count : Contact_Count) return Boolean is
     (for all I in Contact_Count range 1 .. Count =>
        (for all J in Contact_Count range 1 .. I - 1 =>
           Identity (Items (Positive (I))) /= Identity (Items (Positive (J)))));
   function Valid (S : State) return Boolean is
     (S.Is_Configured = (S.Limit /= 0) and then S.Used <= S.Wanted
      and then S.Wanted <= S.Limit and then Unique (S.Items, S.Used)
      and then (if S.Wanted = 0 then S.Time = 0));
end HID_Contact_Frames;
