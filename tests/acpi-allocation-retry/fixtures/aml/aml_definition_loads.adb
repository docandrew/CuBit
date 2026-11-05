package body AML_Definition_Loads with SPARK_Mode is
   function Active (Loads : State) return Boolean is
   begin
      for I in Loads.Entries'Range loop
         if Loads.Entries (I).Phase = Loading then return True; end if;
         pragma Loop_Invariant (for all J in Loads.Entries'First .. I => Loads.Entries (J).Phase /= Loading);
      end loop;
      return False;
   end Active;
   function Session_Admitted (Loads : State; Enclosing : Slot_Selection;
      Expected_Definition : Definition_Identity) return Boolean is
   begin
      if Enclosing = No_Slot then return not Active (Loads); end if;
      if Enclosing > Slot_Selection (Loads.Capacity) or else not Has_Definition (Expected_Definition)
      then return False; end if;
      return Loads.Entries (Positive (Enclosing)).Phase = Loading
        and then Loads.Entries (Positive (Enclosing)).Definition = Expected_Definition
        and then (for all I in Loads.Entries'Range =>
          (if I /= Positive (Enclosing) then Loads.Entries (I).Phase /= Loading));
   end Session_Admitted;
   procedure Reset (Loads : in out State) is
   begin Loads.Entries := [others => <>]; end Reset;
   procedure Reserve (Loads : in out State; Index : Positive; Accepted : out Boolean) is
   begin
      Accepted := Loads.Entries (Index).Phase = Retained;
      if Accepted then Loads.Entries (Index).Phase := Loading; end if;
   end Reserve;
   procedure Consume (Loads : in out State; Index : Positive;
      ID : Definition_Identity; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Loads.Entries (Index).Phase /= Loading
        or else Has_Definition (Loads.Entries (Index).Definition) or else not Has_Definition (ID) then return; end if;
      for I in Loads.Entries'Range loop
         if Loads.Entries (I).Definition = ID then return; end if;
         pragma Loop_Invariant
           (for all J in Loads.Entries'First .. I => Loads.Entries (J).Definition /= ID);
      end loop;
      Loads.Entries (Index).Definition := ID; Accepted := True;
   end Consume;
   procedure Finish (Loads : in out State; Index : Positive; Succeeded : Boolean;
      Accepted : out Boolean) is
   begin
      Accepted := Loads.Entries (Index).Phase = Loading;
      if not Accepted then return; end if;
      if not Has_Definition (Loads.Entries (Index).Definition) then Loads.Entries (Index).Phase := Retained;
      elsif Succeeded then Loads.Entries (Index).Phase := Loaded;
      else Loads.Entries (Index).Phase := Failed; end if;
   end Finish;
   procedure Require_Recovery (Loads : in out State; Index : Positive; Accepted : out Boolean) is
   begin
      Accepted := Loads.Entries (Index).Phase = Loaded;
      if Accepted then Loads.Entries (Index).Phase := Failed; end if;
   end Require_Recovery;
end AML_Definition_Loads;
