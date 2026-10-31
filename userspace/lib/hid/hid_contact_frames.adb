pragma Ada_2022;
package body HID_Contact_Frames with SPARK_Mode is
   use type Interfaces.Unsigned_64;
   procedure Reset (S : in out State) is
   begin
      S.Wanted := 0; S.Used := 0; S.Time := 0; S.Items := [others => Empty_Contact];
   end Reset;
   procedure Configure (S : in out State; Limit : Contact_Limit) is
   begin
      Reset (S); S.Limit := Limit; S.Is_Configured := True;
   end Configure;
   procedure Feed
     (S : in out State; Time : Scan_Time; Declared : Declared_Count;
      Slots : Contact_Array; Complete : out Frame; Info : out Report)
   is
      Take : Contact_Count;
      C : Contact;
      procedure Refuse (Why : Status) is
      begin
         Info.Discarded_Partial := Info.Discarded_Partial or S.Used /= 0;
         Reset (S); Info.Outcome := Why;
      end Refuse;
   begin
      Complete := (Count => 0, Time => 0, Items => [others => Empty_Contact]);
      Info := (Outcome => Not_Configured, Discarded_Partial => False);
      if not S.Is_Configured then return; end if;
      if Declared > Declared_Count (S.Limit) then
         Info.Discarded_Partial := Pending (S); Refuse (Count_Refused); return;
      end if;
      if Declared /= 0 then
         Info.Discarded_Partial := Pending (S);
         Reset (S); S.Wanted := Contact_Count (Declared); S.Time := Time;
      elsif Pending (S) then
         if Time /= S.Time then
            Info.Discarded_Partial := True; Refuse (Scan_Mismatch); return;
         end if;
      else
         Complete.Time := Time; Info.Outcome := Frame_Complete; return;
      end if;
      Take := S.Wanted - S.Used;
      if Interfaces.Unsigned_64 (Slots'Length) < Interfaces.Unsigned_64 (Take) then
         Take := Contact_Count (Slots'Length);
      end if;
      if Take = 0 then Refuse (Invalid_Slots); return; end if;
      for N in Contact_Count range 1 .. Take loop
         C := Slots (Slots'First + Natural (N - 1));
         for J in Contact_Count range 1 .. S.Used loop
            if Identity (S.Items (Positive (J))) = Identity (C) then
               Refuse (Duplicate_Contact); return;
            end if;
            pragma Loop_Invariant
              (for all K in Contact_Count range 1 .. J =>
                 Identity (S.Items (Positive (K))) /= Identity (C));
         end loop;
         S.Used := S.Used + 1; S.Items (Positive (S.Used)) := C;
         pragma Loop_Invariant (Valid (S));
         pragma Loop_Invariant (S.Wanted = S.Wanted'Loop_Entry);
         pragma Loop_Invariant (S.Limit = S.Limit'Loop_Entry);
         pragma Loop_Invariant (S.Time = S.Time'Loop_Entry);
         pragma Loop_Invariant (S.Used = S.Used'Loop_Entry + N);
      end loop;
      if S.Used = S.Wanted then
         Complete := (Count => S.Wanted, Time => S.Time, Items => S.Items);
         Reset (S); Info.Outcome := Frame_Complete;
      else Info.Outcome := Frame_Pending;
      end if;
   end Feed;
end HID_Contact_Frames;
