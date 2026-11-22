pragma Ada_2022;
package body HID_Touchpad_Reports with SPARK_Mode is
   use type Schema.Status;
   use type Schema.Report_Kind;
   use type Schema.Count;
   use type Schema.Word;
   use type Schema.Byte;
   use type Schema.Wide;
   use type Schema.Report_ID;
   use type Bindings.Work_Count;
   use type Contacts.Compile_Status;
   use type Contacts.Decode_Status;
   use type Metadata.Compile_Status;
   use type Metadata.Decode_Status;
   use type Frames.Contact_Count;
   use type Interfaces.Unsigned_16;
   use type Contacts.Coordinate_Bounds;
   Application_Collection : constant Schema.Byte := 1;
   Logical_Collection : constant Schema.Byte := 2;
   Finger_Usage : constant Schema.Word := 16#000D_0022#;
   Constant_Flag : constant Schema.Word := 1;
   procedure Reset (S : in out State) is
   begin Frames.Reset (S.Frames_State); end Reset;
   procedure Invalidate (S : in out State) is
   begin Reset (S); S.Is_Ready := False; S.Used := 0; S.Limit := 0;
      S.Geometry := (Outcome => Coordinates_Unavailable); end Invalidate;
   procedure Configure
     (S : in out State; Layout : Schema.Result; Fields : Schema.Field_Table;
      Usages : Schema.Usage_Table; Collections : Schema.Collection_Table;
      Touchpad : Schema.Count; Limit : Frames.Contact_Limit;
      Budget : Bindings.Work_Count; Charged : out Bindings.Work_Count;
      Info : out Compile_Report)
   is
      Meta : Metadata.Report_Layout;
      Candidates : Slot_Table;
      IDs : Collection_Table := [others => 0];
      Count : Slot_Count := 0;
      Work : Bindings.Work_Count;
      F : Schema.Field_Span;
      C : Schema.Collection;
      Current, Finger, Application : Schema.Count;
      Seen, Allowed : Boolean;
      Geometry : Coordinate_Report;
      procedure Charge is
      begin
         Allowed := Charged < Budget;
         if Allowed then Charged := Charged + 1; else Info.Outcome := Budget_Exhausted; end if;
      end Charge;
   begin
      Invalidate (S); Charged := 0; Info := (others => <>);
      if Layout.Outcome /= Schema.Decoded then return; end if;
      Metadata.Compile (Layout, Fields, Usages, Collections, Touchpad, Budget,
                        Meta, Work, Info.Metadata_Reason); Charged := Work;
      if Info.Metadata_Reason /= Metadata.Bound then
         Info.Outcome := (if Info.Metadata_Reason = Metadata.Budget_Exhausted then Budget_Exhausted else Metadata_Refused); return;
      end if;
      Info.Outcome := Invalid_Tables;
      if Schema.Wide (Layout.Fields_Required) > Schema.Wide (Fields'Length) or else
         Schema.Wide (Layout.Collections_Required) > Schema.Wide (Collections'Length) then return; end if;
      for N in Schema.Count range 1 .. Layout.Fields_Required loop
         Charge; if not Allowed then return; end if;
         F := Fields (Fields'First + Natural (N - 1));
         if F.Kind = Schema.Input and then (F.Flags and Constant_Flag) = 0 then
            Current := F.Collection_ID; Finger := 0; Application := 0;
            while Current /= 0 loop
               if Current > Layout.Collections_Required then return; end if;
               Charge; if not Allowed then return; end if;
               C := Collections (Collections'First + Natural (Current - 1));
               if C.Parent >= Current then return; end if;
               if Finger = 0 and then C.Kind = Logical_Collection and then C.Usage = Finger_Usage then Finger := Current; end if;
               if Application = 0 and then C.Kind = Application_Collection then Application := Current; end if;
               Current := C.Parent;
            end loop;
            if Application = Touchpad and then Finger /= 0 then
               Seen := False;
               for J in Slot_Count range 1 .. Count loop
                  Charge; if not Allowed then return; end if;
                  if IDs (Positive (J)) = Finger then Seen := True; end if;
               end loop;
               if not Seen then
                  if Count = Slot_Count'Last then Info.Outcome := Too_Many_Slots; return; end if;
                  Count := Count + 1; IDs (Positive (Count)) := Finger; Info.Collection_ID := Finger;
                  Contacts.Compile (Layout, Fields, Usages, Collections, Finger, Budget - Charged,
                                    Candidates (Positive (Count)), Work, Info.Contact_Reason);
                  Charged := Charged + Work;
                  if Info.Contact_Reason /= Contacts.Bound then
                     Info.Outcome := (if Info.Contact_Reason = Contacts.Budget_Exhausted then Budget_Exhausted else Contact_Refused); return;
                  end if;
                  if Contacts.Report_Identity (Candidates (Positive (Count))) /= Metadata.Report_Identity (Meta)
                  then Info.Outcome := Split_Report; return; end if;
                  Charge; if not Allowed then return; end if;
                  declare Bounds : constant Contacts.Coordinate_Bounds := Contacts.Coordinates (Candidates (Positive (Count))); begin
                     if Count = 1 then Geometry := (Uniform_Coordinates, Bounds);
                     elsif Geometry.Outcome = Uniform_Coordinates and then Geometry.Bounds /= Bounds then
                        Geometry := (Outcome => Inconsistent_Coordinates);
                     end if;
                  end;
               end if;
            end if;
         end if;
      end loop;
      if Count = 0 then Info.Outcome := No_Slots; return; end if;
      S.Meta := Meta; S.Layouts := Candidates; S.Used := Count; S.Has_Report_IDs := Layout.Has_Report_IDs;
      S.Limit := Limit; Frames.Configure (S.Frames_State, Limit); S.Is_Ready := True; Info.Outcome := Configured;
      S.Geometry := Geometry;
   end Configure;
   procedure Feed
     (S : in out State; Data : Schema.Bytes;
      Complete : out Complete_Report; Info : out Report)
   is
      Empty : Complete_Report;
      M : Metadata.Metadata;
      Samples : Frames.Contact_Array (1 .. Maximum_Slots) := [others => Empty_Contact];
      Needed : Frames.Contact_Count;
      Take : Slot_Count;
      Had_Partial : constant Boolean := Pending (S);
      procedure Assemble (Count : Slot_Count) is
      begin
         Frames.Feed (S.Frames_State, M.Scan_Time, M.Contact_Count, Samples (1 .. Natural (Count)), Complete.Contacts, Info.Assembly);
         Info.Discarded_Partial := Info.Assembly.Discarded_Partial;
         case Info.Assembly.Outcome is
            when Frames.Frame_Complete => Complete.Buttons := M.Buttons; Info.Outcome := Frame_Ready;
            when Frames.Frame_Pending => Info.Outcome := Frame_Pending;
            when others => Info.Outcome := Assembly_Refused;
         end case;
      end Assemble;
   begin
      Complete := Empty; Info := (others => <>);
      if not S.Is_Ready then return; end if;
      if S.Has_Report_IDs and then Schema.Wide (Data'Length) /= 0 and then
         Data (Data'First) /= Schema.Byte (Metadata.Report_Identity (S.Meta))
      then Info.Outcome := Ignored_Report; return; end if;
      Metadata.Decode (S.Meta, Data, M, Info.Metadata_Reason);
      if Info.Metadata_Reason /= Metadata.Sampled then
         Reset (S); Info.Outcome := Metadata_Invalid; Info.Discarded_Partial := Had_Partial; return;
      end if;
      if M.Contact_Count > Interfaces.Unsigned_32 (S.Limit) then Assemble (0); return; end if;
      if M.Contact_Count /= 0 then Needed := Frames.Contact_Count (M.Contact_Count);
      elsif Pending (S) then
         if M.Scan_Time /= Frames.Pending_Time (S.Frames_State) then Assemble (0); return; end if;
         Needed := Frames.Expected (S.Frames_State) - Frames.Collected (S.Frames_State);
      else Needed := 0;
      end if;
      Take := (if Schema.Wide (Needed) < Schema.Wide (S.Used) then Slot_Count (Needed) else S.Used);
      for N in Slot_Count range 1 .. Take loop
         Contacts.Decode (S.Layouts (Positive (N)), Data, Samples (Positive (N)), Info.Contact_Reason);
         if Info.Contact_Reason /= Contacts.Sampled then
            Reset (S); Info.Outcome := Contact_Invalid; Info.Bad_Slot := N; Info.Discarded_Partial := Had_Partial; return;
         end if;
      end loop;
      Assemble (Take);
   end Feed;
end HID_Touchpad_Reports;
