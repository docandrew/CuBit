pragma Ada_2022;
package body AML_Objects.Copies with SPARK_Mode is
   function Copy_Frame (Store, Prior : State) return Boolean is
      Bound : Object_ID := Prior.Used;
   begin
      if not Extends (Store, Prior)
        or else Store.Bytes (Store.Bytes_Used + 1 .. Max_Bytes) /=
          Prior.Bytes (Store.Bytes_Used + 1 .. Max_Bytes)
        or else Store.Elements (Store.Elements_Used + 1 .. Max_Elements) /=
          Prior.Elements (Store.Elements_Used + 1 .. Max_Elements)
      then return False; end if;
      for I in 1 .. Max_Objects loop
         if Is_Live (Store, I) and then not Is_Live (Prior, I) then
            if Prior.Objects (I).Stamp = Prior.Generation_Limit
              or else Store.Objects (I).Stamp /= Prior.Objects (I).Stamp + 1
            then return False; end if;
            Bound := Object_ID'Max (Bound, I);
         elsif Store.Objects (I) /= Prior.Objects (I) then
            return False;
         end if;
      end loop;
      return Store.Used = Bound;
   end Copy_Frame;

   function Valid_Witness
     (Store, Prior : State; Source, Target : Object_ID; Witness : Copy_Witness)
      return Boolean
   is
      type Reverse_Map is array (Object_ID range 1 .. Max_Objects) of Object_ID;
      Position : Reverse_Map := [others => 0];
      Bytes : Natural range 0 .. Max_Bytes := Prior.Bytes_Used;
      Elements : Natural range 0 .. Max_Elements := Prior.Elements_Used;
      Link : Copy_Link;
      Parent, Old_Child, New_Child, Child_Position : Object_ID;
   begin
      if not Copy_Frame (Store, Prior) or else Witness.Used = 0
        or else Store.Live_Used <= Prior.Live_Used
        or else Witness.Used /= Store.Live_Used - Prior.Live_Used
        or else Target /= Witness.Links (1).Copy
        or else Witness.Links (1).Original /= Source
        or else Witness.Links (1).Parent /= 0 or else Witness.Links (1).Slot /= 0
      then return False; end if;
      for J in 1 .. Witness.Used loop
         Link := Witness.Links (J);
         if not Header_Matches (Store, Prior, Link.Original, Link.Copy)
           or else Position (Link.Copy) /= 0 then return False; end if;
         Position (Link.Copy) := J;
         case Kind (Store, Link.Copy) is
            when String_Object | Buffer_Object =>
               if Store.Objects (Link.Copy).First /= Bytes
                 or else Length (Store, Link.Copy) > Max_Bytes - Bytes then return False; end if;
               Bytes := Bytes + Length (Store, Link.Copy);
            when Package_Object =>
               if Store.Objects (Link.Copy).First /= Elements
                 or else Length (Store, Link.Copy) > Max_Elements - Elements then return False; end if;
               Elements := Elements + Length (Store, Link.Copy);
            when Integer_Object | Reference_Object => null;
         end case;
         if Link.Bytes_After /= Bytes or else Link.Elements_After /= Elements then return False; end if;
      end loop;
      if Bytes /= Store.Bytes_Used or else Elements /= Store.Elements_Used then return False; end if;
      for J in 1 .. Witness.Used loop
         Link := Witness.Links (J);
         if J > 1 then
            if Link.Parent not in 1 .. J - 1 then return False; end if;
            Parent := Witness.Links (Link.Parent).Copy;
            if Kind (Store, Parent) /= Package_Object
              or else Link.Slot >= Length (Store, Parent)
              or else Element (Store, Parent, Link.Slot) /= Link.Copy
              or else Element (Prior, Witness.Links (Link.Parent).Original, Link.Slot) /= Link.Original
            then return False; end if;
            if J > 2 and then
              (Link.Parent < Witness.Links (J - 1).Parent or else
                (Link.Parent = Witness.Links (J - 1).Parent and then
                  Link.Slot <= Witness.Links (J - 1).Slot)) then return False; end if;
         end if;
         if Kind (Store, Link.Copy) = Package_Object then
            for I in 1 .. Length (Store, Link.Copy) loop
               Old_Child := Element (Prior, Link.Original, I - 1);
               New_Child := Element (Store, Link.Copy, I - 1);
               if Old_Child = No_Object then
                  if New_Child /= No_Object then return False; end if;
               else
                  if New_Child = No_Object then return False; end if;
                  Child_Position := Position (New_Child);
                  if Child_Position <= J
                    or else Witness.Links (Child_Position).Original /= Old_Child
                    or else Witness.Links (Child_Position).Parent /= J
                    or else Witness.Links (Child_Position).Slot /= I - 1
                  then return False; end if;
               end if;
            end loop;
         end if;
      end loop;
      for I in 1 .. Max_Objects loop
         if Is_Live (Store, I) and then not Is_Live (Prior, I)
           and then Position (I) = 0 then return False; end if;
      end loop;
      return True;
   end Valid_Witness;

   function Is_Independent_Copy
     (Store, Prior : State; Source, Target : Object_ID; Witness : Copy_Witness) return Boolean
   is
      type Object_List is array (Positive range 1 .. Max_Objects) of Object_ID;
      type Seen_Set is array (Object_ID range 1 .. Max_Objects) of Boolean;
      Originals, Copies : Object_List := [others => No_Object];
      Seen : Seen_Set := [others => False];
      Added : Object_ID;
      Bytes_Next : Natural range 0 .. Max_Bytes := Byte_Count (Prior);
      Elements_Next : Natural range 0 .. Max_Elements := Element_Count (Prior);
      Queued : Object_ID := 1;
      Next : Positive := 1;
      Original, Copy, Old_Child, New_Child : Object_ID;
   begin
      if not Copy_Frame (Store, Prior) or else Store.Live_Used <= Prior.Live_Used
        or else not Is_Live (Store, Target) or else Is_Live (Prior, Target)
      then return False; end if;
      Added := Store.Live_Used - Prior.Live_Used;
      if Witness.Used /= Added then return False; end if;
      Originals (1) := Source; Copies (1) := Target; Seen (Target) := True;
      while Next <= Queued loop
         pragma Loop_Invariant (Queued >= 1 and then Queued <= Added);
         pragma Loop_Invariant (Next <= Queued + 1);
         pragma Loop_Invariant
           (for all J in 1 .. Queued => Is_Live (Prior, Originals (J))
             and then Is_Live (Store, Copies (J)) and then not Is_Live (Prior, Copies (J)));
         pragma Loop_Variant (Decreases => Max_Objects + 1 - Next);
         Original := Originals (Next); Copy := Copies (Next);
         if not Header_Matches (Store, Prior, Original, Copy)
           or else Witness.Links (Next).Original /= Original
           or else Witness.Links (Next).Copy /= Copy then return False; end if;
         case Kind (Prior, Original) is
            when Integer_Object | Reference_Object => null;
            when String_Object | Buffer_Object =>
               if Store.Objects (Copy).First /= Bytes_Next
                 or else Length (Store, Copy) > Max_Bytes - Bytes_Next then return False; end if;
               Bytes_Next := Bytes_Next + Length (Store, Copy);
            when Package_Object =>
               if Store.Objects (Copy).First /= Elements_Next
                 or else Length (Store, Copy) > Max_Elements - Elements_Next then return False; end if;
               Elements_Next := Elements_Next + Length (Store, Copy);
               for I in 1 .. Length (Prior, Original) loop
                  pragma Loop_Invariant (Queued >= 1 and then Queued <= Added);
                  pragma Loop_Invariant (Next <= Queued);
                  pragma Loop_Invariant
                    (for all J in 1 .. Queued => Is_Live (Prior, Originals (J))
                      and then Is_Live (Store, Copies (J)) and then not Is_Live (Prior, Copies (J)));
                  Old_Child := Element (Prior, Original, I - 1);
                  New_Child := Element (Store, Copy, I - 1);
                  if Old_Child = No_Object then
                     if New_Child /= No_Object then return False; end if;
                  else
                     if Queued = Added or else not Is_Live (Store, New_Child)
                       or else Is_Live (Prior, New_Child) or else Seen (New_Child) then return False; end if;
                     Queued := Queued + 1;
                     Originals (Queued) := Old_Child; Copies (Queued) := New_Child;
                     Seen (New_Child) := True;
                  end if;
               end loop;
         end case;
         Next := Next + 1;
      end loop;
      return Queued = Added and then Bytes_Next = Byte_Count (Store)
        and then Elements_Next = Element_Count (Store);
   end Is_Independent_Copy;

   procedure Clone
     (Store : in out State; Source : Object_ID;
      Target : out Object_ID; Status : out Allocation_Status; Witness : out Copy_Witness)
   is
      Candidate : State := Store;
      subtype Work_Item is Copy_Link;
      Work : Link_Array renames Witness.Links;
      Used : Object_ID renames Witness.Used;
      Next : Positive := 1;
      Root, Child : Object_ID;
      Current : Work_Item;
      Original_Child : Object_ID;
      function Matching (Original, Copy : Object_ID) return Boolean is
        (Is_Live (Store, Original)
         and then not Is_Live (Store, Copy) and then Is_Live (Candidate, Copy)
         and then Kind (Candidate, Copy) = Kind (Store, Original)
         and then Length (Candidate, Copy) = Length (Store, Original)
         and then (if Kind (Store, Original) = Package_Object then
           Candidate.Objects (Copy).First >= Store.Elements_Used)
         and then (if Kind (Store, Original) in Byte_Kind then
           Candidate.Objects (Copy).First >= Store.Bytes_Used)
         and then (if Kind (Store, Original) = Integer_Object then
           Integer_Data (Candidate, Copy) = Integer_Data (Store, Original)
           and then Origin_Of (Candidate, Copy) = Origin_Of (Store, Original))
         and then (if Kind (Store, Original) = Reference_Object then
           Reference_Data (Candidate, Copy) = Reference_Data (Store, Original))
         and then (if Kind (Store, Original) in Byte_Kind then
           Byte_Data (Candidate, Copy) = Byte_Data (Store, Original)))
        with Ghost, Pre => Valid (Store) and then Valid (Candidate);
      procedure Allocate (Original : Object_ID; Copy : out Object_ID)
      with Pre => Valid (Store) and then Valid (Candidate)
          and then Is_Live (Store, Original)
          and then Extends (Candidate, Store)
          and then (for all J in 1 .. Used => Matching (Work (J).Original, Work (J).Copy)),
        Post => Valid (Candidate) and then Extends (Candidate, Candidate'Old)
          and then Extends (Candidate, Store)
          and then (for all J in 1 .. Used => Matching (Work (J).Original, Work (J).Copy))
          and then (for all J in 1 .. Slot_Bound (Candidate'Old) => (if Is_Live (Candidate'Old, J) then
            Candidate.Objects (J) = Candidate'Old.Objects (J)
            and then (if Kind (Candidate'Old, J) in Byte_Kind then
              Byte_Data (Candidate, J) = Byte_Data (Candidate'Old, J))))
          and then (if Status = Allocated then
            Fresh_Allocation (Candidate, Candidate'Old, Copy)
            and then Live_Count (Candidate) = Live_Count (Candidate'Old) + 1
            and then Is_Live (Candidate, Copy) and then Matching (Original, Copy)
           else Copy = 0 and then Candidate = Candidate'Old)
      is
      begin
         case Kind (Store, Original) is
            when Integer_Object =>
               New_Integer (Candidate, Integer_Data (Store, Original), Copy, Status, Origin_Of (Store, Original));
            when Reference_Object =>
               New_Reference (Candidate, Reference_Data (Store, Original), Copy, Status);
            when String_Object | Buffer_Object =>
               New_Bytes (Candidate, Kind (Store, Original), Byte_Data (Store, Original), Copy, Status);
            when Package_Object =>
               New_Package (Candidate, Length (Store, Original), Copy, Status);
         end case;
      end Allocate;
      procedure Link_Child (Parent : Object_ID; Index : Natural; Value : Object_ID)
      with Pre => Valid (Store) and then Valid (Candidate)
          and then Extends (Candidate, Store)
          and then not Is_Live (Store, Parent) and then Is_Live (Candidate, Parent)
          and then Kind (Candidate, Parent) = Package_Object
          and then Index < Length (Candidate, Parent)
          and then (Value = No_Object or else Is_Live (Candidate, Value))
          and then Candidate.Objects (Parent).First >= Store.Elements_Used
          and then (for all J in 1 .. Used => Matching (Work (J).Original, Work (J).Copy)),
        Post => Valid (Candidate) and then Extends (Candidate, Store)
          and then Live_Count (Candidate) = Live_Count (Candidate'Old)
          and then (for all J in 1 .. Used => Matching (Work (J).Original, Work (J).Copy))
      is
      begin
         Set_Element (Candidate, Parent, Index, Value);
      end Link_Child;
      procedure Enqueue (Original, Copy, Parent : Object_ID; Slot : Natural)
      with Pre => Valid (Store) and then Valid (Candidate)
          and then Used >= 1 and then Used < Max_Objects
          and then Parent in 1 .. Used and then Slot < Max_Elements
          and then Matching (Original, Copy)
          and then (for all J in 1 .. Used => Matching (Work (J).Original, Work (J).Copy)),
        Post => Used = Used'Old + 1
          and then Work (Used) = Copy_Link'(Original => Original, Copy => Copy, Parent => Parent, Slot => Slot, Bytes_After => Candidate.Bytes_Used, Elements_After => Candidate.Elements_Used)
          and then (for all J in 1 .. Used'Old => Work (J) = Work'Old (J))
          and then (for all J in 1 .. Used => Matching (Work (J).Original, Work (J).Copy))
      is
      begin
         Used := Used + 1;
         Work (Used) := Copy_Link'(Original => Original, Copy => Copy, Parent => Parent, Slot => Slot, Bytes_After => Candidate.Bytes_Used, Elements_After => Candidate.Elements_Used);
      end Enqueue;
   begin
      Witness := (others => <>);
      Target := No_Object;
      Allocate (Source, Root);
      if Status /= Allocated then return; end if;
      Used := 1;
      Work (1) := (Original => Source, Copy => Root, Parent => 0, Slot => 0,
                   Bytes_After => Candidate.Bytes_Used, Elements_After => Candidate.Elements_Used);
      while Next <= Used loop
         pragma Loop_Variant (Decreases => Max_Objects + 1 - Next);
         pragma Loop_Invariant (Valid (Candidate));
         pragma Loop_Invariant (Extends (Candidate, Store));
         pragma Loop_Invariant (Used >= 1 and then Live_Count (Candidate) = Live_Count (Store) + Used);
         pragma Loop_Invariant (Next <= Used + 1);
         pragma Loop_Invariant (Is_Live (Candidate, Root) and then not Is_Live (Store, Root));
         pragma Loop_Invariant (Work (1).Original = Source and then Work (1).Copy = Root);
         pragma Loop_Invariant
           (for all J in 1 .. Used => Matching (Work (J).Original, Work (J).Copy));
         Current := Work (Next);
         if Kind (Store, Current.Original) = Package_Object then
            for I in 1 .. Length (Store, Current.Original) loop
               pragma Loop_Invariant (Valid (Candidate));
               pragma Loop_Invariant (Extends (Candidate, Store));
               pragma Loop_Invariant (Used >= 1 and then Live_Count (Candidate) = Live_Count (Store) + Used);
               pragma Loop_Invariant (Next <= Used);
               pragma Loop_Invariant (Current = Work (Next));
               pragma Loop_Invariant (Is_Live (Candidate, Root) and then not Is_Live (Store, Root));
               pragma Loop_Invariant (Work (1).Original = Source and then Work (1).Copy = Root);
               pragma Loop_Invariant (Matching (Current.Original, Current.Copy));
               pragma Loop_Invariant
                 (for all J in 1 .. Used => Matching (Work (J).Original, Work (J).Copy));
               Original_Child := Element (Store, Current.Original, I - 1);
               if Original_Child /= No_Object then
                  Allocate (Original_Child, Child);
                  if Status /= Allocated then return; end if;
                  if Used = Max_Objects then
                     Status := Object_Limit;
                     return;
                  end if;
                  Enqueue (Original_Child, Child, Next, I - 1);
                  Link_Child (Current.Copy, I - 1, Child);
               end if;
               pragma Assert
                 (for all J in 1 .. Used => Matching (Work (J).Original, Work (J).Copy));
            end loop;
         end if;
         pragma Assert
           (for all J in 1 .. Used => Matching (Work (J).Original, Work (J).Copy));
         Next := Next + 1;
      end loop;
      Store := Candidate;
      Target := Root;
      Status := Allocated;
   end Clone;
end AML_Objects.Copies;
