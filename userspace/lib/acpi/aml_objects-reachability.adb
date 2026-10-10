package body AML_Objects.Reachability with SPARK_Mode is
   package IDs renames AML_Object_Identifiers;
   function Target (Store : State; Address : IDs.Object_Address) return Object_ID is
     (if Matches_Address (Store, Address) then IDs.Slot_Of (Address) else No_Object);
   function Edge (Store : State; From, Into : Object_ID; Targets : Reference_Targets)
     return Boolean is
     (Is_Live (Store, From) and then Is_Live (Store, Into) and then
       (case Kind (Store, From) is
          when Package_Object => Length (Store, From) > 0 and then
            (for some Index in 0 .. Length (Store, From) - 1 => Element (Store, From, Index) = Into),
          when Reference_Object => Target (Store, Targets (From)) = Into,
          when others => False)) with Ghost, Pre => Valid (Store);
   function Exact_Closure
     (Store : State; Seeds : Keep_Set; Targets : Reference_Targets;
      Keep : Keep_Set; Scratch : Workspace) return Boolean is
     (Scratch.Used <= Live_Count (Store)
      and then (for all ID in Keep'Range =>
        (if Seeds (ID) then Keep (ID))
        and then (if Keep (ID) then Is_Live (Store, ID)
          and then Scratch.Rank (ID) in 1 .. Scratch.Used
          and then Scratch.Queue (Scratch.Rank (ID)) = ID
          else Scratch.Rank (ID) = 0))
      and then (for all Position in 1 .. Scratch.Used =>
        Scratch.Queue (Position) /= No_Object
        and then Keep (Scratch.Queue (Position))
        and then Scratch.Rank (Scratch.Queue (Position)) = Position
        and then (if Seeds (Scratch.Queue (Position)) then
           Scratch.Parent (Position) = No_Object
         else Scratch.Parent (Position) /= No_Object
           and then Scratch.Rank (Scratch.Parent (Position)) in 1 .. Position - 1
           and then Edge (Store, Scratch.Parent (Position), Scratch.Queue (Position), Targets)))
      and then (for all ID in Keep'Range => (if Keep (ID) then
        (case Kind (Store, ID) is
           when Package_Object => (if Length (Store, ID) > 0 then
             (for all Index in 0 .. Length (Store, ID) - 1 =>
               Element (Store, ID, Index) = No_Object or else Keep (Element (Store, ID, Index)))),
           when Reference_Object => Target (Store, Targets (ID)) = No_Object
             or else Keep (Target (Store, Targets (ID))),
           when others => True))));
   procedure Trace
     (Store : State; Seeds : Keep_Set; Targets : Reference_Targets;
      Scratch : in out Workspace; Keep : out Keep_Set; Status : out Trace_Status)
   is
      Scanned : Object_ID := 0;
      Current, Next : Object_ID;
      procedure Add (ID, Parent : Object_ID) with Pre => Is_Live (Store, ID) is
      begin
         if not Keep (ID) then
            Scratch.Used := Scratch.Used + 1;
            Scratch.Queue (Scratch.Used) := ID;
            Scratch.Parent (Scratch.Used) := Parent;
            Scratch.Rank (ID) := Scratch.Used;
            Keep (ID) := True;
         end if;
      end Add;
   begin
      Scratch.Used := 0;
      Scratch.Queue := [others => No_Object];
      Scratch.Rank := [others => No_Object];
      Scratch.Parent := [others => No_Object];
      Keep := [others => False];
      Status := Invalid_Seed;
      for ID in Seeds'Range loop
         if Seeds (ID) and then not Is_Live (Store, ID) then return; end if;
      end loop;
      for ID in Seeds'Range loop
         if Seeds (ID) then Add (ID, No_Object); end if;
      end loop;
      while Scanned < Scratch.Used loop
         Scanned := Scanned + 1;
         Current := Scratch.Queue (Scanned);
         case Kind (Store, Current) is
            when Package_Object =>
               for Index in 1 .. Length (Store, Current) loop
                  Next := Element (Store, Current, Index - 1);
                  if Next /= No_Object then Add (Next, Current); end if;
               end loop;
            when Reference_Object =>
               Next := Target (Store, Targets (Current));
               if Next /= No_Object then Add (Next, Current); end if;
            when others => null;
         end case;
      end loop;
      Status := Traced;
   end Trace;
end AML_Objects.Reachability;
