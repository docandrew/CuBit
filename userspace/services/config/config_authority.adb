with CuBit.Config_Inspection;

package body Config_Authority with SPARK_Mode is
   use type Interfaces.Unsigned_64;

   procedure Append
     (Rules : in out Rule_Set; Scope : String; Allowed : Rights;
      Accepted : out Boolean)
   is
      Entry_Value : Rule;
   begin
      Accepted := False;
      if Rules.Count = Maximum_Rules or Scope'Length > Maximum_Scope then return; end if;
      Entry_Value.Scope (1 .. Scope'Length) := Scope;
      Entry_Value.Length := Scope'Length;
      Entry_Value.Allowed := Allowed;
      Rules.Count := Rules.Count + 1;
      Rules.Entries (Rules.Count) := Entry_Value;
      Accepted := True;
   end Append;

   function Has_Profile (State : Authority_State; Subject : Subject_ID) return Boolean is
     (Subject /= No_Subject and then
      (for some Item of State.Profiles => Item.Subject = Subject));

   function Revision (State : Authority_State; Subject : Subject_ID)
      return Interfaces.Unsigned_64 is
   begin
      if Subject = No_Subject then return 0; end if;
      for Item of State.Profiles loop
         if Item.Subject = Subject then return Item.Revision; end if;
      end loop;
      return 0;
   end Revision;

   function Allows
     (State : Authority_State; Subject : Subject_ID; Key : String;
      Requested : Operation) return Boolean is
     (Subject /= No_Subject and then
      (for some Item of State.Profiles =>
         Item.Subject = Subject and then
         (for some I in 1 .. Item.Rules.Count =>
            Item.Rules.Entries (I).Allowed (Requested) and then
            CuBit.Config_Inspection.Contains
              (Item.Rules.Entries (I).Scope (1 .. Item.Rules.Entries (I).Length), Key))));

   function Other_Profiles_Unchanged
     (Before, After : Authority_State; Subject : Subject_ID) return Boolean is
     (for all I in Before.Profiles'Range =>
        (if (Before.Profiles (I).Subject /= Subject and
             Before.Profiles (I).Subject /= No_Subject) or
            (After.Profiles (I).Subject /= Subject and
             After.Profiles (I).Subject /= No_Subject)
         then Before.Profiles (I) = After.Profiles (I)));

   procedure Install
     (State : in out Authority_State; Subject : Subject_ID; Rules : Rule_Set;
      Result : out Install_Result)
   is
      Slot : Natural range 0 .. Maximum_Subjects := 0;
   begin
      if Subject = No_Subject or Subject = Subject_ID'Last then
         Result := Invalid_Subject; return;
      end if;
      if State.Last_Revision = Interfaces.Unsigned_64'Last then
         Result := Identity_Exhausted; return;
      end if;
      for I in State.Profiles'Range loop
         if State.Profiles (I).Subject = Subject then Slot := I; exit; end if;
         if Slot = 0 and State.Profiles (I).Subject = No_Subject then Slot := I; end if;
         pragma Loop_Invariant
           (Slot = 0 or else State.Profiles (Slot).Subject = No_Subject);
      end loop;
      if Slot = 0 then Result := Capacity_Exceeded; return; end if;
      State.Last_Revision := State.Last_Revision + 1;
      State.Profiles (Slot) := (Subject, State.Last_Revision, Rules);
      Result := Installed;
   end Install;

   procedure Revoke (State : in out Authority_State; Subject : Subject_ID) is
   begin
      for I in State.Profiles'Range loop
         if State.Profiles (I).Subject = Subject then
            State.Profiles (I) := (others => <>);
         end if;
         pragma Loop_Invariant
           (Subject = No_Subject or else
              (for all J in State.Profiles'First .. I =>
                 State.Profiles (J).Subject /= Subject));
         pragma Loop_Invariant
           (Other_Profiles_Unchanged (State'Loop_Entry, State, Subject));
      end loop;
   end Revoke;
end Config_Authority;
