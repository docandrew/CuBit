pragma Ada_2022;
package body CuBit.Startup_Builder with SPARK_Mode is

   procedure Build
     (Into : in out Context; Declared : Table;
      Self_Handle, Self_Process_Handle : Unsigned_64;
      Built : out Table; Result : out Build_Result)
   is
      Work : Table := Empty_Table;
      Handle : Unsigned_64;
      Outcome : Mint_Outcome;
      Self_Name : constant Name := To_Name (Self_Text);
      Self_Process_Name : constant Name := To_Name (Self_Process_Text);
   begin
      Built := Empty_Table;
      Result := (Status => Not_A_Declaration, Index => 0, Why => Granted);

      if not Declaration (Declared) then
         return;
      end if;
      if Declared.Count > Maximum_Entries - Self_Entries then
         Result.Status := Too_Many_Entries;
         return;
      end if;
      if Find (Declared, Self_Name) /= 0 or else Find (Declared, Self_Process_Name) /= 0 then
         Result.Status := Reserved_Name_Declared;
         return;
      end if;
      if Self_Handle = No_Handle or else Self_Process_Handle = No_Handle then
         Result.Status := Bad_Self_Handle;
         return;
      end if;
      pragma Assert (Valid_Name (Self_Name) and then Valid_Name (Self_Process_Name));

      Work.Limit := Declared.Limit;
      for I in 1 .. Declared.Count loop
         pragma Loop_Invariant (Work.Count = I - 1 and then Work.Limit = Declared.Limit);
         pragma Loop_Invariant
           (for all K in 1 .. I - 1 =>
              Work.Entries (K).Label = Declared.Entries (K).Label
              and then Work.Entries (K).Kind = Declared.Entries (K).Kind
              and then Work.Entries (K).Requirement = Declared.Entries (K).Requirement
              and then Valid_Entry (Work.Entries (K))
              and then Work.Entries (K).State /= Unassigned
              and then (if Declared.Entries (K).Requirement = Required
                        then Work.Entries (K).State = Granted));
         pragma Loop_Invariant (Built = Empty_Table);

         Mint (Into, Declared.Entries (I), I, Handle, Outcome);
         Report (Into, Declared.Entries (I), I, Outcome);
         Work.Entries (I) := Declared.Entries (I);
         if Outcome = Granted then
            if Handle = No_Handle then
               Result := (Status => Minter_Fault, Index => I, Why => Outcome);
               return;
            end if;
            Work.Entries (I).State := Granted;
            Work.Entries (I).Handle := Handle;
         elsif Declared.Entries (I).Requirement = Required then
            Result := (Status => Required_Refused, Index => I, Why => Outcome);
            return;
         else
            Work.Entries (I).State := Absent;
            Work.Entries (I).Handle := No_Handle;
         end if;
         Work.Count := I;
      end loop;

      Work.Entries (Declared.Count + 1) :=
        (Label => Self_Name, Kind => Endpoint, Requirement => Required, State => Granted,
         Handle => Self_Handle);
      Work.Entries (Declared.Count + 2) :=
        (Label => Self_Process_Name, Kind => Process, Requirement => Required, State => Granted,
         Handle => Self_Process_Handle);
      Work.Count := Declared.Count + Self_Entries;

      --  Names stay distinct: the declared ones are, and neither self name
      --  is among them.
      pragma Assert
        (for all K in 1 .. Declared.Count =>
           Work.Entries (K).Label /= Self_Name and then Work.Entries (K).Label /= Self_Process_Name);
      pragma Assert (Self_Name /= Self_Process_Name);
      pragma Assert (Distinct_Names (Work));

      Built := Work;
      Result := (Status => Ready, Index => 0, Why => Granted);
   end Build;

end CuBit.Startup_Builder;
