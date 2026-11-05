pragma Ada_2022;
package body CuBit.Startup_State
  with SPARK_Mode, Refined_State => (Loaded_Table => (Value, Loaded_Flag))
is
   --  Always a valid, complete table: Empty_Table until Load succeeds.
   subtype Loadable_Table is Table
     with Dynamic_Predicate => Valid_Table (Loadable_Table) and then Complete (Loadable_Table);

   Value       : Loadable_Table := Empty_Table;
   Loaded_Flag : Boolean := False;

   function Is_Loaded return Boolean is (Loaded_Flag)
   with Refined_Global => Loaded_Flag;

   function Current return Table is (Value)
   with Refined_Global => Value;

   procedure Load (Raw : Bytes; Outcome : out Load_Result)
   with Refined_Global => (In_Out => (Value, Loaded_Flag))
   is
      Candidate : Table;
      Decoding : Decode_Result;
   begin
      if Loaded_Flag then
         Outcome := Already_Loaded;
         return;
      end if;
      Decode (Raw, Candidate, Decoding);
      if Decoding /= Decoded then
         Outcome := Malformed;
      elsif not Complete (Candidate) then
         Outcome := Incomplete;
      else
         Value := Candidate;
         Loaded_Flag := True;
         Outcome := Loaded;
      end if;
   end Load;

   function Granted (Label : String; Kind : Capability_Kind) return Unsigned_64
   with Refined_Global => Value
   is
      Index : Entry_Count;
   begin
      if not Valid_Name_Text (Label) then
         return No_Handle;
      end if;
      Index := Find (Value, To_Name (Label));
      if Index /= 0
        and then Value.Entries (Index).Kind = Kind
        and then Value.Entries (Index).State = Granted
      then
         return Value.Entries (Index).Handle;
      end if;
      return No_Handle;
   end Granted;

   function Verify (Expected : Expectations) return Verification
   with Refined_Global => (Value, Loaded_Flag)
   is
      Index : Entry_Count;
   begin
      if not Loaded_Flag then
         return (Status => Not_Loaded, Index => 0);
      end if;
      for I in Expected'Range loop
         Index := Find (Value, Expected (I).Label);
         if Index = 0 then
            return (Status => Missing, Index => I);
         elsif Value.Entries (Index).Kind /= Expected (I).Kind then
            return (Status => Wrong_Kind, Index => I);
         elsif Value.Entries (Index).Requirement /= Expected (I).Requirement then
            return (Status => Wrong_Need, Index => I);
         elsif Expected (I).Requirement = Required and then Value.Entries (Index).State /= Granted then
            return (Status => Not_Granted, Index => I);
         end if;
         pragma Loop_Invariant
           (for all K in Expected'First .. I =>
              Find (Value, Expected (K).Label) /= 0
              and then Value.Entries (Find (Value, Expected (K).Label)).Kind = Expected (K).Kind
              and then Value.Entries (Find (Value, Expected (K).Label)).Requirement
                       = Expected (K).Requirement
              and then (Expected (K).Requirement = Optional
                        or else Value.Entries (Find (Value, Expected (K).Label)).State = Granted));
      end loop;
      return (Status => Ready, Index => 0);
   end Verify;

end CuBit.Startup_State;
