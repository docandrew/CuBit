pragma Ada_2022;
package body CuBit.Filesystem_Places with SPARK_Mode is

   function Make_Root (Name : String) return Volume_Root is
      Result : Volume_Root;
   begin
      Result.Text (1) := Volume_Mark;
      for K in 1 .. Name'Length loop
         Result.Text (K + 1) := Name (Name'First + (K - 1));
      end loop;
      Result.Text (Name'Length + 2) := Root_End;
      Result.Length := Name'Length + 2;
      return Result;
   end Make_Root;

   function On_Root (Root : Volume_Root; Prefix : String) return Boolean is
     (Root.Length > 1 and then Prefix'Length > 0
      and then ((Prefix'Length >= Root.Length
                 and then Prefix (Prefix'First .. Prefix'First + (Root.Length - 1)) =
                          Root.Text (1 .. Root.Length))
                or else (Prefix'Length = Root.Length - 1
                         and then Prefix = Root.Text (1 .. Root.Length - 1))));

   --  Entry N's first byte, and its prefix length as the wire states it.
   function Entry_Base (N : Positive) return Positive is
     ((N - 1) * FA.Wire_Entry_Bytes + 1)
     with Pre => N <= FA.Maximum_Entries;

   function Stated_Length (Scopes : FA.Wire_Bytes; Base : Positive) return Natural is
     (Natural (Scopes (Base + Prefix_Length_At))
      + Byte_Radix * Natural (Scopes (Base + Prefix_Length_At + 1)))
     with Pre => Scopes'First = 1 and then Base <= Scopes'Length
                 and then Scopes'Length - Base >= FA.Wire_Entry_Bytes - 1;

   function Prefix_Of
     (Scopes : FA.Wire_Bytes; Base : Positive; Length : Natural) return String
     with Pre  => Scopes'First = 1 and then Base <= Scopes'Length
                  and then Scopes'Length - Base >= FA.Wire_Entry_Bytes - 1
                  and then Length <= FA.Maximum_Prefix_Bytes,
          Post => Prefix_Of'Result'First = 1 and then Prefix_Of'Result'Length = Length;

   function Prefix_Of
     (Scopes : FA.Wire_Bytes; Base : Positive; Length : Natural) return String
   is
      Result : String (1 .. Length);
   begin
      for K in 1 .. Length loop
         Result (K) := Character'Val (Scopes (Base + Prefix_At + (K - 1)));
      end loop;
      return Result;
   end Prefix_Of;

   --  A volume-qualified prefix whose volume is present.
   function Present_Scope (Prefix : String; Present : Root_Table) return Boolean is
     (Prefix'Length > 0 and then Prefix (Prefix'First) = Volume_Mark
      and then (for some R of Present => On_Root (R, Prefix)));

   function Count_Places
     (Scopes : FA.Wire_Bytes; Count : Scope_Count; Present : Root_Table)
      return Natural
   is
      Each  : constant Positive := Natural'Max (1, Present'Length);
      Total : Natural := 0;
   begin
      for N in 1 .. Count loop
         pragma Loop_Invariant (Total <= (N - 1) * Each);
         declare
            Base   : constant Positive := Entry_Base (N);
            Length : constant Natural := Stated_Length (Scopes, Base);
         begin
            if Length = 0 then
               Total := Total + Present'Length;
            elsif Length <= FA.Maximum_Prefix_Bytes
              and then Present_Scope (Prefix_Of (Scopes, Base, Length), Present)
            then
               Total := Total + 1;
            end if;
         end;
      end loop;
      return Total;
   end Count_Places;

   procedure Select_Places
     (Scopes  : FA.Wire_Bytes;
      Count   : Scope_Count;
      Present : Root_Table;
      Places  : out Place_Bytes;
      Written : out Natural)
   is
      --  One entry after the last written, if it fits.
      procedure Put (Rights : Unsigned_8; Text : String)
        with Pre  => Places'First = 1 and then Written <= Places'Length / FA.Wire_Entry_Bytes
                     and then Text'Length <= FA.Maximum_Prefix_Bytes,
             Post => Written <= Places'Length / FA.Wire_Entry_Bytes;

      procedure Put (Rights : Unsigned_8; Text : String) is
         Base : Positive;
      begin
         if Written >= Places'Length / FA.Wire_Entry_Bytes then
            return;
         end if;
         Base := Written * FA.Wire_Entry_Bytes + 1;
         Places (Base + Rights_At) := Rights;
         Places (Base + Prefix_Length_At) := Unsigned_8 (Text'Length mod Byte_Radix);
         Places (Base + Prefix_Length_At + 1) := Unsigned_8 (Text'Length / Byte_Radix);
         for K in 1 .. Text'Length loop
            Places (Base + Prefix_At + (K - 1)) := Character'Pos (Text (Text'First + (K - 1)));
         end loop;
         Written := Written + 1;
      end Put;
   begin
      Places := [others => 0];
      Written := 0;
      for N in 1 .. Count loop
         pragma Loop_Invariant (Written <= Places'Length / FA.Wire_Entry_Bytes);
         declare
            Base   : constant Positive := Entry_Base (N);
            Length : constant Natural := Stated_Length (Scopes, Base);
         begin
            if Length = 0 then
               for R of Present loop
                  pragma Loop_Invariant (Written <= Places'Length / FA.Wire_Entry_Bytes);
                  Put (Scopes (Base + Rights_At), R.Text (1 .. R.Length));
               end loop;
            elsif Length <= FA.Maximum_Prefix_Bytes then
               declare
                  Prefix : constant String := Prefix_Of (Scopes, Base, Length);
               begin
                  if Present_Scope (Prefix, Present) then
                     Put (Scopes (Base + Rights_At), Prefix);
                  end if;
               end;
            end if;
         end;
      end loop;
   end Select_Places;
end CuBit.Filesystem_Places;
