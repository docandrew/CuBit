pragma Ada_2022;

package body CuBit.Program_Descriptions with SPARK_Mode is

   package LA renames CuBit.Launch_Arguments;
   package LG renames CuBit.Launch_Grants;

   function Printable (B : Unsigned_8) return Boolean is (B in 32 .. 126);

   procedure Decode (Item : Bytes; S : out Signature; Accepted : out Boolean) is
      Position : Natural := Header_Bytes + 1;
      Count, Pieces, Ports, Maps : Natural;
      Length : Natural;
      Empty : constant Signature :=
        (Parameters => [others => (Of_Kind => Text, Many => False, Optional => False,
                                   Name => [others => ' '], Name_Length => 0)],
         Parameter_Total => 0,
         Pieces => [others => (Of_Kind => Literal, Parameter => 0,
                               Text => [others => ' '], Text_Length => 0)],
         Piece_Total => 0,
         Connectors => [others => (Direction => Outlet, Element => Text_Lines, Signal => Stream,
                              Pages => 1, Name => [others => ' '], Name_Length => 0)],
         Connector_Total => 0,
         Descriptors => [others => (Number => 0, Target => 0)],
         Descriptor_Total => 0);
   begin
      S := Empty;
      Accepted := False;
      if Item'Length < Header_Bytes
        or else Item (1) /= Magic_0 or else Item (2) /= Magic_1
        or else Item (3) /= Magic_2 or else Item (4) /= Magic_3
        or else Item (5) /= Version or else Item (6) /= 0
        or else Natural (Item (7)) > Maximum_Parameters
        or else Natural (Item (8)) > Maximum_Pieces
        or else Natural (Item (9)) > Maximum_Connectors
        or else Natural (Item (10)) > Maximum_Descriptors
        or else Item (11) /= 0 or else Item (12) /= 0
      then
         return;
      end if;
      Count := Natural (Item (7));
      Pieces := Natural (Item (8));
      Ports := Natural (Item (9));
      Maps := Natural (Item (10));

      for P in 0 .. Count - 1 loop
         pragma Loop_Invariant (Position in Header_Bytes + 1 .. Item'Last + 1);
         pragma Loop_Invariant (S.Piece_Total = 0);
         pragma Loop_Invariant (S.Connector_Total = 0 and then S.Descriptor_Total = 0);
         if Position + 2 > Item'Last then
            S := Empty;
            return;
         end if;
         Length := Natural (Item (Position + 2));
         if Item (Position) not in 1 .. 6
           or else (Item (Position + 1) and not (Many_Flag or Optional_Flag)) /= 0
           or else Length not in 1 .. Maximum_Name_Bytes
           or else Length > Item'Last - Position - 2
         then
            S := Empty;
            return;
         end if;
         S.Parameters (P).Of_Kind :=
           (case Item (Position) is
               when 1 => Input_File, when 2 => Output_File,
               when 3 => Input_Directory, when 4 => Output_Directory,
               when 5 => Flag, when others => Text);
         S.Parameters (P).Many := (Item (Position + 1) and Many_Flag) /= 0;
         S.Parameters (P).Optional := (Item (Position + 1) and Optional_Flag) /= 0;
         for K in 1 .. Length loop
            if not Printable (Item (Position + 2 + K)) then
               S := Empty;
               return;
            end if;
            S.Parameters (P).Name (K) := Character'Val (Item (Position + 2 + K));
         end loop;
         S.Parameters (P).Name_Length := Length;
         Position := Position + 3 + Length;
      end loop;
      S.Parameter_Total := Count;

      for P in 1 .. Pieces loop
         pragma Loop_Invariant (Position in Header_Bytes + 1 .. Item'Last + 1);
         pragma Loop_Invariant (S.Parameter_Total = Count);
         pragma Loop_Invariant (S.Piece_Total = P - 1);
         pragma Loop_Invariant (S.Connector_Total = 0 and then S.Descriptor_Total = 0);
         pragma Loop_Invariant
           (for all Q in 1 .. P - 1 =>
              (if S.Pieces (Q).Of_Kind /= Literal then
                 S.Pieces (Q).Parameter < S.Parameter_Total));
         pragma Loop_Invariant
           (for all Q in 1 .. P - 1 =>
              (if S.Pieces (Q).Of_Kind /= Value then S.Pieces (Q).Text_Length >= 1));
         if Position + 2 > Item'Last then
            S := Empty;
            return;
         end if;
         Length := Natural (Item (Position + 2));
         if Item (Position) not in 1 .. 3
           or else (Item (Position) = 1 and then Item (Position + 1) /= 0)
           or else (Item (Position) /= 1
                    and then Natural (Item (Position + 1)) >= Count)
           or else (Item (Position) = 2 and then Length /= 0)
           or else (Item (Position) /= 2 and then Length not in 1 .. Maximum_Text_Bytes)
           or else Length > Item'Last - Position - 2
         then
            S := Empty;
            return;
         end if;
         S.Pieces (P).Of_Kind :=
           (case Item (Position) is
               when 1 => Literal, when 2 => Value, when others => When_Set);
         S.Pieces (P).Parameter := Natural (Item (Position + 1));
         for K in 1 .. Length loop
            if not Printable (Item (Position + 2 + K)) then
               S := Empty;
               return;
            end if;
            S.Pieces (P).Text (K) := Character'Val (Item (Position + 2 + K));
         end loop;
         S.Pieces (P).Text_Length := Length;
         S.Piece_Total := P;
         Position := Position + 3 + Length;
      end loop;

      for P in 0 .. Ports - 1 loop
         pragma Loop_Invariant (Position in Header_Bytes + 1 .. Item'Last + 1);
         pragma Loop_Invariant (S.Parameter_Total = Count);
         pragma Loop_Invariant (S.Piece_Total = Pieces);
         pragma Loop_Invariant (S.Connector_Total = P and then S.Descriptor_Total = 0);
         pragma Loop_Invariant
           (for all Q in 1 .. S.Piece_Total =>
              (if S.Pieces (Q).Of_Kind /= Literal then
                 S.Pieces (Q).Parameter < S.Parameter_Total));
         pragma Loop_Invariant
           (for all Q in 1 .. S.Piece_Total =>
              (if S.Pieces (Q).Of_Kind /= Value then S.Pieces (Q).Text_Length >= 1));
         if Position + 4 > Item'Last then
            S := Empty;
            return;
         end if;
         Length := Natural (Item (Position + 4));
         if Item (Position) not in
             Connector_Direction'Enum_Rep (Connector_Direction'First) ..
             Connector_Direction'Enum_Rep (Connector_Direction'Last)
           or else Item (Position + 1) not in
             Element_Kind'Enum_Rep (Element_Kind'First) .. Element_Kind'Enum_Rep (Element_Kind'Last)
           or else Item (Position + 2) not in
             Signal_Kind'Enum_Rep (Signal_Kind'First) .. Signal_Kind'Enum_Rep (Signal_Kind'Last)
           or else Item (Position + 3) = 0
           or else Length not in Minimum_Connector_Name_Bytes .. Maximum_Connector_Name_Bytes
           or else Length > Item'Last - Position - 4
         then
            S := Empty;
            return;
         end if;
         declare
            Name_Length : constant Natural := Length;
            Name : String (1 .. Name_Length) := [others => ' '];
         begin
            for K in 1 .. Length loop
               if not Printable (Item (Position + 4 + K)) then
                  S := Empty;
                  return;
               end if;
               Name (K) := Character'Val (Item (Position + 4 + K));
            end loop;
            if not Valid_Connector_Name (Name) then
               S := Empty;
               return;
            end if;
            for Q in 0 .. P - 1 loop
               if S.Connectors (Q).Name_Length = Length
                 and then S.Connectors (Q).Name (1 .. Length) = Name
               then
                  S := Empty;
                  return;
               end if;
            end loop;
            S.Connectors (P).Name (1 .. Length) := Name;
         end;
         S.Connectors (P).Direction :=
           (if Item (Position) = Connector_Direction'Enum_Rep (Inlet) then Inlet else Outlet);
         S.Connectors (P).Element :=
           (case Item (Position + 1) is
               when Element_Kind'Enum_Rep (Text_Lines) => Text_Lines,
               when Element_Kind'Enum_Rep (Raw_Bytes) => Raw_Bytes,
               when Element_Kind'Enum_Rep (Integers) => Integers,
               when others => Log_Records);
         S.Connectors (P).Signal :=
           (case Item (Position + 2) is
               when Signal_Kind'Enum_Rep (Stream) => Stream,
               when Signal_Kind'Enum_Rep (Level) => Level,
               when others => Edge);
         S.Connectors (P).Pages := Natural (Item (Position + 3));
         S.Connectors (P).Name_Length := Length;
         S.Connector_Total := P + 1;
         Position := Position + 5 + Length;
      end loop;

      for D in 1 .. Maps loop
         pragma Loop_Invariant (Position in Header_Bytes + 1 .. Item'Last + 1);
         pragma Loop_Invariant (S.Parameter_Total = Count);
         pragma Loop_Invariant (S.Piece_Total = Pieces);
         pragma Loop_Invariant (S.Connector_Total = Ports);
         pragma Loop_Invariant (S.Descriptor_Total = D - 1);
         pragma Loop_Invariant
           (for all Q in 1 .. S.Piece_Total =>
              (if S.Pieces (Q).Of_Kind /= Literal then
                 S.Pieces (Q).Parameter < S.Parameter_Total));
         pragma Loop_Invariant
           (for all Q in 1 .. S.Piece_Total =>
              (if S.Pieces (Q).Of_Kind /= Value then S.Pieces (Q).Text_Length >= 1));
         pragma Loop_Invariant
           (for all Q in 1 .. D - 1 => S.Descriptors (Q).Target < S.Connector_Total);
         if Position + 1 > Item'Last
           or else Natural (Item (Position + 1)) >= Ports
         then
            S := Empty;
            return;
         end if;
         --  Descriptor 0 reads an inlet; every other one writes to an
         --  outlet; no descriptor is mapped twice.
         if (Item (Position) = 0)
              /= (S.Connectors (Natural (Item (Position + 1))).Direction = Inlet)
         then
            S := Empty;
            return;
         end if;
         for Q in 1 .. D - 1 loop
            if S.Descriptors (Q).Number = Natural (Item (Position)) then
               S := Empty;
               return;
            end if;
         end loop;
         S.Descriptors (D) :=
           (Number => Natural (Item (Position)), Target => Natural (Item (Position + 1)));
         S.Descriptor_Total := D;
         Position := Position + 2;
      end loop;

      if Position /= Item'Last + 1 then
         S := Empty;
         return;
      end if;
      Accepted := True;
   end Decode;

   procedure Find_Connector (S : Signature; Name : String; Index : out Connector_Index;
                        Found : out Boolean) is
   begin
      Index := 0;
      Found := False;
      for P in 0 .. S.Connector_Total - 1 loop
         if S.Connectors (P).Name (1 .. S.Connectors (P).Name_Length) = Name then
            Index := P;
            Found := True;
            return;
         end if;
      end loop;
   end Find_Connector;

   procedure Find (S : Signature; Name : String; Index : out Parameter_Index;
                   Found : out Boolean) is
   begin
      Index := 0;
      Found := False;
      for P in 0 .. S.Parameter_Total - 1 loop
         if S.Parameters (P).Name (1 .. S.Parameters (P).Name_Length) = Name then
            Index := P;
            Found := True;
            return;
         end if;
      end loop;
   end Find;

   procedure Clear (V : out Values) is
   begin
      V := (Entries => [others => (Parameter => 0, First => 1, Last => 0)],
            Total => 0, Text => [others => ' '], Used => 0);
   end Clear;

   procedure Add (V : in out Values; P : Parameter_Index; Item : String;
                  Added : out Boolean) is
   begin
      Added := V.Total < Maximum_Values
        and then Item'Length <= Maximum_Value_Text - V.Used;
      if not Added then
         return;
      end if;
      V.Text (V.Used + 1 .. V.Used + Item'Length) := Item;
      V.Total := V.Total + 1;
      V.Entries (V.Total) :=
        (Parameter => P, First => V.Used + 1, Last => V.Used + Item'Length);
      V.Used := V.Used + Item'Length;
   end Add;

   function Is_File (K : Kind) return Boolean is (K not in Flag | Text);

   procedure Render
     (S : Signature; V : Values; Program : String;
      Block : out CuBit.Launch_Arguments.Builder;
      Grants : out CuBit.Launch_Grants.Builder;
      Result : out Check_Result)
   is
      Counts : array (Parameter_Index) of Value_Count := [others => 0];
      Accepted : Boolean;
      Length : LA.Present_Length;
   begin
      LA.Start (Block);
      LG.Start (Grants);

      --  Check every value against the signature.
      for E in 1 .. V.Total loop
         declare
            Item : Value_Entry renames V.Entries (E);
         begin
            if Item.Parameter >= S.Parameter_Total then
               Result := Unknown_Parameter;
               return;
            end if;
            if Counts (Item.Parameter) < Value_Count'Last then
               Counts (Item.Parameter) := Counts (Item.Parameter) + 1;
            end if;
            if Counts (Item.Parameter) > 1 and then not S.Parameters (Item.Parameter).Many then
               Result := Too_Many_Values;
               return;
            end if;
            for K in Item.First .. Item.Last loop
               if V.Text (K) = Character'Val (0) then
                  Result := Bad_Text;
                  return;
               end if;
            end loop;
            if Is_File (S.Parameters (Item.Parameter).Of_Kind) then
               declare
                  Name : constant String (1 .. Item.Last - Item.First + 1) :=
                    V.Text (Item.First .. Item.Last);
               begin
                  if not LG.Valid_Name (Name) then
                     Result := Bad_File_Name;
                     return;
                  end if;
               end;
            end if;
         end;
      end loop;
      for P in 0 .. S.Parameter_Total - 1 loop
         if Counts (P) = 0 and then not S.Parameters (P).Optional
           and then S.Parameters (P).Of_Kind /= Flag
         then
            Result := Missing_Parameter;
            return;
         end if;
      end loop;

      --  argv: the program, then the pieces in order.
      Result := Too_Large;
      LA.Add_Argument (Block, Program, Accepted);
      if not Accepted then
         return;
      end if;
      for P in 1 .. S.Piece_Total loop
         pragma Loop_Invariant (LA.Builder_Valid (Block));
         declare
            Item : Piece renames S.Pieces (P);
         begin
            case Item.Of_Kind is
               when Literal =>
                  LA.Add_Argument (Block, Item.Text (1 .. Item.Text_Length), Accepted);
                  if not Accepted then
                     return;
                  end if;
               when When_Set =>
                  if Counts (Item.Parameter) > 0 then
                     LA.Add_Argument (Block, Item.Text (1 .. Item.Text_Length), Accepted);
                     if not Accepted then
                        return;
                     end if;
                  end if;
               when Value =>
                  if S.Parameters (Item.Parameter).Of_Kind /= Flag then
                     for E in 1 .. V.Total loop
                        pragma Loop_Invariant (LA.Builder_Valid (Block));
                        if V.Entries (E).Parameter = Item.Parameter then
                           LA.Add_Argument
                             (Block, V.Text (V.Entries (E).First .. V.Entries (E).Last),
                              Accepted);
                           if not Accepted then
                              return;
                           end if;
                        end if;
                     end loop;
                  end if;
            end case;
         end;
      end loop;
      pragma Warnings (GNATprove, Off, "is set by",
                       Reason => "the block's length stays in Block.Used");
      LA.Finish (Block, Length, Accepted);
      pragma Warnings (GNATprove, On, "is set by");
      if not Accepted then
         return;
      end if;

      --  One place per file value.
      Result := Too_Many_Places;
      for E in 1 .. V.Total loop
         declare
            Item : Value_Entry renames V.Entries (E);
            K : constant Kind := S.Parameters (Item.Parameter).Of_Kind;
         begin
            if Is_File (K) then
               declare
                  Name : constant String (1 .. Item.Last - Item.First + 1) :=
                    V.Text (Item.First .. Item.Last);
               begin
                  LG.Add (Grants, Rights_For (K), Name, Accepted);
                  if not Accepted then
                     return;
                  end if;
               end;
            end if;
         end;
      end loop;
      Result := Matches;
   end Render;

end CuBit.Program_Descriptions;
