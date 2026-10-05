------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Path_Names with SPARK_Mode is

   --  A name being built: the volume Result (1 .. Root), then components,
   --  each one '/' and at least one other character; no ".." component.
   function Building
     (Result : Name; Root, Length, Limit : Natural) return Boolean is
     (Limit <= Maximum_Name_Bytes
      and then Root in 1 .. Limit and then Length in Root .. Limit
      and then Result (1) = Volume_Mark
      and then (for all K in 1 .. Root => Result (K) /= Separator)
      and then (Length = Root
                or else (Result (Root + 1) = Separator
                         and then Result (Length) /= Separator))
      and then (for all I in Root + 1 .. Length - 1 =>
                  (if Result (I) = Separator
                   then Result (I + 1) /= Separator))
      and then No_Parent_Component (Result, Root, Length))
   with Ghost;

   --  The first separator in Text at or after From, or Text'Last + 1.
   function Next_Separator (Text : String; From : Positive) return Positive
   with Pre => Text'First = 1 and then Text'Last < Maximum_Path_Bytes + 1
               and then From in 1 .. Text'Last + 1,
        Post => Next_Separator'Result in From .. Text'Last + 1
                and then (for all K in From .. Next_Separator'Result - 1 =>
                            Text (K) /= Separator)
                and then (Next_Separator'Result = Text'Last + 1
                          or else Text (Next_Separator'Result) = Separator);

   function Next_Separator (Text : String; From : Positive) return Positive is
   begin
      for K in From .. Text'Last loop
         if Text (K) = Separator then
            return K;
         end if;
         pragma Loop_Invariant (for all J in From .. K => Text (J) /= Separator);
      end loop;
      return Text'Last + 1;
   end Next_Separator;

   procedure Resolve
     (Base, Path : String; Capacity : Name_Length;
      Result : out Name; Length : out Name_Length; Status : out Resolution)
   is
      Limit : constant Natural := (if Capacity = 0 then 0 else Capacity - 1);
      Root : Natural;
      Over : Boolean := False;

      --  Remove the last component (none: the volume stays).
      procedure Pop
      with Pre => Building (Result, Root, Length, Limit),
           Post => Building (Result, Root, Length, Limit)
                   and then Length <= Length'Old;

      procedure Pop is
         K : Natural := Length;
      begin
         if Length = Root then
            return;
         end if;
         --  The separator that starts the last component.
         while Result (K) /= Separator loop
            pragma Loop_Invariant (K in Root + 1 .. Length);
            pragma Loop_Invariant
              (for all J in K .. Length => Result (J) /= Separator);
            pragma Loop_Variant (Decreases => K);
            K := K - 1;
         end loop;
         pragma Assert (K in Root + 1 .. Length and then Result (K) = Separator);
         pragma Assert (K = Root + 1 or else Result (K - 1) /= Separator);
         Length := K - 1;
      end Pop;

      --  Append '/' and Text (First .. Last), a component: no separator,
      --  not "." or "..". Over when it would pass Limit (Result unchanged).
      procedure Append (Text : String; First, Last : Positive)
      with Pre => Building (Result, Root, Length, Limit)
                  and then Text'First = 1
                  and then Text'Last <= Maximum_Path_Bytes
                  and then First <= Last and then Last <= Text'Last
                  and then (for all K in First .. Last => Text (K) /= Separator)
                  and then not (Last = First + 1 and then Text (First) = Dot
                                and then Text (Last) = Dot),
           Post => Building (Result, Root, Length, Limit)
                   and then Root = Root'Old;

      procedure Append (Text : String; First, Last : Positive) is
         Count : constant Positive := Last - First + 1;
         Old_Length : constant Natural := Length with Ghost;
      begin
         if Count >= Limit - Length then
            Over := True;
            return;
         end if;
         Result (Length + 1) := Separator;
         Result (Length + 2 .. Length + 1 + Count) := Text (First .. Last);
         Length := Length + 1 + Count;
         pragma Assert (for all J in Old_Length + 2 .. Length =>
                          Result (J) /= Separator);
      end Append;

      --  Start the name with the volume Text (1 .. Last).
      procedure Put_Volume (Text : String; Last : Natural; Fits : out Boolean)
      with Pre => Text'First = 1 and then Last <= Text'Last
                  and then Last >= 1 and then Text (1) = Volume_Mark
                  and then (for all K in 1 .. Last => Text (K) /= Separator),
           Post => (if Fits then Building (Result, Root, Length, Limit)
                    and then Root = Last);

      procedure Put_Volume (Text : String; Last : Natural; Fits : out Boolean)
      is
      begin
         Fits := Last < Limit;
         if not Fits then
            return;
         end if;
         Result (1 .. Last) := Text (1 .. Last);
         Root := Last;
         Length := Last;
      end Put_Volume;

      --  Each component of Text (From .. Text'Last), in order.
      procedure Walk (Text : String; From : Positive)
      with Pre => Text'First = 1 and then Text'Last <= Maximum_Path_Bytes
                  and then From in 1 .. Text'Last + 1
                  and then Building (Result, Root, Length, Limit)
                  and then not Over,
           Post => Building (Result, Root, Length, Limit)
                   and then Root = Root'Old;

      procedure Walk (Text : String; From : Positive) is
         I : Positive := From;
         Stop : Positive;
      begin
         while I <= Text'Last loop
            pragma Loop_Invariant (Building (Result, Root, Length, Limit));
            pragma Loop_Invariant (I in From .. Text'Last);
            pragma Loop_Invariant (Root = Root'Loop_Entry);
            pragma Loop_Variant (Increases => I);
            if Text (I) = Separator then
               I := I + 1;
            else
               Stop := Next_Separator (Text, I);
               if Stop = I + 1 and then Text (I) = Dot then
                  null;                                     --  "."
               elsif Stop = I + 2 and then Text (I) = Dot
                 and then Text (I + 1) = Dot
               then
                  Pop;                                      --  ".."
               else
                  Append (Text, I, Stop - 1);
                  exit when Over;
               end if;
               exit when Stop > Text'Last;
               I := Stop;
            end if;
         end loop;
      end Walk;

      Volume_End : Positive;
      Fits : Boolean;
   begin
      Result := [others => ' '];
      Length := 0;
      Root := 0;
      if Path'Length = 0 then
         Status := Empty_Path;
         return;
      end if;

      if Path (1) = Volume_Mark then
         Volume_End := Next_Separator (Path, 1);
         Put_Volume (Path, Volume_End - 1, Fits);
         if Fits then
            Walk (Path, Volume_End);
         end if;
      elsif Path (1) = Separator then
         Put_Volume (System_Volume, System_Volume'Length, Fits);
         if Fits then
            Walk (Path, 1);
         end if;
      elsif Base'Length = 0 or else Base (1) /= Volume_Mark then
         Status := Invalid_Base;
         return;
      else
         Volume_End := Next_Separator (Base, 1);
         Put_Volume (Base, Volume_End - 1, Fits);
         if Fits then
            Walk (Base, Volume_End);
            if not Over then
               Walk (Path, 1);
            end if;
         end if;
      end if;

      if not Fits or else Over or else
        (Length = Root and then Length >= Limit)
      then
         Length := 0;
         Status := Too_Long;
         return;
      end if;
      if Length = Root then
         Length := Length + 1;
         Result (Length) := Separator;              --  the volume's root
      end if;
      pragma Assert (Result (Root + 1) = Separator);
      Status := Resolved;
   end Resolve;

   procedure Display
     (Item : String; Result : out Name; Length : out Name_Length)
   is
      Prefix : constant Natural := System_Volume'Length;
      On_System : constant Boolean :=
        Item'Length >= Prefix and then Item (1 .. Prefix) = System_Volume
        and then (Item'Length = Prefix or else Item (Prefix + 1) = Separator);
   begin
      Result := [others => ' '];
      if Item'Length = 0 or else (On_System and then Item'Length <= Prefix + 1)
      then
         Length := 1;                           --  the system volume's root
         Result (1) := Separator;
      elsif On_System then
         Length := Item'Length - Prefix;        --  "/src"
         Result (1 .. Length) := Item (Prefix + 1 .. Item'Last);
      else
         Length := Item'Length;                 --  "@usb:0/x"
         Result (1 .. Length) := Item;
      end if;
   end Display;

end CuBit.Path_Names;
