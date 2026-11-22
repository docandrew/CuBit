package body Config_Tree is
   function Key (Item : Model; ID : Node_ID) return String is
     (if ID in 1 .. Item.Count then Item.Nodes (ID).Path (1 .. Item.Nodes (ID).Length) else "");
   function Label (Item : Model; ID : Node_ID) return String is
     (if ID = 1 then "Machine"
      elsif ID in 2 .. Item.Count then
         Item.Nodes (ID).Path (Item.Nodes (ID).Label_First .. Item.Nodes (ID).Length)
      else "");
   function Find (Item : Model; Path : String) return Node_ID is
   begin
      for I in 1 .. Item.Count loop
         if Key (Item, I) = Path then return I; end if;
      end loop;
      return 0;
   end Find;
   procedure Load (Item : in out Model; Names : String; Success : out Boolean) is
      Candidate : Model;
      Name : String (1 .. 128) := [others => ' '];
      Length : Name_Length := 0;
      Valid : Boolean := True;
      procedure Add_Key is
         Parent : Node_ID := 1;
         Child, Tail, Previous : Node_ID;
         First : Positive := 1;
      begin
         if Length = 0 then Valid := False; return; end if;
         for Last in 1 .. Length loop
            if Last = Length or else Name (Last + 1) = '.' then
               if Name (Last) = '.' then Valid := False; return; end if;
               Child := Find (Candidate, Name (1 .. Last));
               if Child = 0 then
                  if Candidate.Count = Maximum_Nodes then Valid := False; return; end if;
                  Candidate.Count := Candidate.Count + 1;
                  Child := Candidate.Count;
                  Candidate.Nodes (Child).Path (1 .. Last) := Name (1 .. Last);
                  Candidate.Nodes (Child).Length := Last;
                  Candidate.Nodes (Child).Label_First := First;
                  Candidate.Nodes (Child).Parent := Parent;
                  Candidate.Nodes (Child).Depth := Candidate.Nodes (Parent).Depth + 1;
                  Previous := Find (Item, Name (1 .. Last));
                  Candidate.Nodes (Child).Expanded :=
                    (if Previous /= 0 then Item.Nodes (Previous).Expanded else True);
                  if Candidate.Nodes (Parent).Child = 0 then
                     Candidate.Nodes (Parent).Child := Child;
                  else
                     Tail := Candidate.Nodes (Parent).Child;
                     while Candidate.Nodes (Tail).Next /= 0 loop Tail := Candidate.Nodes (Tail).Next; end loop;
                     Candidate.Nodes (Tail).Next := Child;
                  end if;
               end if;
               Parent := Child;
               First := Last + 2;
            end if;
         end loop;
         Candidate.Nodes (Parent).Has_Value := True;
      end Add_Key;
   begin
      Success := False;
      Candidate.Nodes (1).Expanded := Item.Nodes (1).Expanded;
      for Ch of Names loop
         if Ch = ASCII.LF then
            Add_Key;
            if not Valid then return; end if;
            Length := 0;
         elsif Length = Name'Length or else Ch < ' ' or else Ch > '~' then
            return;
         else
            Length := Length + 1;
            Name (Length) := Ch;
            if Ch = '.' and then (Length = 1 or else Name (Length - 1) = '.') then return; end if;
         end if;
      end loop;
      if Length /= 0 then Add_Key; end if;
      if not Valid then return; end if;
      Item := Candidate;
      Success := True;
   end Load;
   function Visible (Item : Model) return Rows is
      Result : Rows;
      ID : Node_ID := 1;
   begin
      while ID /= 0 loop
         Result.Count := Result.Count + 1;
         Result.IDs (Result.Count) := ID;
         if Item.Nodes (ID).Expanded and Item.Nodes (ID).Child /= 0 then
            ID := Item.Nodes (ID).Child;
         else
            while ID /= 0 and then Item.Nodes (ID).Next = 0 loop ID := Item.Nodes (ID).Parent; end loop;
            if ID /= 0 then ID := Item.Nodes (ID).Next; end if;
         end if;
      end loop;
      return Result;
   end Visible;
end Config_Tree;
