------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
package body Debugger_Model with SPARK_Mode => Off is

   function To_Text (S : String) return Text is
      T : Text;
      N : constant Natural := Natural'Min (S'Length, TEXT_CAPACITY);
   begin
      T.Data (1 .. N) := S (S'First .. S'First + N - 1);
      T.Length := N;
      return T;
   end To_Text;

   function To_Short (S : String) return Short_Text is
      T : Short_Text;
      N : constant Natural := Natural'Min (S'Length, SHORT_CAPACITY);
   begin
      T.Data (1 .. N) := S (S'First .. S'First + N - 1);
      T.Length := N;
      return T;
   end To_Short;

   procedure Clear_Source (S : in out Snapshot) is
   begin
      S.Pool_Used := 0;
      S.Line_Total := 0;
   end Clear_Source;

   procedure Add_Source_Line (S : in out Snapshot; Line : String; Added : out Boolean) is
   begin
      Added := False;
      if S.Line_Total >= MAX_SOURCE_LINES or else Line'Length > SOURCE_POOL_BYTES - S.Pool_Used then
         return;
      end if;
      S.Line_Total := S.Line_Total + 1;
      S.Lines (S.Line_Total) :=
        (First => Pool_Index'Min (S.Pool_Used + 1, SOURCE_POOL_BYTES), Last => S.Pool_Used + Line'Length);
      S.Source_Pool (S.Pool_Used + 1 .. S.Pool_Used + Line'Length) := Line;
      S.Pool_Used := S.Pool_Used + Line'Length;
      Added := True;
   end Add_Source_Line;

   function Source_Line (S : Snapshot; N : Line_Number) return String is
     (S.Source_Pool (S.Lines (N).First .. S.Lines (N).Last));

   procedure Add_Local (S : in out Snapshot; E : Local_Entry; Added : out Boolean) is
   begin
      Added := S.Local_Total < MAX_LOCALS;
      if Added then
         S.Local_Total := S.Local_Total + 1;
         S.Locals (S.Local_Total) := E;
      else
         S.Locals_Cut := True;
      end if;
   end Add_Local;

   function Subtree_Last (S : Snapshot; Parent : Local_Index) return Local_Index is
      Last : Local_Index := Parent;
   begin
      while Last < S.Local_Total and then S.Locals (Last + 1).Depth > S.Locals (Parent).Depth loop
         Last := Last + 1;
      end loop;
      return Last;
   end Subtree_Last;

   procedure Insert_Children (S : in out Snapshot; Parent : Local_Index; Children : Local_Batch) is
      Room  : constant Natural := MAX_LOCALS - S.Local_Total;
      Count : constant Natural := Natural'Min (Children'Length, Room);
      After : constant Local_Index := Subtree_Last (S, Parent);
      Depth : constant Tree_Depth := Tree_Depth'Min (S.Locals (Parent).Depth + 1, MAX_DEPTH);
   begin
      if Count < Children'Length then
         S.Locals_Cut := True;
      end if;
      if Count > 0 then
         --  Open a gap after Parent's subtree.
         for I in reverse After + 1 .. S.Local_Total loop
            S.Locals (I + Count) := S.Locals (I);
         end loop;
         for K in 0 .. Count - 1 loop
            S.Locals (After + 1 + K) := Children (Children'First + K);
            S.Locals (After + 1 + K).Depth := Depth;
         end loop;
         S.Local_Total := S.Local_Total + Count;
      end if;
      S.Locals (Parent).Loaded := True;
      S.Locals (Parent).Expanded := True;
   end Insert_Children;

   procedure Add_Node (S : in out Snapshot; N : Graph_Node; Index : out Natural) is
   begin
      Index := Find_Node (S, N.Id);
      if Index /= 0 then
         return;
      end if;
      if S.Node_Total >= MAX_GRAPH_NODES then
         S.Graph_Cut := True;
         return;
      end if;
      S.Node_Total := S.Node_Total + 1;
      S.Nodes (S.Node_Total) := N;
      Index := S.Node_Total;
   end Add_Node;

   function Find_Node (S : Snapshot; Id : Node_Id) return Natural is
   begin
      for I in 1 .. S.Node_Total loop
         if S.Nodes (I).Id = Id then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Node;

   procedure Add_Edge (S : in out Snapshot; E : Graph_Edge) is
   begin
      for I in 1 .. S.Edge_Total loop
         if S.Edges (I).From = E.From and then S.Edges (I).From_Field = E.From_Field then
            S.Edges (I) := E;
            return;
         end if;
      end loop;
      if S.Edge_Total >= MAX_GRAPH_EDGES then
         S.Graph_Cut := True;
         return;
      end if;
      S.Edge_Total := S.Edge_Total + 1;
      S.Edges (S.Edge_Total) := E;
   end Add_Edge;

   procedure Classify_Edges (S : in out Snapshot) is
      --  Depth-first order from the roots: an edge to a node still on the
      --  stack is a back edge.
      type Mark is (White, Grey, Black);
      Marks : array (Node_Index) of Mark := [others => White];
      procedure Visit (N : Node_Index; Level : Natural) is
      begin
         Marks (N) := Grey;
         for E in 1 .. S.Edge_Total loop
            if S.Edges (E).From = N then
               case Marks (S.Edges (E).To) is
                  when Grey => S.Edges (E).Back := True;
                  when White =>
                     S.Edges (E).Back := False;
                     if Level < MAX_GRAPH_NODES then
                        Visit (S.Edges (E).To, Level + 1);
                     end if;
                  when Black => S.Edges (E).Back := False;
               end case;
            end if;
         end loop;
         Marks (N) := Black;
      end Visit;
   begin
      for N in 1 .. S.Node_Total loop
         if S.Nodes (N).Is_Root and then Marks (N) = White then
            Visit (N, 0);
         end if;
      end loop;
      for N in 1 .. S.Node_Total loop
         if Marks (N) = White then
            Visit (N, 0);
         end if;
      end loop;
   end Classify_Edges;

   function Name_Path (S : Snapshot; I : Local_Index) return String is
   begin
      if S.Locals (I).Depth = 0 then
         return Image (S.Locals (I).Name);
      end if;
      for P in reverse 1 .. I - 1 loop
         if S.Locals (P).Depth < S.Locals (I).Depth then
            return Name_Path (S, P) & "." & Image (S.Locals (I).Name);
         end if;
      end loop;
      return Image (S.Locals (I).Name);
   end Name_Path;

   function Was_Expanded (Before : Snapshot; Name_Path : String) return Boolean is
   begin
      for I in 1 .. Before.Local_Total loop
         if Before.Locals (I).Expanded and then Debugger_Model.Name_Path (Before, I) = Name_Path then
            return True;
         end if;
      end loop;
      return False;
   end Was_Expanded;

   procedure Mark_Changes (Before : Snapshot; After : in out Snapshot; Now : Microseconds) is
   begin
      --  Locals, by name path.
      for I in 1 .. After.Local_Total loop
         declare
            Path : constant String := Name_Path (After, I);
         begin
            for J in 1 .. Before.Local_Total loop
               if Name_Path (Before, J) = Path then
                  if Image (Before.Locals (J).Value) /= Image (After.Locals (I).Value) then
                     After.Locals (I).Changed_At := Now;
                  elsif After.Locals (I).Changed_At = NEVER then
                     After.Locals (I).Changed_At := Before.Locals (J).Changed_At;
                  end if;
                  exit;
               end if;
            end loop;
         end;
      end loop;
      --  Graph fields, by node identity and field name; new nodes appear.
      for N in 1 .. After.Node_Total loop
         declare
            Old : constant Natural := Find_Node (Before, After.Nodes (N).Id);
         begin
            if Old = 0 then
               if Before.Generation > 0 then
                  After.Nodes (N).Appeared_At := Now;
               end if;
            else
               if After.Nodes (N).Appeared_At = NEVER then
                  After.Nodes (N).Appeared_At := Before.Nodes (Old).Appeared_At;
               end if;
               for F in 1 .. After.Nodes (N).Field_Total loop
                  for G in 1 .. Before.Nodes (Old).Field_Total loop
                     if Image (Before.Nodes (Old).Fields (G).Name) = Image (After.Nodes (N).Fields (F).Name) then
                        if Image (Before.Nodes (Old).Fields (G).Value) /= Image (After.Nodes (N).Fields (F).Value)
                        then
                           After.Nodes (N).Fields (F).Changed_At := Now;
                        elsif After.Nodes (N).Fields (F).Changed_At = NEVER then
                           After.Nodes (N).Fields (F).Changed_At := Before.Nodes (Old).Fields (G).Changed_At;
                        end if;
                        exit;
                     end if;
                  end loop;
               end loop;
            end if;
         end;
      end loop;
      --  Memory, when the window did not move.
      if Before.Mem_Base = After.Mem_Base and then Before.Mem_Length = After.Mem_Length then
         After.Mem_Prev := Before.Mem_Bytes;
         if Before.Mem_Bytes /= After.Mem_Bytes then
            After.Mem_Changed_At := Now;
         else
            After.Mem_Changed_At := Before.Mem_Changed_At;
            After.Mem_Prev := Before.Mem_Prev;
         end if;
      else
         After.Mem_Prev := After.Mem_Bytes;
      end if;
   end Mark_Changes;
   --  The selected frame's own package is implied, as a "use" clause would
   --  make it: "Demo_World.Percent" shows as "Percent" while a Demo_World
   --  subprogram is selected. Only the display changes.
   function Package_Prefix (S : Snapshot) return String is
      --  Ada's "Pkg." or Rust's "crate::module::".
      Func : constant String :=
        (if S.Frame_Total >= S.Frame_Sel and then S.Frame_Total > 0 then Image (S.Frames (S.Frame_Sel).Func) else "");
   begin
      for I in reverse Func'First .. Func'Last - 1 loop
         if Func (I) = ':' and then Func (I + 1) = ':' then
            return Func (Func'First .. I + 1);
         end if;
      end loop;
      for I in reverse Func'Range loop
         if Func (I) = '.' then
            return Func (Func'First .. I);
         end if;
      end loop;
      return "";
   end Package_Prefix;

   function Without_Package (S : Snapshot; Text : String) return String is
      Prefix : constant String := Package_Prefix (S);
      Result : String (1 .. Text'Length);
      Last : Natural := 0;
      I : Natural := Text'First;
      function Ident (C : Character) return Boolean is (C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.' | ':');
   begin
      if Prefix'Length = 0 then
         return Text;
      end if;
      while I <= Text'Last loop
         if I + Prefix'Length - 1 <= Text'Last and then Text (I .. I + Prefix'Length - 1) = Prefix
           and then (I = Text'First or else not Ident (Text (I - 1)))
         then
            I := I + Prefix'Length;
         else
            Last := Last + 1;
            Result (Last) := Text (I);
            I := I + 1;
         end if;
      end loop;
      return Result (1 .. Last);
   end Without_Package;
end Debugger_Model;
