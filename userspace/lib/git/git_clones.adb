------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Writing a clone (see the spec). File formats from git's documentation
--  (gitformat-pack, gitformat-index, gitrepository-layout, git-config);
--  facts only.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Unchecked_Deallocation;
with Git_Deltas;
with Git_Hashes;
with Git_Objects;

package body Git_Clones is
   use type Interfaces.Unsigned_32;
   use type Git_Packs.Event_Kind;
   use type Git_Packs.Object_Kind;
   use type Git_Objects.Tree_Refusal;
   use type Git_Objects.Entry_Mode;
   use type Git_Packs.Refusal;
   use type Git_Deltas.Refusal;
   subtype U32 is Interfaces.Unsigned_32;

   MAXIMUM_PATH_BYTES : constant := 4_096;
   HEADS : constant String := "refs/heads/";
   --  The temporary directory's suffix (".DIRECTORY.cloning").
   CLONING : constant String := ".cloning";
   LF : constant Character := Character'Val (10);
   TAB : constant Character := Character'Val (9);
   --  gitformat-index: the stage-0 flags hold the name length up to this.
   INDEX_NAME_MASK : constant := 16#0FFF#;
   --  gitformat-pack: offsets from 2 ** 31 go in the 8-byte table.
   LARGE_OFFSET : constant := 2 ** 31;
   --  git's modes, as the index and tree record them.
   MODE_REGULAR : constant := 8#100644#;
   MODE_EXECUTABLE : constant := 8#100755#;
   MODE_LINK : constant := 8#120000#;
   MODE_GITLINK : constant := 8#160000#;
   OUTPUT_CHUNK : constant := 65_536;

   procedure Free is new Ada.Unchecked_Deallocation (Byte_Array, Bytes_Access);

   ---------------------------------------------------------------------------
   --  Byte buffers for the files written.

   type Buffer is record
      Data : Bytes_Access := new Byte_Array (1 .. 4_096);
      Length : Natural := 0;
   end record;

   procedure Append (B : in out Buffer; Bytes : Byte_Array) is
   begin
      if B.Length + Bytes'Length > B.Data'Length then
         declare
            Bigger : constant Bytes_Access :=
              new Byte_Array (1 .. Natural'Max (2 * B.Data'Length, B.Length + Bytes'Length));
         begin
            Bigger (1 .. B.Length) := B.Data (1 .. B.Length);
            Free (B.Data);
            B.Data := Bigger;
         end;
      end if;
      B.Data (B.Length + 1 .. B.Length + Bytes'Length) := Bytes;
      B.Length := B.Length + Bytes'Length;
   end Append;

   procedure Append (B : in out Buffer; Text : String) is
      Bytes : Byte_Array (1 .. Text'Length);
   begin
      for K in Text'Range loop
         Bytes (K - Text'First + 1) := Character'Pos (Text (K));
      end loop;
      Append (B, Bytes);
   end Append;

   procedure Append_32 (B : in out Buffer; V : U32) is
   begin
      Append (B, Byte_Array'(Byte (Interfaces.Shift_Right (V, 24) and 255), Byte (Interfaces.Shift_Right (V, 16) and 255),
                             Byte (Interfaces.Shift_Right (V, 8) and 255), Byte (V and 255)));
   end Append_32;

   --  An object name: the format's bytes of it.
   procedure Append_Id (B : in out Buffer; Format : Object_Format; Id : Object_Id) is
   begin
      Append (B, Byte_Array (Id (1 .. Id_Bytes (Format))));
   end Append_Id;

   --  The format's hash of the buffer, appended (index and pack index
   --  trailers).
   procedure Seal (B : in out Buffer; Format : Object_Format) is
      Ctx : Git_Hashes.Context;
      Id : Object_Id;
   begin
      Git_Hashes.Start (Ctx, Format);
      Git_Hashes.Update (Ctx, B.Data (1 .. B.Length));
      Git_Hashes.Finish (Ctx, Id);
      Append_Id (B, Format, Id);
   end Seal;

   ---------------------------------------------------------------------------

   function Result (C : Clone) return Outcome is (C.Result);
   function Format (C : Clone) return Object_Format is (C.Format);
   function Hex (C : Clone; Id : Object_Id) return String is (To_Hex (Id, C.Format));
   function Detail (C : Clone) return String is (To_String (C.Detail));
   function Objects (C : Clone) return Natural is (C.Entries.Length);
   function Files (C : Clone) return Natural is (C.Files);

   function Root (C : Clone) return String is (To_String (C.Parent) & "/" & To_String (C.Temporary));
   function Git_Dir (C : Clone) return String is (Root (C) & "/.git");

   --  In-place heapsort (no allocation; CuBit's runtime has no
   --  Ada.Containers.Generic_Array_Sort).
   generic
      type Item is private;
      type Items is array (Positive range <>) of Item;
      with function "<" (Left, Right : Item) return Boolean is <>;
   procedure Heap_Sort (A : in out Items);
   procedure Heap_Sort (A : in out Items) is
      procedure Swap (I, J : Positive) is
         T : constant Item := A (I);
      begin
         A (I) := A (J);
         A (J) := T;
      end Swap;
      --  Sift A (First + Root - 1) down within the first Size items.
      procedure Sift (Root_At, Size : Positive) is
         Parent : Positive := Root_At;
         Child : Positive;
      begin
         while 2 * Parent <= Size loop
            Child := 2 * Parent;
            if Child < Size and then A (A'First + Child - 1) < A (A'First + Child) then
               Child := Child + 1;
            end if;
            exit when not (A (A'First + Parent - 1) < A (A'First + Child - 1));
            Swap (A'First + Parent - 1, A'First + Child - 1);
            Parent := Child;
         end loop;
      end Sift;
   begin
      if A'Length < 2 then
         return;
      end if;
      for Start in reverse 1 .. A'Length / 2 loop
         Sift (Start, A'Length);
      end loop;
      for Size in reverse 2 .. A'Length loop
         Swap (A'First, A'First + Size - 1);
         Sift (1, Size - 1);
      end loop;
   end Heap_Sort;

   --  The entry at a pack offset (entries are in pack order), or 0.
   function Entry_At (C : Clone; Offset : Git_Packs.Pack_Offset) return Natural is
      Low : Positive := 1;
      High : Natural := C.Entries.Length;
   begin
      while Low <= High loop
         declare
            Middle : constant Positive := Low + (High - Low) / 2;
            At_Middle : constant Git_Packs.Pack_Offset := C.Entries (Middle).Offset;
         begin
            if At_Middle = Offset then
               return Middle;
            elsif At_Middle < Offset then
               Low := Middle + 1;
            else
               High := Middle - 1;
            end if;
         end;
      end loop;
      return 0;
   end Entry_At;

   --  The table position an object name starts probing at.
   function Home (C : Clone; Id : Object_Id) return Positive is
     (Natural ((U32 (Id (1)) * 2 ** 24 + U32 (Id (2)) * 2 ** 16 + U32 (Id (3)) * 2 ** 8 + U32 (Id (4)))
               mod U32 (C.Id_Slots.Length)) + 1);

   --  The entry an object name names, or 0.
   function Object_Of (C : Clone; Id : Object_Id) return Natural is
      Size : constant Natural := C.Id_Slots.Length;
      P : Positive;
   begin
      if Size = 0 then
         return 0;
      end if;
      P := Home (C, Id);
      for Probe in 1 .. Size loop
         exit when C.Id_Slots (P) = 0;
         if C.Entries (C.Id_Slots (P)).Id = Id then
            return C.Id_Slots (P);
         end if;
         P := (if P = Size then 1 else P + 1);
      end loop;
      return 0;
   end Object_Of;

   procedure Place_Name (C : in out Clone; Id : Object_Id; K : Positive) is
      Size : constant Positive := C.Id_Slots.Length;
      P : Positive := Home (C, Id);
   begin
      while C.Id_Slots (P) /= 0 loop
         P := (if P = Size then 1 else P + 1);
      end loop;
      C.Id_Slots.Replace (P, K);
   end Place_Name;

   --  Record that entry K is named Id (not named before): the table is kept
   --  at most half full, doubling (and placing every name again) as needed.
   procedure Insert_Name (C : in out Clone; Id : Object_Id; K : Positive) is
   begin
      if 2 * (C.Named_Count + 1) > C.Id_Slots.Length then
         declare
            New_Size : constant Positive := Positive'Max (64, 2 * C.Id_Slots.Length);
         begin
            C.Id_Slots.Clear;
            for J in 1 .. New_Size loop
               C.Id_Slots.Append (0);
            end loop;
            for J in 1 .. C.Entries.Length loop
               if J /= K and then C.Entries (J).Resolved and then Object_Of (C, C.Entries (J).Id) = 0 then
                  Place_Name (C, C.Entries (J).Id, J);
               end if;
            end loop;
         end;
      end if;
      Place_Name (C, Id, K);
      C.Named_Count := C.Named_Count + 1;
   end Insert_Name;

   type Position_Array is array (Positive range <>) of Positive;

   --  Entry positions in the order of their names.
   procedure Sort_Positions (C : Clone; Order : in out Position_Array) is
      type Keyed is record
         Id : Object_Id;
         Position : Positive;
      end record;
      type Keyed_Array is array (Positive range <>) of Keyed;
      function "<" (Left, Right : Keyed) return Boolean is (Left.Id < Right.Id);
      procedure Sort is new Heap_Sort (Keyed, Keyed_Array);
      Keys : Keyed_Array (Order'Range);
   begin
      for J in Order'Range loop
         Keys (J) := (C.Entries (Order (J)).Id, Order (J));
      end loop;
      Sort (Keys);
      for J in Order'Range loop
         Order (J) := Keys (J).Position;
      end loop;
   end Sort_Positions;

   procedure Fail (C : in out Clone; O : Outcome; Why : String) is
   begin
      if C.Result = In_Progress then
         C.Result := O;
         C.Detail := To_Text (Why);
      end if;
   end Fail;

   procedure Make_Dir (C : in out Clone; S : in out Store; Path : String) is
      Ok : Boolean;
   begin
      if C.Result /= In_Progress then
         return;
      end if;
      Make_Directory (S, Path, Ok);
      if Ok then
         C.Made.Append (Made_Record'(Made_Directory, To_Text (Path)));
      else
         Fail (C, Write_Failed, Path);
      end if;
   end Make_Dir;

   procedure Write_Whole (C : in out Clone; S : in out Store; Path : String; Data : Byte_Array) is
      Ok : Boolean;
   begin
      if C.Result /= In_Progress then
         return;
      end if;
      Create_File (S, Path, Ok);
      if not Ok then
         Fail (C, Write_Failed, Path);
         return;
      end if;
      C.Made.Append (Made_Record'(Made_File, To_Text (Path)));
      Write (S, 0, Data, Ok);
      Close_File (S);
      if not Ok then
         Fail (C, Write_Failed, Path);
      end if;
   end Write_Whole;

   procedure Write_Text (C : in out Clone; S : in out Store; Path, Text : String) is
      B : Buffer;
   begin
      Append (B, Text);
      Write_Whole (C, S, Path, B.Data (1 .. B.Length));
      Free (B.Data);
   end Write_Text;

   ---------------------------------------------------------------------------

   procedure Start (C : in out Clone; S : in out Store; Parent, Directory, Url : String; Ceiling : Limits) is
      Ok : Boolean;
   begin
      C.Parent := To_Text (Parent);
      C.Directory := To_Text (Directory);
      C.Temporary := To_Text ("." & Directory & CLONING);
      C.Url := To_Text (Url);
      C.Ceiling := Ceiling;
      if not Git_Objects.Safe_Name (Directory)
        or else Directory'Length + CLONING'Length + 1 > Git_Objects.MAXIMUM_NAME_BYTES
      then
         Fail (C, Bad_Directory, Directory);
         return;
      elsif Url'Length = 0 or else (for some Ch of Url => Ch not in '!' .. '~' or else Ch in '"' | '\') then
         Fail (C, Bad_Url, "");
         return;
      elsif Exists (S, Parent & "/" & Directory) or else Exists (S, Root (C)) then
         Fail (C, Target_Exists, Directory);
         return;
      end if;
      Make_Dir (C, S, Root (C));
      Make_Dir (C, S, Git_Dir (C));
      Make_Dir (C, S, Git_Dir (C) & "/objects");
      Make_Dir (C, S, Git_Dir (C) & "/objects/pack");
      Make_Dir (C, S, Git_Dir (C) & "/objects/info");
      Make_Dir (C, S, Git_Dir (C) & "/refs");
      Make_Dir (C, S, Git_Dir (C) & "/refs/heads");
      Make_Dir (C, S, Git_Dir (C) & "/refs/tags");
      Make_Dir (C, S, Git_Dir (C) & "/refs/remotes");
      Make_Dir (C, S, Git_Dir (C) & "/refs/remotes/origin");
      if C.Result /= In_Progress then
         return;
      end if;
      Create_File (S, Git_Dir (C) & "/objects/pack/tmp_pack", Ok);
      if not Ok then
         Fail (C, Write_Failed, Git_Dir (C) & "/objects/pack/tmp_pack");
         return;
      end if;
      C.Made.Append (Made_Record'(Made_File, To_Text (Git_Dir (C) & "/objects/pack/tmp_pack")));
      C.Pack_Open := True;
      Set_Format (C, Sha1_Format);
   end Start;

   procedure Set_Format (C : in out Clone; Format : Object_Format) is
   begin
      C.Format := Format;
      Git_Packs.Start
        (C.Reader, (Maximum_Objects => C.Ceiling.Maximum_Objects, Maximum_Object_Bytes => C.Ceiling.Maximum_Bytes,
                    Maximum_Pack_Bytes => Byte_Count'Min (C.Ceiling.Maximum_Bytes, Git_Packs.MAXIMUM_PACK_CEILING)),
         Format);
   end Set_Format;

   procedure Add_Ref
     (C : in out Clone; Name : String; Id : Object_Id; Symref : String; Has_Peeled : Boolean; Peeled : Object_Id) is
   begin
      if Name = "HEAD" then
         C.Has_Head := True;
         C.Head_Id := Id;
         C.Head_Target := To_Text (Symref);
      else
         C.Refs.Append (Ref_Record'(To_Text (Name), Id, Has_Peeled, Peeled));
      end if;
   end Add_Ref;

   procedure Pack_Data (C : in out Clone; S : in out Store; Data : Byte_Array) is
      Ok : Boolean;
      Output : Byte_Array (1 .. OUTPUT_CHUNK);
      Next : Positive := Data'First;
      Out_Next : Positive;
      E : Git_Packs.Event_Kind;
   begin
      if C.Result /= In_Progress or else Data'Length = 0 then
         return;
      end if;
      Write (S, C.Pack_Written, Data, Ok);
      if not Ok then
         Fail (C, Write_Failed, "pack");
         return;
      end if;
      C.Pack_Written := C.Pack_Written + Byte_Count (Data'Length);
      loop
         Out_Next := 1;
         Git_Packs.Step (C.Reader, Data, Next, Output, Out_Next, E);
         if Out_Next > 1 then
            C.Current (C.Filled + 1 .. C.Filled + Out_Next - 1) := Output (1 .. Out_Next - 1);
            C.Filled := C.Filled + Out_Next - 1;
         end if;
         case E is
            when Git_Packs.Entry_Start =>
               C.Held := C.Held + Git_Packs.Size (C.Reader);
               if C.Held > C.Ceiling.Maximum_Bytes - Byte_Count'Min (C.Ceiling.Maximum_Bytes, C.Pack_Written) then
                  Fail (C, Too_Large, "objects");
                  return;
               end if;
               C.Current := new Byte_Array (1 .. Natural (Git_Packs.Size (C.Reader)));
               C.Filled := 0;
            when Git_Packs.Entry_End =>
               C.Entries.Append
                 (Pack_Entry'(Kind => Git_Packs.Kind (C.Reader), Offset => Git_Packs.Entry_Offset (C.Reader),
                   Base_Offset => (if Git_Packs.Kind (C.Reader) = Git_Packs.Ofs_Delta
                                   then Git_Packs.Base_Offset (C.Reader) else 0),
                   Base_Id => Git_Packs.Base_Id (C.Reader), CRC => Git_Packs.Entry_CRC (C.Reader),
                   Data => C.Current, others => <>));
               C.Current := null;
            when Git_Packs.Refused =>
               Fail (C, (if Git_Packs.Failure (C.Reader) = Git_Packs.Too_Many_Objects then Too_Many_Objects
                         elsif Git_Packs.Failure (C.Reader) in Git_Packs.Object_Too_Large | Git_Packs.Pack_Too_Large
                         then Too_Large else Bad_Pack),
                     Git_Packs.Name (Git_Packs.Failure (C.Reader))
                     & (if Git_Packs.Failure (C.Reader) = Git_Packs.Bad_Compression
                        then " " & Inflate_Streams.Name (Git_Packs.Compression_Failure (C.Reader)) else ""));
               return;
            when Git_Packs.Pack_End =>
               if Next <= Data'Last then
                  Fail (C, Bad_Pack, "Extra_Data");
               end if;
               return;
            when Git_Packs.Need_Input =>
               return;
            when Git_Packs.Output_Full =>
               null;
         end case;
      end loop;
   end Pack_Data;

   function Name_Of (Format : Object_Format; Kind : Git_Packs.Whole_Kind; Content : Byte_Array) return Object_Id is
      H : Git_Objects.Namer;
      Id : Object_Id;
   begin
      Git_Objects.Start (H, Format, Kind, Byte_Count (Content'Length));
      Git_Objects.Update (H, Content);
      Git_Objects.Finish (H, Id);
      return Id;
   end Name_Of;

   --  As git index-pack: whole objects, then deltas whose base is known,
   --  until none is left.
   procedure Resolve (C : in out Clone) is
      procedure Named (K : Positive) is
         X : constant Pack_Entry := C.Entries (K);
      begin
         if Object_Of (C, X.Id) /= 0 then
            Fail (C, Bad_Pack, "duplicate object " & Hex (C, X.Id));
         else
            Insert_Name (C, X.Id, K);
         end if;
      end Named;
   begin
      for K in 1 .. C.Entries.Length loop
         declare
            X : Pack_Entry := C.Entries (K);
         begin
            if X.Kind in Git_Packs.Whole_Kind then
               X.Resolved := True;
               X.Whole := X.Kind;
               X.Content := X.Data;
               X.Id := Name_Of (C.Format, X.Whole, X.Content.all);
               C.Entries.Replace (K, X);
               Named (K);
            end if;
         end;
      end loop;
      while C.Result = In_Progress loop
         declare
            Progress_Made : Boolean := False;
            Left : Natural := 0;
         begin
            for K in 1 .. C.Entries.Length loop
               exit when C.Result /= In_Progress;
               declare
                  X : Pack_Entry := C.Entries (K);
                  Base_Index : Natural := 0;
               begin
                  if not X.Resolved then
                     if X.Kind = Git_Packs.Ofs_Delta then
                        Base_Index := Entry_At (C, X.Base_Offset);
                        if Base_Index = 0 then
                           Fail (C, Bad_Delta, "an OFS_DELTA base that is not an entry");
                           return;
                        end if;
                     else
                        Base_Index := Object_Of (C, X.Base_Id);
                     end if;
                     if Base_Index /= 0 and then C.Entries (Base_Index).Resolved then
                        declare
                           Base : constant Bytes_Access := C.Entries (Base_Index).Content;
                           A : Git_Deltas.Applier;
                           In_Next : Positive := X.Data'First;
                           Target : Bytes_Access;
                           R : Inflate_Streams.Progress;
                           Nothing : Byte_Array (1 .. 0);
                           Out_Next : Positive := 1;
                           Room : constant Byte_Count := C.Ceiling.Maximum_Bytes - Byte_Count'Min
                             (C.Ceiling.Maximum_Bytes, C.Held + C.Pack_Written);
                        begin
                           Git_Deltas.Start (A, Byte_Count (Base'Length), Room);
                           --  The sizes first (into no room), then the result.
                           Git_Deltas.Apply (A, Base.all, X.Data.all, In_Next, Nothing, Out_Next, R);
                           if R = Inflate_Streams.Refused or else not Git_Deltas.Sizes_Read (A) then
                              Fail (C, (if R = Inflate_Streams.Refused and then Git_Deltas.Failure (A) = Git_Deltas.Result_Too_Large
                                        then Too_Large else Bad_Delta),
                                    (if R = Inflate_Streams.Refused then Git_Deltas.Name (Git_Deltas.Failure (A)) else "Truncated"));
                              return;
                           end if;
                           Target := new Byte_Array (1 .. Natural (Git_Deltas.Result_Size (A)));
                           C.Held := C.Held + Git_Deltas.Result_Size (A);
                           Out_Next := 1;
                           Git_Deltas.Apply (A, Base.all, X.Data.all, In_Next, Target.all, Out_Next, R);
                           if R /= Inflate_Streams.Refused then
                              Git_Deltas.Finish (A, R);
                           end if;
                           if R /= Inflate_Streams.Finished then
                              Fail (C, Bad_Delta, Git_Deltas.Name (Git_Deltas.Failure (A)));
                              return;
                           end if;
                           X.Resolved := True;
                           X.Whole := C.Entries (Base_Index).Whole;
                           X.Content := Target;
                           X.Id := Name_Of (C.Format, X.Whole, Target.all);
                           C.Entries.Replace (K, X);
                           Named (K);
                           Progress_Made := True;
                        end;
                     else
                        Left := Left + 1;
                     end if;
                  end if;
               end;
            end loop;
            exit when Left = 0 or else C.Result /= In_Progress;
            if not Progress_Made then
               Fail (C, Bad_Delta, "a REF_DELTA base that is not in the pack");
               return;
            end if;
         end;
      end loop;
   end Resolve;

   --  gitformat-pack: the version 2 index.
   procedure Write_Pack_Index (C : in out Clone; S : in out Store; Path : String) is
      N : constant Natural := C.Entries.Length;
      Order : Position_Array (1 .. N);
      B : Buffer;
      Fanout : array (Byte) of U32 := [others => 0];
      Large : Natural := 0;
   begin
      --  Every entry is named once (Resolve): by name, as the index lists them.
      for J in 1 .. N loop
         Order (J) := J;
      end loop;
      Sort_Positions (C, Order);
      Append (B, Byte_Array'(16#FF#, Character'Pos ('t'), Character'Pos ('O'), Character'Pos ('c')));
      Append_32 (B, 2);
      for J of Order loop
         declare
            First : constant Byte := C.Entries (J).Id (1);
         begin
            for F in First .. Byte'Last loop
               Fanout (F) := Fanout (F) + 1;
            end loop;
         end;
      end loop;
      for F in Byte loop
         Append_32 (B, Fanout (F));
      end loop;
      for J of Order loop
         Append_Id (B, C.Format, C.Entries (J).Id);
      end loop;
      for J of Order loop
         Append_32 (B, C.Entries (J).CRC);
      end loop;
      for J of Order loop
         if C.Entries (J).Offset < LARGE_OFFSET then
            Append_32 (B, U32 (C.Entries (J).Offset));
         else
            Append_32 (B, U32 (LARGE_OFFSET) or U32 (Large));
            Large := Large + 1;
         end if;
      end loop;
      for J of Order loop
         if C.Entries (J).Offset >= LARGE_OFFSET then
            Append_32 (B, U32 (C.Entries (J).Offset / 2 ** 32));
            Append_32 (B, U32 (C.Entries (J).Offset mod 2 ** 32));
         end if;
      end loop;
      Append_Id (B, C.Format, Git_Packs.Checksum (C.Reader));
      Seal (B, C.Format);
      Write_Whole (C, S, Path, B.Data (1 .. B.Length));
      Free (B.Data);
   end Write_Pack_Index;


   --  Check out a tree below Root, recording index entries; directories
   --  depth first in tree order, which is the index's order.
   procedure Check_Out (C : in out Clone; S : in out Store; Tree_Id : Object_Id) is
      type Frame is record
         Tree : Positive;
         Pos : Positive;
         Prefix : Text;
         Has_Previous : Boolean := False;
         Previous : Git_Objects.Tree_Entry;
      end record;
      package Frame_Lists is new Git_Lists (Frame);
      Stack : Frame_Lists.List;
      Top : constant Natural := Object_Of (C, Tree_Id);
   begin
      if Top = 0 or else C.Entries (Top).Whole /= Git_Packs.Tree_Object then
         Fail (C, Missing_Object, "tree " & Hex (C, Tree_Id));
         return;
      end if;
      Stack.Append (Frame'(Tree => Top, Pos => 1, Prefix => Null_Text, others => <>));
      while not Stack.Is_Empty and then C.Result = In_Progress loop
         declare
            F : Frame := Stack.Last;
            Content : constant Bytes_Access := C.Entries (F.Tree).Content;
            E : Git_Objects.Tree_Entry;
            Why : Git_Objects.Tree_Refusal;
         begin
            if F.Pos > Content'Last then
               Stack.Delete_Last;
            else
               Git_Objects.Next_Entry (Content.all, C.Format, F.Pos, F.Has_Previous, F.Previous, E, Why);
               if Why /= Git_Objects.No_Refusal then
                  Fail (C, Bad_Object, Git_Objects.Name (Why) & " in tree " & Hex (C, C.Entries (F.Tree).Id));
                  return;
               end if;
               F.Has_Previous := True;
               F.Previous := E;
               Stack.Replace (Stack.Length, F);
               declare
                  Rel : constant String := To_String (F.Prefix) & Git_Objects.Name_Text (Content.all, E);
                  Full : constant String := Root (C) & "/" & Rel;
                  Target : constant Natural := Object_Of (C, E.Id);
               begin
                  if Full'Length > MAXIMUM_PATH_BYTES then
                     Fail (C, Path_Too_Long, Rel);
                     return;
                  end if;
                  case E.Mode is
                     when Git_Objects.Directory =>
                        if Target = 0 or else C.Entries (Target).Whole /= Git_Packs.Tree_Object then
                           Fail (C, Missing_Object, "tree " & Hex (C, E.Id) & " for " & Rel);
                           return;
                        end if;
                        Make_Dir (C, S, Full);
                        Stack.Append (Frame'(Tree => Target, Pos => 1, Prefix => To_Text (Rel & "/"),
                                       others => <>));
                     when Git_Objects.Gitlink =>
                        Make_Dir (C, S, Full);
                        C.Index.Append (Index_Record'(To_Text (Rel), MODE_GITLINK, 0, E.Id));
                     when Git_Objects.Regular_File | Git_Objects.Executable_File | Git_Objects.Symbolic_Link =>
                        if Target = 0 or else C.Entries (Target).Whole /= Git_Packs.Blob_Object then
                           Fail (C, Missing_Object, "blob " & Hex (C, E.Id) & " for " & Rel);
                           return;
                        end if;
                        Write_Whole (C, S, Full, C.Entries (Target).Content.all);
                        C.Files := C.Files + 1;
                        C.Index.Append
                          (Index_Record'(To_Text (Rel),
                            (case E.Mode is when Git_Objects.Executable_File => MODE_EXECUTABLE,
                                            when Git_Objects.Symbolic_Link => MODE_LINK,
                                            when others => MODE_REGULAR),
                            Byte_Count (C.Entries (Target).Content'Length), E.Id));
                  end case;
               end;
            end if;
         end;
      end loop;
   end Check_Out;

   --  gitformat-index, version 2, no stat data.
   procedure Write_Index (C : in out Clone; S : in out Store) is
      B : Buffer;
   begin
      Append (B, "DIRC");
      Append_32 (B, 2);
      Append_32 (B, U32 (C.Index.Length));
      for I in 1 .. C.Index.Length loop
         declare
            X : constant Index_Record := C.Index (I);
            Path : constant String := To_String (X.Path);
            --  Ten 4-byte stat fields, the name, two bytes of flags.
            Entry_Bytes : constant Natural := 40 + Id_Bytes (C.Format) + 2 + Path'Length;
            Padding : constant Positive := 8 - Entry_Bytes mod 8;
         begin
            for K in 1 .. 6 loop       --  ctime, mtime (seconds, nanoseconds), dev, ino
               Append_32 (B, 0);
            end loop;
            Append_32 (B, U32 (X.Mode));
            Append_32 (B, 0);          --  uid
            Append_32 (B, 0);          --  gid
            Append_32 (B, U32 (X.Size mod 2 ** 32));
            Append_Id (B, C.Format, X.Id);
            declare
               Flags : constant Natural := Natural'Min (Path'Length, INDEX_NAME_MASK);
            begin
               Append (B, Byte_Array'(Byte (Flags / 256), Byte (Flags mod 256)));
            end;
            Append (B, Path);
            Append (B, Byte_Array'(1 .. Padding => 0));
         end;
      end loop;
      Seal (B, C.Format);
      Write_Whole (C, S, Git_Dir (C) & "/index", B.Data (1 .. B.Length));
      Free (B.Data);
   end Write_Index;

   --  A config subsection name: '"' escaped (git-config, "Syntax").
   function Quoted (Name : String) return String is
      Out_Text : Text;
   begin
      for Ch of Name loop
         if Ch = '"' then
            Append (Out_Text, "\""");
         else
            Append (Out_Text, Ch);
         end if;
      end loop;
      return To_String (Out_Text);
   end Quoted;

   --  The config's [core] head: format version 0 for SHA-1; version 1 and
   --  extensions.objectformat for SHA-256 (gitrepository-layout, "config";
   --  git-config, "extensions.objectFormat").
   function Core_Section (C : Clone) return String is
     (case C.Format is
         when Sha1_Format => "[core]" & LF & TAB & "repositoryformatversion = 0" & LF,
         when Sha256_Format =>
            "[extensions]" & LF & TAB & "objectformat = sha256" & LF
            & "[core]" & LF & TAB & "repositoryformatversion = 1" & LF);

   function Starts (S, Prefix : String) return Boolean is
     (S'Length >= Prefix'Length and then S (S'First .. S'First + Prefix'Length - 1) = Prefix);

   procedure Release_Objects (C : in out Clone);

   procedure Finish (C : in out Clone; S : in out Store) is
      E : Git_Packs.Event_Kind;
      Ok : Boolean;
      Default : Text;      --  refs/heads/NAME, or empty when detached
      Head_Commit : Object_Id;
      Has_Commit : Boolean := False;
   begin
      if C.Pack_Open then
         Close_File (S);
         C.Pack_Open := False;
      end if;
      if C.Result = In_Progress and then C.Refs.Is_Empty and then not C.Has_Head then
         Fail (C, No_Refs, "");
      end if;
      if C.Result = In_Progress then
         Git_Packs.Input_Ended (C.Reader, E);
         if E /= Git_Packs.Pack_End then
            Fail (C, Bad_Pack, Git_Packs.Name (Git_Packs.Failure (C.Reader)));
         end if;
      end if;
      if C.Result = In_Progress then
         Resolve (C);
      end if;
      if C.Result /= In_Progress then
         Abandon (C, S);
         return;
      end if;
      --  Every ref names an object of the pack.
      for Ref_Position in 1 .. C.Refs.Length loop
         declare
            R : constant Ref_Record := C.Refs (Ref_Position);
         begin
         if Object_Of (C, R.Id) = 0 or else (R.Has_Peeled and then Object_Of (C, R.Peeled) = 0) then
            Fail (C, Missing_Object, To_String (R.Name));
         end if;
         end;
      end loop;
      --  The default branch: HEAD's target, else main or the first branch.
      if C.Has_Head and then Length (C.Head_Target) > 0 then
         Default := C.Head_Target;
      elsif not C.Has_Head then
         for Ref_Position in 1 .. C.Refs.Length loop
            declare
               R : constant Ref_Record := C.Refs (Ref_Position);
            begin
            if Starts (To_String (R.Name), HEADS)
              and then (Length (Default) = 0 or else To_String (R.Name) = "refs/heads/main")
            then
               Default := R.Name;
            end if;
            end;
         end loop;
      end if;
      if Length (Default) > 0 then
         for Ref_Position in 1 .. C.Refs.Length loop
            declare
               R : constant Ref_Record := C.Refs (Ref_Position);
            begin
            if R.Name = Default then
               Head_Commit := R.Id;
               Has_Commit := True;
            end if;
            end;
         end loop;
         if not Has_Commit or else not Starts (To_String (Default), HEADS) then
            Fail (C, Missing_Object, To_String (Default));
         end if;
      elsif C.Has_Head then
         Head_Commit := C.Head_Id;
         Has_Commit := True;
      end if;
      if C.Result = In_Progress and then Has_Commit
        and then (Object_Of (C, Head_Commit) = 0
                  or else C.Entries (Object_Of (C, Head_Commit)).Whole /= Git_Packs.Commit_Object)
      then
         Fail (C, Missing_Object, "commit " & Hex (C, Head_Commit));
      end if;
      if C.Result /= In_Progress then
         Abandon (C, S);
         return;
      end if;

      --  The pack and its index.
      declare
         Name : constant String := Git_Dir (C) & "/objects/pack/pack-" & Hex (C, Git_Packs.Checksum (C.Reader));
      begin
         Rename (S, Git_Dir (C) & "/objects/pack/tmp_pack", Name & ".pack", Ok);
         if not Ok then
            Fail (C, Write_Failed, Name & ".pack");
         else
            for M in 1 .. C.Made.Length loop
               if C.Made (M).Kind = Made_File
                 and then To_String (C.Made (M).Path) = Git_Dir (C) & "/objects/pack/tmp_pack"
               then
                  C.Made.Replace (M, (Made_File, To_Text (Name & ".pack")));
               end if;
            end loop;
            Write_Pack_Index (C, S, Name & ".idx");
         end if;
      end;

      --  Refs: HEAD, the default branch loose, the rest packed.
      declare
         Packed : Text := To_Text ("# pack-refs with: peeled fully-peeled sorted " & LF);
         type Line is record
            Name : Text;
            Content : Text;
         end record;
         type Lines is array (Positive range <>) of Line;
         function "<" (A, B : Line) return Boolean is (A.Name < B.Name);
         procedure Sort is new Heap_Sort (Line, Lines);
         All_Lines : Lines (1 .. C.Refs.Length);
         N : Natural := 0;
      begin
         if Length (Default) > 0 then
            Write_Text (C, S, Git_Dir (C) & "/HEAD", "ref: " & To_String (Default) & LF);
            declare
               Branch : constant String := To_String (Default) (HEADS'Length + 1 .. Length (Default));
               Path : constant String := Git_Dir (C) & "/" & To_String (Default);
            begin
               --  Directories for a branch named a/b.
               for K in Path'Last - Branch'Length + 1 .. Path'Last loop
                  if Path (K) = '/' then
                     Make_Dir (C, S, Path (Path'First .. K - 1));
                  end if;
               end loop;
               Write_Text (C, S, Path, Hex (C, Head_Commit) & LF);
               Write_Text (C, S, Git_Dir (C) & "/refs/remotes/origin/HEAD",
                           "ref: refs/remotes/origin/" & Branch & LF);
               Write_Text (C, S, Git_Dir (C) & "/config",
                           Core_Section (C) & TAB & "filemode = false" & LF
                           & TAB & "bare = false" & LF & TAB & "logallrefupdates = true" & LF
                           & TAB & "symlinks = false" & LF
                           & "[remote ""origin""]" & LF & TAB & "url = " & To_String (C.Url) & LF
                           & TAB & "fetch = +refs/heads/*:refs/remotes/origin/*" & LF
                           & "[branch """ & Quoted (Branch) & """]" & LF & TAB & "remote = origin" & LF
                           & TAB & "merge = " & To_String (Default) & LF);
            end;
         else
            Write_Text (C, S, Git_Dir (C) & "/HEAD", Hex (C, Head_Commit) & LF);
            Write_Text (C, S, Git_Dir (C) & "/config",
                        Core_Section (C) & TAB & "filemode = false" & LF
                        & TAB & "bare = false" & LF & TAB & "logallrefupdates = true" & LF
                        & TAB & "symlinks = false" & LF
                        & "[remote ""origin""]" & LF & TAB & "url = " & To_String (C.Url) & LF
                        & TAB & "fetch = +refs/heads/*:refs/remotes/origin/*" & LF);
         end if;
         for Ref_Position in 1 .. C.Refs.Length loop
            declare
               R : constant Ref_Record := C.Refs (Ref_Position);
            begin
            declare
               Ref_Name : constant String := To_String (R.Name);
               Stored : constant String :=
                 (if Starts (Ref_Name, HEADS)
                  then "refs/remotes/origin/" & Ref_Name (Ref_Name'First + HEADS'Length .. Ref_Name'Last)
                  else Ref_Name);
            begin
               N := N + 1;
               All_Lines (N) :=
                 (To_Text (Stored),
                  To_Text (Hex (C, R.Id) & " " & Stored & LF
                                       & (if R.Has_Peeled then "^" & Hex (C, R.Peeled) & LF else "")));
            end;
            end;
         end loop;
         Sort (All_Lines (1 .. N));
         for K in 1 .. N loop
            Append (Packed, To_String (All_Lines (K).Content));
         end loop;
         Write_Text (C, S, Git_Dir (C) & "/packed-refs", To_String (Packed));
      end;

      --  The working tree and the index.
      declare
         Tree : Object_Id;
         Found : Boolean;
      begin
         if C.Result = In_Progress then
            Git_Objects.Commit_Tree (C.Entries (Object_Of (C, Head_Commit)).Content.all, C.Format, Tree, Found);
            if not Found then
               Fail (C, Bad_Object, "commit " & Hex (C, Head_Commit));
            else
               Check_Out (C, S, Tree);
               Write_Index (C, S);
            end if;
         end if;
      end;
      if C.Result = In_Progress then
         Rename (S, Root (C), To_String (C.Parent) & "/" & To_String (C.Directory), Ok);
         if Ok then
            C.Result := Cloned;
            C.Made.Clear;
         else
            Fail (C, Write_Failed, To_String (C.Directory));
         end if;
      end if;
      if C.Result /= Cloned then
         Abandon (C, S);
      end if;
      Release_Objects (C);
   end Finish;

   --  The objects held in memory, let go (their count stays).
   procedure Release_Objects (C : in out Clone) is
   begin
      for K in 1 .. C.Entries.Length loop
         declare
            X : Pack_Entry := C.Entries (K);
         begin
            if X.Content /= null and then X.Content /= X.Data then
               Free (X.Content);
            end if;
            X.Content := null;
            Free (X.Data);
            C.Entries.Replace (K, X);
         end;
      end loop;
      Free (C.Current);
   end Release_Objects;

   procedure Abandon (C : in out Clone; S : in out Store) is
   begin
      Release_Objects (C);
      if C.Pack_Open then
         Close_File (S);
         C.Pack_Open := False;
      end if;
      for K in reverse 1 .. C.Made.Length loop
         case C.Made (K).Kind is
            when Made_File => Remove_File (S, To_String (C.Made (K).Path));
            when Made_Directory => Remove_Directory (S, To_String (C.Made (K).Path));
         end case;
      end loop;
      C.Made.Clear;
      if C.Result = In_Progress then
         C.Result := Write_Failed;
         C.Detail := To_Text ("abandoned");
      end if;
   end Abandon;
end Git_Clones;
