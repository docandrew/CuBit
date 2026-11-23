------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Hosted driver of the git codecs (userspace/lib/git) for
--  tests/git-codecs/check.py, which compares what it prints with what the
--  host's git says about the same bytes.
--    git_tool advert FILE CHUNK          capability advertisement
--    git_tool refs FILE CHUNK            ls-refs response
--    git_tool fetch FILE CHUNK OUT       fetch response; pack data to OUT
--    git_tool pack FILE CHUNK OUTCHUNK   index a pack: every object's name
--    git_tool delta BASE DELTA OUT       apply a delta
--    git_tool tree FILE                  a tree's entries
--  FILE is read CHUNK bytes at a time. Built with -gnata, so the units'
--  contracts are checked on every call too.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Command_Line;
with Ada.Environment_Variables; use Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;
with Inflate_Streams; use type Inflate_Streams.Byte_Count, Inflate_Streams.Progress;
with Git_Packets;
with Git_Protocol; use Git_Protocol;
with Git_Packs; use type Git_Packs.Event_Kind, Git_Packs.Object_Kind, Git_Packs.Refusal;
with Git_Deltas;
with Git_Objects; use type Git_Objects.Tree_Refusal;

procedure Git_Tool is
   --  GIT_FORMAT=sha256: the repository's object format (SHA-1 otherwise).
   Fmt : constant Object_Format :=
     (if Ada.Environment_Variables.Exists ("GIT_FORMAT")
        and then Ada.Environment_Variables.Value ("GIT_FORMAT") = "sha256"
      then Sha256_Format else Sha1_Format);
   package GP renames Git_Packs;
   subtype Byte_Array is Inflate_Streams.Byte_Array;
   subtype Byte_Count is Inflate_Streams.Byte_Count;
   use type Git_Packets.Event_Kind;
   type Bytes_Access is access Byte_Array;
   procedure Free is new Ada.Unchecked_Deallocation (Byte_Array, Bytes_Access);

   function Read_File (Path : String) return Bytes_Access is
      Size : constant Natural := Natural (Ada.Directories.Size (Path));
      Data : constant Bytes_Access := new Byte_Array (1 .. Size);
      F : Ada.Streams.Stream_IO.File_Type;
   begin
      Ada.Streams.Stream_IO.Open (F, Ada.Streams.Stream_IO.In_File, Path);
      Byte_Array'Read (Ada.Streams.Stream_IO.Stream (F), Data.all);
      Ada.Streams.Stream_IO.Close (F);
      return Data;
   end Read_File;

   procedure Write_File (Path : String; Data : Byte_Array) is
      F : Ada.Streams.Stream_IO.File_Type;
   begin
      Ada.Streams.Stream_IO.Create (F, Ada.Streams.Stream_IO.Out_File, Path);
      Byte_Array'Write (Ada.Streams.Stream_IO.Stream (F), Data);
      Ada.Streams.Stream_IO.Close (F);
   end Write_File;

   function Img (N : Long_Long_Integer) return String is (Ada.Strings.Fixed.Trim (N'Image, Ada.Strings.Left));
   function Img (N : Byte_Count) return String is (Img (Long_Long_Integer (N)));

   function Hex32 (V : Unsigned_32) return String is
      D : constant String := "0123456789abcdef";
      S : String (1 .. 8);
      X : Unsigned_32 := V;
   begin
      for K in reverse S'Range loop
         S (K) := D (Natural (X and 15) + 1);
         X := Shift_Right (X, 4);
      end loop;
      return S;
   end Hex32;

   --  Feed FILE's packets, CHUNK bytes at a time, to Handle; stop when it
   --  says so. Returns False if the framing was refused (printed).
   generic
      with procedure Handle (Kind : Git_Packets.Packet_Kind; Payload : Byte_Array; Stop : out Boolean);
   procedure Each_Packet (Data : Byte_Array; Chunk : Positive);

   procedure Each_Packet (Data : Byte_Array; Chunk : Positive) is
      R : Git_Packets.Reader;
      E : Git_Packets.Event;
      Lo : Positive := Data'First;
      Stop : Boolean := False;
   begin
      Git_Packets.Start (R);
      while Lo <= Data'Last and then not Stop loop
         declare
            Hi : constant Natural := Natural'Min (Lo + Chunk - 1, Data'Last);
            Next : Positive := Lo;
         begin
            while Next <= Hi and then not Stop loop
               Git_Packets.Step (R, Data (Lo .. Hi), Next, E);
               case E.Kind is
                  when Git_Packets.Packet =>
                     Handle (E.Packet, Git_Packets.Payload (R), Stop);
                  when Git_Packets.Refused =>
                     Put_Line ("REFUSED PACKET " & Git_Packets.Name (E.Why));
                     Stop := True;
                  when Git_Packets.Need_More =>
                     null;
               end case;
            end loop;
            Lo := Hi + 1;
         end;
      end loop;
      if not Stop then
         Git_Packets.Input_Ended (R, E);
         if E.Kind = Git_Packets.Refused then
            Put_Line ("REFUSED PACKET " & Git_Packets.Name (E.Why));
         else
            Put_Line ("REFUSED INPUT_ENDED");
         end if;
      end if;
   end Each_Packet;

   procedure Advert (Data : Byte_Array; Chunk : Positive) is
      A : Advertisement_Reader;
      procedure Handle (Kind : Git_Packets.Packet_Kind; Payload : Byte_Array; Stop : out Boolean) is
         R : Reply_Kind;
      begin
         Feed (A, Kind, Payload, R);
         Stop := R /= Need_More;
         if R = Ended then
            Put_Line ("ENDED agent=" & Offers_Agent (A)'Image & " object-format=" & Offers_Object_Format (A)'Image
                      & " format=" & Format_Name (Format (A)));
         elsif R = Refused then
            Put_Line ("REFUSED " & Git_Protocol.Name (Git_Protocol.Failure (A)));
         end if;
      end Handle;
      procedure Run is new Each_Packet (Handle);
   begin
      Start (A);
      Run (Data, Chunk);
   end Advert;

   procedure Refs (Data : Byte_Array; Chunk : Positive) is
      L : Refs_Reader;
      procedure Handle (Kind : Git_Packets.Packet_Kind; Payload : Byte_Array; Stop : out Boolean) is
         R : Ref_Reply_Kind;
      begin
         Feed (L, Kind, Payload, R);
         Stop := R /= Ref_Read;
         case R is
            when Ref_Read =>
               Put_Line ("REF " & To_Hex (Ref_Id (L), Fmt) & " " & Ref_Name (L)
                         & (if Has_Symref (L) then " symref " & Symref_Target (L) else "")
                         & (if Has_Peeled (L) then " peeled " & To_Hex (Peeled_Id (L), Fmt) else ""));
            when Refs_Ended =>
               Put_Line ("ENDED" & Git_Protocol.Count (L)'Image);
            when Refs_Refused =>
               Put_Line ("REFUSED " & Git_Protocol.Name (Git_Protocol.Failure (L)));
         end case;
      end Handle;
      procedure Run is new Each_Packet (Handle);
   begin
      Start (L, 1000, Fmt);
      Run (Data, Chunk);
   end Refs;

   procedure Fetch (Data : Byte_Array; Chunk : Positive; Out_Path : String) is
      F : Fetch_Reader;
      Pack : Ada.Streams.Stream_IO.File_Type;
      Bytes : Long_Long_Integer := 0;
      procedure Handle (Kind : Git_Packets.Packet_Kind; Payload : Byte_Array; Stop : out Boolean) is
         R : Fetch_Reply;
      begin
         Feed (F, Kind, Payload, R);
         Stop := R.Kind in Fetch_Ended | Fetch_Refused;
         case R.Kind is
            when Pack_Data =>
               Byte_Array'Write (Ada.Streams.Stream_IO.Stream (Pack), Payload (R.First .. R.Last));
               Bytes := Bytes + Long_Long_Integer (R.Last - R.First + 1);
            when Git_Protocol.Progress | Nothing =>
               null;
            when Fetch_Ended =>
               Put_Line ("ENDED " & Img (Bytes));
            when Fetch_Refused =>
               Put_Line ("REFUSED " & Git_Protocol.Name (Git_Protocol.Failure (F)) & " " & Message (F));
         end case;
      end Handle;
      procedure Run is new Each_Packet (Handle);
   begin
      Ada.Streams.Stream_IO.Create (Pack, Ada.Streams.Stream_IO.Out_File, Out_Path);
      Start (F);
      Run (Data, Chunk);
      Ada.Streams.Stream_IO.Close (Pack);
   end Fetch;

   ---------------------------------------------------------------------------
   --  Packs: read every entry, then resolve the deltas, as git index-pack
   --  does, keeping everything in memory.

   type Pack_Entry is record
      Kind : GP.Object_Kind;
      Offset : GP.Pack_Offset;
      Size : Byte_Count;
      Base_Offset : GP.Pack_Offset := 0;
      Base_Id : Object_Id;
      CRC : Unsigned_32 := 0;
      Data : Bytes_Access;
      --  Once resolved: the whole object's kind, bytes and name.
      Resolved : Boolean := False;
      Whole : GP.Whole_Kind := GP.Blob_Object;
      Content : Bytes_Access;
      Id : Object_Id := [others => 0];
   end record;
   package Entry_Vectors is new Ada.Containers.Vectors (Positive, Pack_Entry);
   package Offset_Maps is new Ada.Containers.Ordered_Maps (GP.Pack_Offset, Positive);
   package Id_Maps is new Ada.Containers.Ordered_Maps (Object_Id, Positive);

   function Name_Of (Kind : GP.Whole_Kind; Content : Byte_Array) return Object_Id is
      H : Git_Objects.Namer;
      Id : Object_Id;
   begin
      Git_Objects.Start (H, Fmt, Kind, Byte_Count (Content'Length));
      Git_Objects.Update (H, Content);
      Git_Objects.Finish (H, Id);
      return Id;
   end Name_Of;

   procedure Index_Pack (Data : Byte_Array; Chunk, Out_Chunk : Positive) is
      P : GP.Pack_Reader;
      E : GP.Event_Kind;
      Entries : Entry_Vectors.Vector;
      By_Offset : Offset_Maps.Map;
      By_Id : Id_Maps.Map;
      Buffer : Byte_Array (1 .. Out_Chunk) := [others => 0];
      Current : Bytes_Access := new Byte_Array (1 .. 0);
      Filled : Natural := 0;
      Lo : Positive := Data'First;
      Finished_Pack : Boolean := False;
   begin
      GP.Start (P, (Maximum_Objects => 1_000_000, Maximum_Object_Bytes => 2 ** 30,
                 Maximum_Pack_Bytes => 2 ** 40), Fmt);
      Outer :
      loop
         if Lo > Data'Last then
            GP.Input_Ended (P, E);
            exit Outer when E = GP.Pack_End;
            Put_Line ("REFUSED " & GP.Name (GP.Failure (P)));
            return;
         end if;
         declare
            Hi : constant Natural := Natural'Min (Lo + Chunk - 1, Data'Last);
            Next : Positive := Lo;
            Out_Next : Positive;
         begin
            loop
               Out_Next := 1;
               GP.Step (P, Data (Lo .. Hi), Next, Buffer, Out_Next, E);
               if Out_Next > 1 then
                  Current (Filled + 1 .. Filled + Out_Next - 1) := Buffer (1 .. Out_Next - 1);
                  Filled := Filled + Out_Next - 1;
               end if;
               case E is
                  when GP.Entry_Start =>
                     Current := new Byte_Array (1 .. Natural (GP.Size (P)));
                     Filled := 0;
                  when GP.Entry_End =>
                     Entries.Append
                       (Pack_Entry'(Kind => GP.Kind (P), Offset => GP.Entry_Offset (P), Size => GP.Size (P),
                         Base_Offset => (if GP.Kind (P) = GP.Ofs_Delta then GP.Base_Offset (P) else 0),
                         Base_Id => GP.Base_Id (P), CRC => GP.Entry_CRC (P), Data => Current, others => <>));
                     By_Offset.Insert (GP.Entry_Offset (P), Entries.Last_Index);
                  when GP.Pack_End =>
                     Finished_Pack := True;
                     if Next <= Hi or else Hi < Data'Last then
                        Put_Line ("REFUSED EXTRA_DATA");
                        return;
                     end if;
                  when GP.Refused =>
                     Put_Line ("REFUSED " & GP.Name (GP.Failure (P))
                               & (if GP.Failure (P) = GP.Bad_Compression
                                  then " " & Inflate_Streams.Name (GP.Compression_Failure (P)) else ""));
                     return;
                  when GP.Need_Input | GP.Output_Full =>
                     null;
               end case;
               exit when E = GP.Need_Input or else Finished_Pack;
            end loop;
            exit Outer when Finished_Pack;
            Lo := Hi + 1;
         end;
      end loop Outer;
      --  Whole objects first, then deltas whose base is known, until none
      --  is left.
      for K in Entries.First_Index .. Entries.Last_Index loop
         declare
            X : Pack_Entry := Entries (K);
         begin
            if X.Kind in GP.Whole_Kind then
               X.Resolved := True;
               X.Whole := X.Kind;
               X.Content := X.Data;
               X.Id := Name_Of (X.Whole, X.Content.all);
               Entries.Replace_Element (K, X);
               if not By_Id.Contains (X.Id) then
                  By_Id.Insert (X.Id, K);
               end if;
            end if;
         end;
      end loop;
      loop
         declare
            Progress_Made : Boolean := False;
            Left : Natural := 0;
         begin
            for K in Entries.First_Index .. Entries.Last_Index loop
               declare
                  X : Pack_Entry := Entries (K);
                  Base_Index : Natural := 0;
               begin
                  if not X.Resolved then
                     if X.Kind = GP.Ofs_Delta then
                        if not By_Offset.Contains (X.Base_Offset) then
                           Put_Line ("REFUSED BASE_NOT_AN_ENTRY");
                           return;
                        end if;
                        Base_Index := By_Offset (X.Base_Offset);
                     elsif By_Id.Contains (X.Base_Id) then
                        Base_Index := By_Id (X.Base_Id);
                     end if;
                     if Base_Index /= 0 and then Entries (Base_Index).Resolved then
                        declare
                           Base : constant Bytes_Access := Entries (Base_Index).Content;
                           A : Git_Deltas.Applier;
                           In_Next : Positive := X.Data'First;
                           Target : Bytes_Access;
                           R : Inflate_Streams.Progress;
                           Scratch : Byte_Array (1 .. 0);
                           Out_Next : Positive := 1;
                        begin
                           --  Read the declared size first (into no room), then apply.
                           Git_Deltas.Start (A, Byte_Count (Base'Length), 2 ** 30);
                           Git_Deltas.Apply (A, Base.all, X.Data.all, In_Next, Scratch, Out_Next, R);
                           if R = Inflate_Streams.Refused or else not Git_Deltas.Sizes_Read (A) then
                              Put_Line ("REFUSED DELTA "
                                        & (if R = Inflate_Streams.Refused then Git_Deltas.Name (Git_Deltas.Failure (A))
                                           else "NO_SIZES"));
                              return;
                           end if;
                           Target := new Byte_Array (1 .. Natural (Git_Deltas.Result_Size (A)));
                           Out_Next := 1;
                           Git_Deltas.Apply (A, Base.all, X.Data.all, In_Next, Target.all, Out_Next, R);
                           if R /= Inflate_Streams.Refused then
                              Git_Deltas.Finish (A, R);
                           end if;
                           if R /= Inflate_Streams.Finished then
                              Put_Line ("REFUSED DELTA " & Git_Deltas.Name (Git_Deltas.Failure (A)));
                              return;
                           end if;
                           X.Resolved := True;
                           X.Whole := Entries (Base_Index).Whole;
                           X.Content := Target;
                           X.Id := Name_Of (X.Whole, Target.all);
                           Entries.Replace_Element (K, X);
                           if not By_Id.Contains (X.Id) then
                              By_Id.Insert (X.Id, K);
                           end if;
                           Progress_Made := True;
                        end;
                     else
                        Left := Left + 1;
                     end if;
                  end if;
               end;
            end loop;
            exit when Left = 0;
            if not Progress_Made then
               Put_Line ("REFUSED UNRESOLVED_DELTAS" & Left'Image);
               return;
            end if;
         end;
      end loop;
      for X of Entries loop
         Put_Line (To_Hex (X.Id, Fmt) & " " & Git_Objects.Type_Name (X.Whole) & " " & Img (Byte_Count (X.Content'Length))
                   & " " & Img (X.Size) & " " & Img (X.Offset) & " " & Hex32 (X.CRC));
      end loop;
      Put_Line ("PACK " & To_Hex (GP.Checksum (P), Fmt) & " " & Img (Long_Long_Integer (Entries.Length)));
   end Index_Pack;

   procedure Apply_Delta (Base_Path, Delta_Path, Out_Path : String) is
      Base : Bytes_Access := Read_File (Base_Path);
      D : Bytes_Access := Read_File (Delta_Path);
      A : Git_Deltas.Applier;
      Buffer : Byte_Array (1 .. 7) := [others => 0];
      Result : Byte_Array (1 .. 1_000_000);
      Filled : Natural := 0;
      In_Next : Positive := D'First;
      Out_Next : Positive;
      R : Inflate_Streams.Progress;
   begin
      Git_Deltas.Start (A, Byte_Count (Base'Length), Result'Length);
      loop
         Out_Next := 1;
         Git_Deltas.Apply (A, Base.all, D.all, In_Next, Buffer, Out_Next, R);
         Result (Filled + 1 .. Filled + Out_Next - 1) := Buffer (1 .. Out_Next - 1);
         Filled := Filled + Out_Next - 1;
         exit when R /= Inflate_Streams.Output_Full;
      end loop;
      if R /= Inflate_Streams.Refused then
         Git_Deltas.Finish (A, R);
      end if;
      if R = Inflate_Streams.Finished then
         Write_File (Out_Path, Result (1 .. Filled));
         Put_Line ("FINISHED " & Img (Long_Long_Integer (Filled)));
      else
         Put_Line ("REFUSED " & Git_Deltas.Name (Git_Deltas.Failure (A)));
      end if;
      Free (Base);
      Free (D);
   end Apply_Delta;

   procedure Tree (Data : Byte_Array) is
      Pos : Positive := Data'First;
      Prev : Git_Objects.Tree_Entry;
      Has_Prev : Boolean := False;
      E : Git_Objects.Tree_Entry;
      Why : Git_Objects.Tree_Refusal;
   begin
      while Pos <= Data'Last loop
         Git_Objects.Next_Entry (Data, Fmt, Pos, Has_Prev, Prev, E, Why);
         if Why /= Git_Objects.No_Refusal then
            Put_Line ("REFUSED " & Git_Objects.Name (Why));
            return;
         end if;
         Put_Line (E.Mode'Image & " " & Git_Objects.Name_Text (Data, E) & " " & To_Hex (E.Id, Fmt));
         Prev := E;
         Has_Prev := True;
      end loop;
      Put_Line ("ENDED");
   end Tree;

   function Arg (K : Positive) return String is (Argument (K));
begin
   declare
      Mode : constant String := Arg (1);
   begin
      if Mode = "delta" then
         Apply_Delta (Arg (2), Arg (3), Arg (4));
         return;
      end if;
      declare
         Data : Bytes_Access := Read_File (Arg (2));
      begin
         if Mode = "advert" then
            Advert (Data.all, Positive'Value (Arg (3)));
         elsif Mode = "refs" then
            Refs (Data.all, Positive'Value (Arg (3)));
         elsif Mode = "fetch" then
            Fetch (Data.all, Positive'Value (Arg (3)), Arg (4));
         elsif Mode = "pack" then
            Index_Pack (Data.all, Positive'Value (Arg (3)), Positive'Value (Arg (4)));
         elsif Mode = "tree" then
            Tree (Data.all);
         else
            Put_Line ("usage: git_tool advert|refs|fetch|pack|delta|tree ...");
         end if;
         Free (Data);
      end;
   end;
end Git_Tool;
