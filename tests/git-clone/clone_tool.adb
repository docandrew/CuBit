------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Hosted driver of Git_Clones for tests/git-clone/check.py: the three
--  answers a clone gets (capability advertisement, ls-refs, fetch), as
--  the host's git upload-pack gave them, read through the proved codecs
--  and written by Git_Clones into the Linux host's directories.
--    clone_tool ADVERT REFS FETCH PARENT DIRECTORY URL MAX_OBJECTS MAX_BYTES CHUNK
--  prints "CLONED objects files" or "OUTCOME detail".
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Inflate_Streams; use type Inflate_Streams.Byte_Count;
with Git_Packets; use type Git_Packets.Event_Kind;
with Git_Protocol; use Git_Protocol;
with Git_Clones;
with Host_Stores;

procedure Clone_Tool is
   use Ada.Streams;
   package Dirs renames Ada.Directories;
   subtype Byte_Array is Inflate_Streams.Byte_Array;
   subtype Byte_Count is Inflate_Streams.Byte_Count;

   subtype Host_Store is Host_Stores.Host_Store;
   use Host_Stores;

   package Clones is new Git_Clones
     (Host_Store, Make_Directory, Create_File, Write, Close_File, Remove_File, Remove_Directory, Rename, Exists);
   use type Clones.Outcome;

   type Bytes_Access is access Byte_Array;
   function Read_File (Path : String) return Bytes_Access is
      Size : constant Natural := Natural (Dirs.Size (Path));
      Data : constant Bytes_Access := new Byte_Array (1 .. Size);
      F : Stream_IO.File_Type;
   begin
      Stream_IO.Open (F, Stream_IO.In_File, Path);
      Byte_Array'Read (Stream_IO.Stream (F), Data.all);
      Stream_IO.Close (F);
      return Data;
   end Read_File;

   S : Host_Store;
   C : Clones.Clone;
   Chunk : constant Positive := Positive'Value (Ada.Command_Line.Argument (9));

   --  Each packet of a file, to Handle, until it says stop. False when
   --  the framing was refused.
   generic
      with procedure Handle (Kind : Git_Packets.Packet_Kind; Payload : Byte_Array; Stop : out Boolean);
   function Each_Packet (Data : Byte_Array) return Boolean;
   function Each_Packet (Data : Byte_Array) return Boolean is
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
               if E.Kind = Git_Packets.Packet then
                  Handle (E.Packet, Git_Packets.Payload (R), Stop);
               elsif E.Kind = Git_Packets.Refused then
                  return False;
               end if;
            end loop;
            Lo := Hi + 1;
         end;
      end loop;
      return Stop;
   end Each_Packet;

   A : Advertisement_Reader;
   L : Refs_Reader;
   F : Fetch_Reader;
   Problem : Boolean := False;

   procedure On_Advert (Kind : Git_Packets.Packet_Kind; Payload : Byte_Array; Stop : out Boolean) is
      R : Reply_Kind;
   begin
      Feed (A, Kind, Payload, R);
      Stop := R /= Need_More;
      Problem := R = Refused;
   end On_Advert;

   procedure On_Ref (Kind : Git_Packets.Packet_Kind; Payload : Byte_Array; Stop : out Boolean) is
      R : Ref_Reply_Kind;
   begin
      Feed (L, Kind, Payload, R);
      Stop := R /= Ref_Read;
      Problem := R = Refs_Refused;
      if R = Ref_Read then
         Clones.Add_Ref (C, Ref_Name (L), Ref_Id (L), (if Has_Symref (L) then Symref_Target (L) else ""),
                         Has_Peeled (L), (if Has_Peeled (L) then Peeled_Id (L) else NO_ID));
      end if;
   end On_Ref;

   procedure On_Fetch (Kind : Git_Packets.Packet_Kind; Payload : Byte_Array; Stop : out Boolean) is
      R : Fetch_Reply;
   begin
      Feed (F, Kind, Payload, R);
      Stop := R.Kind in Fetch_Ended | Fetch_Refused;
      Problem := R.Kind = Fetch_Refused;
      if R.Kind = Pack_Data then
         Clones.Pack_Data (C, S, Payload (R.First .. R.Last));
      end if;
   end On_Fetch;

   function Advert_Packets is new Each_Packet (On_Advert);
   function Ref_Packets is new Each_Packet (On_Ref);
   function Fetch_Packets is new Each_Packet (On_Fetch);
begin
   Clones.Start (C, S, Ada.Command_Line.Argument (4), Ada.Command_Line.Argument (5), Ada.Command_Line.Argument (6),
                 (Maximum_Objects => Natural'Value (Ada.Command_Line.Argument (7)), Maximum_Bytes => Byte_Count'Value (Ada.Command_Line.Argument (8))));
   if Clones.Result (C) /= Clones.In_Progress then
      Put_Line (Clones.Name (Clones.Result (C)) & " " & Clones.Detail (C));
      return;
   end if;
   Start (A);
   Start (F);
   if not Advert_Packets (Read_File (Ada.Command_Line.Argument (1)).all) or else Problem then
      Put_Line ("ADVERTISEMENT " & Name (Failure (A)));
      Clones.Abandon (C, S);
      return;
   end if;
   --  The object format the server advertised.
   Clones.Set_Format (C, Format (A));
   Start (L, 100_000, Format (A));
   if not Ref_Packets (Read_File (Ada.Command_Line.Argument (2)).all) or else Problem then
      Put_Line ("REFS " & Name (Failure (L)));
      Clones.Abandon (C, S);
      return;
   end if;
   --  No refs: nothing to fetch (an empty repository).
   if Git_Protocol.Count (L) > 0 and then (not Fetch_Packets (Read_File (Ada.Command_Line.Argument (3)).all) or else Problem) then
      Put_Line ("FETCH " & Name (Failure (F)) & " " & Message (F));
      Clones.Abandon (C, S);
      return;
   end if;
   Clones.Finish (C, S);
   if Clones.Result (C) = Clones.Cloned then
      Put_Line ("CLONED" & Clones.Objects (C)'Image & Clones.Files (C)'Image);
   else
      Put_Line (Clones.Name (Clones.Result (C)) & " " & Clones.Detail (C));
   end if;
end Clone_Tool;
