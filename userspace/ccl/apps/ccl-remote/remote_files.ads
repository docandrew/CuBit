------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The remote session's places on the filesystem service: the workspace
--  and the mailbox's two directories, all under the work root the launch
--  parameters name. Names were checked by Remote_Wire.Valid_Name (relative,
--  no "..", no scheme); the filesystem service checks this process's
--  manifest scopes on every request as well.
--
--  @description
--    <work>/remote             the workspace (uploads, build sources, outputs)
--    <work>/mail/to-device     the host's messages for the device's agent
--    <work>/mail/to-host       the device's messages for the host
--    <work>/captures           screenshots (Remote_Capture), read-only to the host
--
--  Uploads go to a staging file (the name plus Remote_Wire.Staging_Suffix)
--  chunk by chunk; Commit renames it over the name only when its size and
--  SHA-256 match what the host declared, so a reader never sees a partial
--  or corrupted file under the real name.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces;
with Remote_Wire;

package Remote_Files is
   package W renames Remote_Wire;

   --  The work root's longest accepted spelling ("@ufs:0/work").
   Maximum_Root_Bytes : constant := 64;

   procedure Initialize (Work : String; Success : out Boolean)
     with Pre => Work'Length <= Maximum_Root_Bytes;

   --  The place's directory as the filesystem names it.
   function Root (Where : W.Place) return String;

   procedure List
     (Where : W.Place; Name : String; Output : out W.Byte_Array; Length : out Natural;
      Result : out W.Status)
     with Pre => Output'First = 1;
   procedure Stat
     (Where : W.Place; Name : String; Size : out Interfaces.Unsigned_64; Hash : out W.Digest;
      Result : out W.Status);
   procedure Get
     (Where : W.Place; Name : String; Offset : W.File_Offset;
      Output : out W.Byte_Array; Length : out Natural; Result : out W.Status)
     with Pre => Output'First = 1 and then Output'Length >= W.Maximum_Chunk_Bytes;
   procedure Put
     (Where : W.Place; Name : String; Offset : W.File_Offset; Data : W.Byte_Array;
      Result : out W.Status)
     with Pre => Data'Length <= W.Maximum_Chunk_Bytes;
   procedure Commit
     (Where : W.Place; Name : String; Size : W.File_Offset; Hash : W.Digest;
      Result : out W.Status);
   procedure Remove (Where : W.Place; Name : String; Result : out W.Status);

   ---------------------------------------------------------------------------
   --  The work root's own files (Remote_Apps' generations): Name is
   --  relative to the work root, outside the host's places, and made by
   --  this process only. Replacing writes go to a staging file renamed
   --  over the name, as uploads do.
   ---------------------------------------------------------------------------
   function Work_Path (Name : String) return String;
   procedure Make_Work_Directory (Name : String; Result : out W.Status);
   procedure Hash_Work
     (Name : String; Size : out Interfaces.Unsigned_64; Hash : out W.Digest; Result : out W.Status);
   --  Copy Where's Name to the work root's Target: its size and the
   --  SHA-256 of the bytes copied.
   procedure Copy_To_Work
     (Where : W.Place; Name : String; Target : String;
      Size : out Interfaces.Unsigned_64; Hash : out W.Digest; Result : out W.Status);
   --  A small text file (at most one chunk), written whole or read whole
   --  (Text (Text'First .. Text'First + Length - 1); a longer file is
   --  Too_Large).
   procedure Write_Work_Text (Name : String; Text : String; Result : out W.Status)
     with Pre => Text'Length < W.Maximum_Chunk_Bytes;
   procedure Read_Work_Text
     (Name : String; Text : out String; Length : out Natural; Result : out W.Status)
     with Pre => Text'Length < W.Maximum_Chunk_Bytes;
end Remote_Files;
