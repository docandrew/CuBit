with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with Text_Hosts;
with Text_Limits; use Text_Limits;

--  The hosted stand-in for CuBit's filesystem queue and clipboard service
--  (tests/text-editor/README.md). Only names under a granted root open; a
--  read-only root refuses saves, as a read-only filesystem-scope does.
--  Saves write a sibling (.name.text-save), flush it and rename it over the
--  original, the same atomic replace the native host does through
--  Queue_Rename. The clipboard is in memory. This is a Linux-hosted
--  stand-in, not CuBit's services.
package Text_Host_Files is
   type Host_Files is limited private;

   --  A granted folder (names under it, after the root and a '/').
   procedure Grant (H : in out Host_Files; Root : String; Writable : Boolean; Label : String := "host");
   --  Offer the regular files under the granted roots (to Depth folders
   --  down, at most Limit) for Open and Go to Anything.
   procedure Offer_Files (H : in out Host_Files; Depth : Natural; Limit : Positive);
   procedure Offer (H : in out Host_Files; Name : String);
   --  At most Bytes per Poll_Load (simulates a slow device; 0: no limit).
   procedure Set_Read_Limit (H : in out Host_Files; Bytes : Natural);

   function Clipboard (H : Host_Files) return String;
   procedure Set_Clipboard (H : in out Host_Files; Text : String);

   procedure Start_Load (H : in out Host_Files; Name : String; Size : out Byte_Count;
                                    Status : out Text_Hosts.File_Status);
   procedure Poll_Load (H : in out Host_Files; Into : out String; Got : out Natural;
                                   Status : out Text_Hosts.File_Status);
   procedure Start_Save (H : in out Host_Files; Name : String; Status : out Text_Hosts.File_Status);
   procedure Save_Chunk (H : in out Host_Files; Bytes : String; Status : out Text_Hosts.File_Status);
   procedure Finish_Save (H : in out Host_Files; Status : out Text_Hosts.File_Status);
   procedure Poll_Save (H : in out Host_Files; Status : out Text_Hosts.File_Status);
   procedure Copy (H : in out Host_Files; Text : String);
   procedure Request_Paste (H : in out Host_Files);
   procedure Poll_Paste
     (H : in out Host_Files; Into : out String; Length : out Natural; Ready, Truncated : out Boolean);
   function Granted_Count (H : Host_Files) return Natural;
   function Granted_Name (H : Host_Files; Index : Positive) return String;
   --  "@label:0/path-within-the-root", as CuBit names a scoped place.
   function Display_Name (H : Host_Files; Name : String) return String;
   function Chunk_Limit (H : Host_Files) return Positive is (1_048_576);
private
   use Ada.Strings.Unbounded;
   MAXIMUM_ROOTS : constant := 8;
   MAXIMUM_OFFERED : constant := 8_192;
   type Root is record
      Path : Unbounded_String;
      Label : Unbounded_String;
      Writable : Boolean := False;
   end record;
   type Root_Array is array (1 .. MAXIMUM_ROOTS) of Root;
   type Name_Array is array (1 .. MAXIMUM_OFFERED) of Unbounded_String;
   type Name_Array_Access is access Name_Array;
   type Host_Files is limited record
      Roots : Root_Array;
      Root_Count : Natural := 0;
      Offered : Name_Array_Access := new Name_Array;
      Offered_Count : Natural := 0;
      Read_Limit : Natural := 0;
      Reading : Ada.Streams.Stream_IO.File_Type;
      Writing : Ada.Streams.Stream_IO.File_Type;
      Target, Temporary : Unbounded_String;
      Clip : Unbounded_String;
      Paste_Asked : Boolean := False;
   end record;
end Text_Host_Files;
