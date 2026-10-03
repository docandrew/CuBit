with Interfaces;
with CCL.Interfaces.Files;

--  The process's own places (docs/ccl-places.md), one body per platform:
--  native/ reads the CuBit filesystem service; the Linux preview's host/
--  reads the host's files.
package CCL_Places is
   package Files renames CCL.Interfaces.Files;

   type Listed is record
      Name : String (1 .. Files.MAXIMUM_NAME) := [others => ' '];
      Name_Length : Natural range 0 .. Files.MAXIMUM_NAME := 0;
      Kind : Files.File_Kind := Files.Other;
      Size, Modified_Ms : Interfaces.Unsigned_64 := 0;
      Mode, Links : Natural := 0;
   end record;
   subtype Listed_Count is Natural range 0 .. Files.MAXIMUM_LISTED;
   type Listing is array (1 .. Files.MAXIMUM_LISTED) of Listed;
   type Result_Kind is (Listed_All, Not_Found, Access_Denied, Unavailable, Failed);

   --  The process's workspace root ("" when it has none).
   function Home return String;
   --  The directory at Path (a place's root and path, joined by "/"): the
   --  first Count entries; Total counts all of them.
   procedure List
     (Path : String; Entries : out Listing; Count : out Listed_Count;
      Total : out Natural; Result : out Result_Kind);
end CCL_Places;
