with Interfaces; use Interfaces;
with CuBit.Directory_Pages;
with Files_Listing;

--  Directory.Page.V2 pages, as the filesystem service writes them
--  (CuBit.Directory_Pages), decoded into a listing (docs/files-app.md, "I/O
--  model"). The caller copies each page out of the shared arena first:
--  these bytes are the copy, and the service is not trusted. A page is
--  taken whole or not at all: CuBit.Directory_Pages.Check must accept it,
--  no name may hold ':' (stricter than the page format), and a page that
--  does not end the directory must move it on (entries, a new resume
--  token). Each entry's metadata comes with it.
--  Proved: no run-time errors on any bytes (tests/files-app/files_proof.gpr).
package Files_Pages with SPARK_Mode is
   package DP renames CuBit.Directory_Pages;

   PAGE_BYTES : constant := DP.Page_Bytes;
   subtype Page_Image is Files_Listing.Name_Bytes (1 .. PAGE_BYTES);

   type Page_Result is
     (Page_Taken,      --  entries appended; more pages follow
      Page_Last,       --  entries appended; the directory ends here
      Page_Malformed,  --  rejected: nothing appended
      Listing_Full);   --  no room for this page's entries: nothing appended

   --  The last page's resume token (pages must move it on).
   type Cursor_State is record
      Last : Unsigned_64 := 0;
      Started : Boolean := False;
   end record;

   procedure Take_Page
     (L : in out Files_Listing.Listing; Page : Page_Image;
      Position : in out Cursor_State; Result : out Page_Result)
     with Pre => Files_Listing.Valid (L),
          Post => Files_Listing.Valid (L) and then L.Count >= L.Count'Old
                  and then (if Result in Page_Malformed | Listing_Full then L.Count = L.Count'Old);
end Files_Pages;
