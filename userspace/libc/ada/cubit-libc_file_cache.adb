------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Libc_File_Cache with SPARK_Mode is

   Golden : constant Unsigned_64 := 16#9E37_79B9_7F4A_7C15#;
   Mixer  : constant Unsigned_64 := 16#C2B2_AE3D_27D4_EB4F#;
   Hash_Shift : constant := 40;

   function Page_Bucket_Of (File : File_Slot; Page : Unsigned_64) return Page_Bucket is
     (Page_Bucket (Shift_Right (Unsigned_64 (File) * Golden xor Page * Mixer, Hash_Shift)
                   and (Page_Buckets - 1)));

   function File_Bucket_Of (Inode : Unsigned_64) return File_Bucket is
     (File_Bucket (Shift_Right (Inode * Golden, Hash_Shift) and (File_Buckets - 1)));

   procedure Find (C : in out Cache; File : File_Slot; Page : Unsigned_64;
                   Found : out Page_Link)
   is
      Link : Page_Link := C.Page_Chains (Page_Bucket_Of (File, Page));
   begin
      Found := No_Link;
      for Steps in 1 .. Maximum_Pages loop
         exit when Link = No_Link;
         if C.Pages (Link - 1).File = File + 1 and then C.Pages (Link - 1).Page = Page then
            if Is_Current (C, Link - 1) then
               C.Pages (Link - 1).Referenced := True;
               Found := Link;
            end if;
            return;
         end if;
         Link := C.Pages (Link - 1).Next;
      end loop;
   end Find;

   --  Take slot S out of its hash chain and free it.
   procedure Unlink (C : in out Cache; S : Page_Slot)
   with Post => C.Pages (S).File = No_Link
                and then C.Pages_Backed = C.Pages_Backed'Old
                and then C.Files = C.Files'Old;

   procedure Unlink (C : in out Cache; S : Page_Slot) is
      Bucket : constant Page_Bucket :=
        (if C.Pages (S).File = No_Link then 0
         else Page_Bucket_Of (C.Pages (S).File - 1, C.Pages (S).Page));
      Link : Page_Link := C.Page_Chains (Bucket);
   begin
      if C.Pages (S).File /= No_Link then
         if Link = S + 1 then
            C.Page_Chains (Bucket) := C.Pages (S).Next;
         else
            for Steps in 1 .. Maximum_Pages loop
               exit when Link = No_Link;
               if C.Pages (Link - 1).Next = S + 1 then
                  C.Pages (Link - 1).Next := C.Pages (S).Next;
                  exit;
               end if;
               Link := C.Pages (Link - 1).Next;
               pragma Loop_Invariant (C.Files = C.Files'Loop_Entry
                                      and then C.Pages_Backed = C.Pages_Backed'Loop_Entry);
            end loop;
         end if;
      end if;
      C.Pages (S).File := No_Link;
   end Unlink;

   procedure File_For
     (C : in out Cache; Inode, Version : Unsigned_64; In_Use : File_Uses;
      File : out File_Slot; Found : out Boolean)
   is
      Bucket : constant File_Bucket := File_Bucket_Of (Inode);
      Link : File_Link := C.File_Chains (Bucket);
   begin
      File := 0;
      Found := False;
      for Steps in 1 .. Cached_Files loop
         exit when Link = No_Link;
         if C.Files (Link - 1).Inode = Inode then
            File := Link - 1;
            if C.Files (File).Version /= Version then
               C.Files (File).Epoch := C.Files (File).Epoch + 1;
               C.Files (File).Version := Version;
               C.Stale_Possible := True;
            end if;
            Found := True;
            return;
         end if;
         Link := C.Files (Link - 1).Next;
      end loop;

      if C.File_Used < Cached_Files then
         File := C.File_Used;
         C.File_Used := C.File_Used + 1;
      else
         --  Reuse an entry no handle uses: its pages go stale.
         for Steps in 1 .. Cached_Files loop
            if not In_Use (C.File_Clock) then
               File := C.File_Clock;
               Found := True;
            end if;
            C.File_Clock := (C.File_Clock + 1) mod Cached_Files;
            exit when Found;
         end loop;
         if not Found then
            return;
         end if;
         --  Out of its old chain.
         declare
            Old_Bucket : constant File_Bucket := File_Bucket_Of (C.Files (File).Inode);
            Walk : File_Link := C.File_Chains (Old_Bucket);
         begin
            if Walk = File + 1 then
               C.File_Chains (Old_Bucket) := C.Files (File).Next;
            else
               for Steps in 1 .. Cached_Files loop
                  exit when Walk = No_Link;
                  if C.Files (Walk - 1).Next = File + 1 then
                     C.Files (Walk - 1).Next := C.Files (File).Next;
                     exit;
                  end if;
                  Walk := C.Files (Walk - 1).Next;
               end loop;
            end if;
         end;
      end if;
      C.Files (File).Inode := Inode;
      C.Files (File).Version := Version;
      C.Files (File).Epoch := C.Files (File).Epoch + 1;
      C.Files (File).Next := C.File_Chains (Bucket);
      C.File_Chains (Bucket) := File + 1;
      C.Stale_Possible := True;
      Found := True;
   end File_For;

   --  A stale or free backed slot found within a short probe, or No_Link.
   procedure Take_Stale (C : in out Cache; Slot : out Page_Link)
   with Post => Slot <= C.Pages_Backed and then C.Pages_Backed = C.Pages_Backed'Old
                and then C.Files = C.Files'Old
                and then (if Slot /= No_Link then C.Pages (Slot - 1).File = No_Link);

   procedure Take_Stale (C : in out Cache; Slot : out Page_Link) is
      S : Page_Slot;
   begin
      Slot := No_Link;
      if not C.Stale_Possible or else C.Pages_Backed = 0 then
         return;
      end if;
      for N in 1 .. Stale_Probe_Pages loop
         S := (if C.Page_Clock < C.Pages_Backed then C.Page_Clock else 0);
         C.Page_Clock := (S + 1) mod C.Pages_Backed;
         if not Is_Current (C, S) then
            Unlink (C, S);
            Slot := S + 1;
            return;
         end if;
         pragma Loop_Invariant (C.Pages_Backed = C.Pages_Backed'Loop_Entry
                                and then C.Pages_Backed > 0
                                and then C.Files = C.Files'Loop_Entry);
      end loop;
      C.Stale_Possible := False;
   end Take_Stale;

   procedure New_Page
     (C : in out Cache; File : File_Slot; Page : Unsigned_64; Can_Grow : Boolean;
      Slot : out Page_Link)
   is
      S : Page_Slot;
      Bucket : constant Page_Bucket := Page_Bucket_Of (File, Page);
   begin
      Take_Stale (C, Slot);
      if Slot = No_Link and then Can_Grow and then C.Pages_Backed < Maximum_Pages then
         C.Pages_Backed := C.Pages_Backed + 1;
         Slot := C.Pages_Backed;
         C.Pages (Slot - 1).File := No_Link;
      end if;
      if Slot = No_Link then
         if C.Pages_Backed = 0 then
            return;
         end if;
         --  The clock: a stale page, or one not referenced since its last
         --  pass (two passes at most).
         S := (if C.Page_Clock < C.Pages_Backed then C.Page_Clock else 0);
         for Steps in 1 .. 2 * Maximum_Pages loop
            exit when not Is_Current (C, S) or else not C.Pages (S).Referenced;
            C.Pages (S).Referenced := False;
            S := (S + 1) mod C.Pages_Backed;
            pragma Loop_Invariant (S < C.Pages_Backed
                                   and then C.Pages_Backed = C.Pages_Backed'Loop_Entry
                                   and then C.Files = C.Files'Loop_Entry);
         end loop;
         C.Page_Clock := (S + 1) mod C.Pages_Backed;
         Unlink (C, S);
         Slot := S + 1;
      end if;
      C.Pages (Slot - 1) :=
        (File => File + 1, Epoch => C.Files (File).Epoch, Page => Page,
         Next => C.Page_Chains (Bucket), Referenced => True);
      C.Page_Chains (Bucket) := Slot;
   end New_Page;

   procedure Set_Version (C : in out Cache; File : File_Slot; Version : Unsigned_64) is
   begin
      C.Files (File).Version := Version;
   end Set_Version;

end CuBit.Libc_File_Cache;
