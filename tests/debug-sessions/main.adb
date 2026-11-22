--  Hosted test of the debug-session admission and the debug-view page walk
--  (DBG-001): every Decide outcome against a reference, view and target
--  ranges at the slice and user-limit edges, and the walk over a small page
--  table store (present, user, write-back, large leaves, 1 GiB leaves).
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Debug_ABI; use Debug_ABI;
with Debug_Admission; use Debug_Admission;
with Debug_View_Walk;

procedure Main is
   Checks : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Put_Line ("debug-sessions: FAIL " & Name);
         raise Program_Error;
      end if;
      Checks := Checks + 1;
   end Check;

   ---------------------------------------------------------------------------
   --  Decide, against a reference written as the rules read.
   ---------------------------------------------------------------------------
   function Reference (F : Facts; Label, Length : Unsigned_64) return Outcome is
   begin
      if not F.Live or else not F.Same_Life or else not F.Same_Kind then
         return Revoked;
      end if;
      if Label not in 16#0D10# .. 16#0D15# then
         return Unknown_Operation;
      end if;
      declare
         Op : constant Operation := To_Operation (Label);
         Words : constant Unsigned_64 :=
           (case Op is when Describe | Detach => 0, when Threads | Unmap_View => 2,
                       when Registers | Map_View => 3);
      begin
         if Length /= Words then
            return Malformed;
         elsif Op = Detach then
            return Done;
         elsif F.Kind /= Observe and then Op /= Describe then
            return Not_Yet;
         elsif Op /= Describe and then not F.Target_Current then
            return Target_Gone;
         end if;
         return Done;
      end;
   end Reference;

   type Label_List is array (Positive range <>) of Unsigned_64;
   Labels : constant Label_List :=
     [0, 16#0D01#, 16#0D0F#, 16#0D10#, 16#0D11#, 16#0D12#, 16#0D13#, 16#0D14#,
      16#0D15#, 16#0D16#, Unsigned_64'Last];
begin
   for Live in Boolean loop
      for Same_Life in Boolean loop
         for Same_Kind in Boolean loop
            for Current in Boolean loop
               for K in Debug_Kind loop
                  for L of Labels loop
                     for Len in Unsigned_64 range 0 .. 4 loop
                        declare
                           F : constant Facts :=
                             (Live => Live, Same_Life => Same_Life, Same_Kind => Same_Kind,
                              Target_Current => Current, Kind => K);
                        begin
                           Check (Decide (F, L, Len) = Reference (F, L, Len), "Decide");
                        end;
                     end loop;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   --  The rule the slice is about: only Observe reaches the target.
   for K in Pause .. Instrument loop
      Check (Decide ((True, True, True, True, K), Label_Of (Threads), 2) = Not_Yet,
             "non-Observe kinds answer Not_Yet");
      Check (Decide ((True, True, True, True, K), Label_Of (Detach), 0) = Done,
             "every kind detaches");
   end loop;
   Check (Decide ((True, False, True, True, Observe), Label_Of (Threads), 2) = Revoked,
          "a stale generation is revoked");

   ---------------------------------------------------------------------------
   --  View and target ranges.
   ---------------------------------------------------------------------------
   for S in Session_Index range 1 .. Session_Index'Last loop
      declare
         Base : constant Unsigned_64 := Slice_Base (S);
      begin
         Check (View_Admitted (S, Base, 1), "slice start");
         Check (View_Admitted (S, Base + Slice_Bytes - Map_Pages_Ceiling * Page_Bytes,
                               Map_Pages_Ceiling), "slice end, full batch");
         Check (not View_Admitted (S, Base + Slice_Bytes - Page_Bytes, 2), "past slice end");
         Check (not View_Admitted (S, Base + Slice_Bytes, 1), "next slice");
         Check (not View_Admitted (S, Base + 1, 1), "unaligned");
         Check (not View_Admitted (S, Base, 0), "no pages");
         Check (not View_Admitted (S, Base, Map_Pages_Ceiling + 1), "too many pages");
         if S > 1 then
            Check (not View_Admitted (S, Base - Page_Bytes, 1), "previous slice");
         end if;
      end;
   end loop;
   Check (not View_Admitted (1, Aperture_First - Page_Bytes, 1), "below the aperture");
   Check (not View_Admitted (1, Unsigned_64'Last - 4095, 1), "wrapping view");
   Check (Target_Admitted (0, 1), "null page is a user page");
   Check (Target_Admitted (User_Limit - Page_Bytes, 1), "last user page");
   Check (not Target_Admitted (User_Limit - Page_Bytes, 2), "past the user limit");
   Check (not Target_Admitted (User_Limit, 1), "kernel half");
   Check (not Target_Admitted (16#FFFF_8000_0000_0000#, 1), "kernel half, high");
   Check (not Target_Admitted (4097, 1), "unaligned target");

   ---------------------------------------------------------------------------
   --  The walk, over a store of page tables.
   ---------------------------------------------------------------------------
   declare
      package W renames Debug_View_Walk;
      use type W.Verdict;
      Frames : constant := 64;
      type Table is array (W.Table_Index) of Unsigned_64;
      Store : array (1 .. Frames) of Table := [others => [others => 0]];
      Physical_Last : constant Unsigned_64 := 16#FFFF_FFFF#;

      procedure Read (Table_Frame : Unsigned_64; Index : W.Table_Index;
                      Word : out Unsigned_64) is
         N : constant Unsigned_64 := Table_Frame / W.Page_Size;
      begin
         Word := (if N in 1 .. Frames then Store (Natural (N)) (Index) else 0);
      end Read;
      function Walk is new W.Walk (Read);

      P4 : constant Unsigned_64 := 1 * 4096;
      P3 : constant Unsigned_64 := 2 * 4096;
      P2 : constant Unsigned_64 := 3 * 4096;
      P1 : constant Unsigned_64 := 4 * 4096;
      Data : constant Unsigned_64 := 16#0100_0000#;
      Big  : constant Unsigned_64 := 16#0400_0000#;   --  2 MiB aligned
      User : constant Unsigned_64 := W.Present_Bit or W.User_Bit or 2;
      --  Address 0x1000 (P4 0, P3 0, P2 0, P1 1); 0x20_0000 + 0x3000 (P2 1, large).
      R : W.Result;
   begin
      Store (1) (0) := P3 or User;
      Store (2) (0) := P2 or User;
      Store (3) (0) := P1 or User;
      Store (4) (1) := Data or User;
      R := Walk (P4, 16#1000#, Physical_Last);
      Check (R.Kind = W.Viewable and then R.Frame = Data, "4 KiB leaf");
      Store (4) (2) := Data or W.Present_Bit;   --  not user
      R := Walk (P4, 16#2000#, Physical_Last);
      Check (R.Kind = W.Unmapped and then R.Frame = 0, "supervisor-only page");
      Store (4) (3) := Data or User or W.Cache_Disable_Bit;
      R := Walk (P4, 16#3000#, Physical_Last);
      Check (R.Kind = W.Not_Write_Back, "uncached leaf");
      Store (4) (4) := Data or User or W.PAT_Small_Bit;
      R := Walk (P4, 16#4000#, Physical_Last);
      Check (R.Kind = W.Not_Write_Back, "PAT leaf (write-combining)");
      Store (4) (5) := Data or User or W.Write_Through_Bit;
      R := Walk (P4, 16#5000#, Physical_Last);
      Check (R.Kind = W.Not_Write_Back, "write-through leaf");
      R := Walk (P4, 16#6000#, Physical_Last);
      Check (R.Kind = W.Unmapped, "absent leaf");
      Store (3) (1) := Big or User or W.Large_Page_Bit;
      R := Walk (P4, 16#20_3000#, Physical_Last);
      Check (R.Kind = W.Viewable and then R.Frame = Big + 16#3000#, "frame inside a 2 MiB leaf");
      Store (3) (2) := Big or User or W.Large_Page_Bit or W.PAT_Large_Bit;
      R := Walk (P4, 16#40_0000#, Physical_Last);
      Check (R.Kind = W.Not_Write_Back, "PAT in a 2 MiB leaf");
      Store (2) (1) := Big or User or W.Large_Page_Bit;
      R := Walk (P4, 16#4000_0000#, Physical_Last);
      Check (R.Kind = W.Unmapped, "1 GiB leaf refused");
      R := Walk (P4, 16#0000_8000_0000_0000#, Physical_Last);
      Check (R.Kind = W.Unmapped, "kernel half");
      R := Walk (P4, 16#1000#, 16#00FF_FFFF#);
      Check (R.Kind = W.Unmapped, "frame beyond physical memory");
   end;

   Put_Line ("debug-sessions: checks" & Checks'Image);
   Put_Line ("debug-sessions: PASS");
end Main;
