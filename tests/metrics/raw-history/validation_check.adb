with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Metric_Protocol;
with CuBit.Metric_Records;
with CuBit.Metric_Raw_Validation;
procedure Validation_Check is
 package P renames CuBit.Metric_Protocol;
 package R renames CuBit.Metric_Records;
 package V renames CuBit.Metric_Raw_Validation;
 Base, Page : P.Raw_Page := (others => (others => 0));
 Words : constant R.Slot_Words := R.Encode ((R.Span, 1, 20, 30, 7));
 Checks : Natural := 0;
 procedure Check (OK : Boolean) is
 begin Checks := Checks + 1; if not OK then raise Program_Error; end if; end;
begin
 Base (0) (0) := 1; Base (0) (1) := 77; Base (0) (2) := P.Publisher_Tag (1); Base (0) (3) := 1;
 for I in R.Slot_Word_Index loop Base (0) (8 + I) := Words (I); end loop;
 Check (V.Valid (Base, 1, 1, 2, 0));
 Check (not V.Valid (Base, 0, 1, 2, 0));
 Check (not V.Valid (Base, 1, 33, 34, 0));
 Check (not V.Valid (Base, 2, 1, 1, 0));
 Check (not V.Valid (Base, 1, 1, 2, Unsigned_64'Last));
 Check (not V.Valid (Base, 1, 1, 3, 0));
 for Field in 0 .. 7 loop
  Page := Base;
  Page (0) (Field) := (case Field is when 0 => 2, when 1 | 3 => 0,
    when 2 => P.Observer_Tag (1), when others => 1);
  if Field in 4 | 5 then Check (V.Valid (Page, 1, 1, 2, 0));
  else Check (not V.Valid (Page, 1, 1, 2, 0)); end if;
 end loop;
 for Field in R.Slot_Word_Index loop
  Page := Base; Page (0) (8 + Field) := Unsigned_64'Last;
  if Field in 0 | 1 | 2 | 5 | 6 | 7 then
   Check (not V.Valid (Page, 1, 1, 2, 0));
  end if;
 end loop;
 for Row in 1 .. 31 loop
  for W in P.Raw_Word_Index loop
   Page := Base; Page (Row) (W) := 1;
   Check (not V.Valid (Page, 1, 1, 2, 0));
  end loop;
 end loop;
 Page := Base; Page (0) (0) := Unsigned_64'Last - 1;
 Check (V.Valid (Page, Unsigned_64'Last - 1, 1, Unsigned_64'Last, 0));
 Check (V.Valid (Page, 1, 1, Unsigned_64'Last, Unsigned_64'Last - 2));
 Check (not V.Valid (Page, Unsigned_64'Last, 1, 0, 0));
 Page := (others => (others => 0));
 Check (V.Valid (Page, Unsigned_64'Last, 0, Unsigned_64'Last, 0));
 Ada.Text_IO.Put_Line ("PASS raw-reply checks:" & Checks'Image);
end Validation_Check;
