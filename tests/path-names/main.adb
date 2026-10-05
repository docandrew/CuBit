--  Hosted tests for CuBit.Path_Names: fixed cases, the C entry points, and
--  a differential check of Resolve against an independent reference (a
--  component stack) on random paths and bases.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C;
with System;
with CuBit.Path_Names; use CuBit.Path_Names;
with CuBit.Path_Names_C;
with CuBit.Directory_Paths;

procedure Main is
   Failures : Natural := 0;
   Checks   : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   function Run (Base, Path : String; Capacity : Name_Length := Maximum_Name_Bytes;
                 Status : out Resolution) return String is
      Result : CuBit.Path_Names.Name;
      Length : Name_Length;
      B : constant String (1 .. Base'Length) := Base;
      P : constant String (1 .. Path'Length) := Path;
   begin
      Resolve (B, P, Capacity, Result, Length, Status);
      return Result (1 .. Length);
   end Run;

   procedure Expect (Base, Path, Want : String) is
      Status : Resolution;
      Got : constant String := Run (Base, Path, Status => Status);
   begin
      Check (Status = Resolved and then Got = Want,
             "resolve (" & Base & ", " & Path & ") = " & Want & ", got " & Got);
   end Expect;

   procedure Expect_Status (Base, Path : String; Want : Resolution;
                            Capacity : Name_Length := Maximum_Name_Bytes) is
      Status : Resolution;
      Got : constant String := Run (Base, Path, Capacity, Status);
   begin
      Check (Status = Want and then Got'Length = 0,
             "resolve (" & Base & ", " & Path & ") is " & Want'Image);
   end Expect_Status;

   function Shown (Item : String) return String is
      Result : CuBit.Path_Names.Name;
      Length : Name_Length;
      I : constant String (1 .. Item'Length) := Item;
   begin
      Display (I, Result, Length);
      return Result (1 .. Length);
   end Shown;

   --  Independent reference: a stack of components.
   function Reference (Base, Path : String; Status : out Resolution)
     return String is
      type Parts is array (1 .. 4096) of Unbounded_String;
      Stack : Parts;
      Depth : Natural := 0;
      Volume : Unbounded_String;
      procedure Feed (Text : String) is
         Part : Unbounded_String;
         procedure Finish is
         begin
            if Length (Part) = 0 or else To_String (Part) = "." then
               null;
            elsif To_String (Part) = ".." then
               if Depth > 0 then
                  Depth := Depth - 1;
               end if;
            else
               Depth := Depth + 1;
               Stack (Depth) := Part;
            end if;
            Part := Null_Unbounded_String;
         end Finish;
      begin
         for C of Text loop
            if C = '/' then
               Finish;
            else
               Append (Part, C);
            end if;
         end loop;
         Finish;
      end Feed;
      function Split_Volume (Text : String) return String is
         Slash : Natural := 0;
      begin
         for K in Text'Range loop
            if Text (K) = '/' then
               Slash := K;
               exit;
            end if;
         end loop;
         if Slash = 0 then
            Volume := To_Unbounded_String (Text);
            return "";
         end if;
         Volume := To_Unbounded_String (Text (Text'First .. Slash - 1));
         return Text (Slash .. Text'Last);
      end Split_Volume;
      Out_Text : Unbounded_String;
   begin
      if Path'Length = 0 then
         Status := Empty_Path;
         return "";
      elsif Path (Path'First) = '@' then
         Feed (Split_Volume (Path));
      elsif Path (Path'First) = '/' then
         Volume := To_Unbounded_String (System_Volume);
         Feed (Path);
      elsif Base'Length = 0 or else Base (Base'First) /= '@' then
         Status := Invalid_Base;
         return "";
      else
         Feed (Split_Volume (Base));
         Feed (Path);
      end if;
      Out_Text := Volume;
      for K in 1 .. Depth loop
         Append (Out_Text, "/" & To_String (Stack (K)));
      end loop;
      if Depth = 0 then
         Append (Out_Text, "/");
      end if;
      --  Random paths stay far below the limit; Too_Long has fixed cases.
      Status := Resolved;
      return To_String (Out_Text);
   end Reference;

begin
   pragma Compile_Time_Error
     (Maximum_Name_Bytes /= CuBit.Directory_Paths.Maximum_Bytes,
      "CuBit.Path_Names limit must match the filesystem service's");

   ------------------------------------------------------------ fixed cases
   Expect ("@nvme:0/", "src/a.c", "@nvme:0/src/a.c");
   Expect ("@nvme:0/build", "../src/./a.c", "@nvme:0/src/a.c");
   Expect ("@nvme:0/a/b", "../../..", "@nvme:0/");
   Expect ("@nvme:0/a", "/tls/roots.der", "@nvme:0/tls/roots.der");
   Expect ("@nvme:0/a", "/../../tls//x/", "@nvme:0/tls/x");
   Expect ("", "@usb:0/x/../y", "@usb:0/y");
   Expect ("", "@usb:0", "@usb:0/");
   Expect ("", "/", "@nvme:0/");
   Expect ("@usb:0/data", "..", "@usb:0/");
   Expect ("@nvme:0/a", "...", "@nvme:0/a/...");
   Expect ("@nvme:0/a", "..x/.y", "@nvme:0/a/..x/.y");
   Expect ("@nvme:0/./b/../c/", ".", "@nvme:0/c");
   Expect_Status ("@nvme:0/", "", Empty_Path);
   Expect_Status ("relative", "x", Invalid_Base);
   Expect_Status ("", "x", Invalid_Base);
   Expect_Status ("@nvme:0/", "abc", Too_Long, Capacity => 11);
   Expect ("@nvme:0/", "ab", "@nvme:0/ab");
   Expect_Status ("@nvme:0/", "x", Too_Long, Capacity => 0);
   declare
      Long : constant String (1 .. 300) := [others => 'a'];
      Status : Resolution;
      --  A capacity smaller than the component (the limit is 4096 now).
      Got : constant String :=
        Run ("@nvme:0/", Long & "/..", Capacity => 256, Status => Status);
   begin
      Check (Status = Too_Long and then Got'Length = 0,
             "a component past the limit is too long, even if .. follows");
      Expect ("@nvme:0/", "x/" & Long (1 .. 200) & "/../y", "@nvme:0/x/y");
   end;

   Check (Shown ("@nvme:0/") = "/", "display root");
   Check (Shown ("@nvme:0/src") = "/src", "display system volume path");
   Check (Shown ("@usb:0/x") = "@usb:0/x", "display other volume");
   Check (Shown ("@nvme:0x/y") = "@nvme:0x/y", "display volume with system prefix");
   Check (Shown ("@nvme:01") = "@nvme:01", "display longer volume name");

   ------------------------------------------------------------ C entry points
   declare
      use type Interfaces.C.long;
      Base : aliased constant String := "@nvme:0/build";
      Path : aliased constant String := "../src" & Character'Val (0);
      Empty : aliased constant String := "" & Character'Val (0);
      Output : aliased String (1 .. 64) := [others => '#'];
      R : Interfaces.C.long;
   begin
      R := CuBit.Path_Names_C.Resolve
        (Base'Address, Base'Length, Path'Address, Output'Address, Output'Length);
      Check (R = 11 and then Output (1 .. 11) = "@nvme:0/src" and then Output (12) = '#',
             "C resolve writes the name and nothing more");
      R := CuBit.Path_Names_C.Resolve
        (Base'Address, Base'Length, Empty'Address, Output'Address, Output'Length);
      Check (R = -CuBit.Path_Names_C.ENOENT, "C resolve: empty path is ENOENT");
      R := CuBit.Path_Names_C.Resolve
        (Base'Address, Base'Length, System.Null_Address, Output'Address, Output'Length);
      Check (R = -CuBit.Path_Names_C.ENOENT, "C resolve: null path is ENOENT");
      R := CuBit.Path_Names_C.Resolve
        (Base'Address, Base'Length, Path'Address, Output'Address, 5);
      Check (R = -CuBit.Path_Names_C.ENAMETOOLONG, "C resolve: small capacity");
      declare
         Unterminated : aliased constant String (1 .. Maximum_Path_Bytes + 1) :=
           [others => 'a'];
         Guard : aliased constant String := "" & Character'Val (0);
         pragma Unreferenced (Guard);
      begin
         R := CuBit.Path_Names_C.Resolve
           (Base'Address, Base'Length, Unterminated'Address, Output'Address,
            Output'Length);
         Check (R = -CuBit.Path_Names_C.ENAMETOOLONG,
                "C resolve: no terminator within PATH_MAX");
      end;
      R := CuBit.Path_Names_C.Display
        (Base'Address, Base'Length, Output'Address, Output'Length);
      Check (R = 7 and then Output (1 .. 7) = "/build" & Character'Val (0),
             "C display with terminator");
      R := CuBit.Path_Names_C.Display (Base'Address, Base'Length, Output'Address, 6);
      Check (R = -CuBit.Path_Names_C.ERANGE, "C display: ERANGE");
   end;

   ------------------------------------------------- differential, random
   declare
      subtype Letter is Natural range 0 .. 4;
      package Letters is new Ada.Numerics.Discrete_Random (Letter);
      subtype Size is Natural range 0 .. 40;
      package Sizes is new Ada.Numerics.Discrete_Random (Size);
      G : Letters.Generator;
      S : Sizes.Generator;
      Alphabet : constant String := "a./.@";
      Trials : constant := 300_000;
      Agree, Resolved_Seen : Natural := 0;
      function Random_Text return String is
         Result : String (1 .. Sizes.Random (S));
      begin
         for C of Result loop
            C := Alphabet (Letters.Random (G) + 1);
         end loop;
         return Result;
      end Random_Text;
   begin
      Letters.Reset (G, 7);
      Sizes.Reset (S, 11);
      for T in 1 .. Trials loop
         declare
            Base_Tail : constant String := Random_Text;
            Base : constant String :=
              (if T mod 7 = 0 then Base_Tail else "@v" & Base_Tail);
            Path : constant String := Random_Text;
            Got_Status, Want_Status : Resolution;
            Got : constant String := Run (Base, Path, Status => Got_Status);
            Want : constant String :=
              Reference (Base, Path, Want_Status);
         begin
            if Got_Status = Want_Status and then Got = Want then
               Agree := Agree + 1;
            else
               Put_Line ("differ: base " & Base & " path " & Path & " got " &
                         Got_Status'Image & " " & Got & " want " &
                         Want_Status'Image & " " & Want);
            end if;
            if Got_Status = Resolved then
               Resolved_Seen := Resolved_Seen + 1;
            end if;
         end;
         exit when Failures > 0 or else Agree < T - 5;
      end loop;
      Check (Agree = Trials, "random paths: Resolve agrees with the reference");
      Check (Resolved_Seen > Trials / 2, "random paths: most resolve");
      Put_Line ("path-names: random trials" & Trials'Image &
                ", resolved" & Resolved_Seen'Image);
   end;

   if Failures = 0 then
      Put_Line ("path-names:" & Checks'Image & " checks PASS");
   else
      Put_Line ("path-names:" & Failures'Image & " of" & Checks'Image &
                " checks FAIL");
   end if;
end Main;
