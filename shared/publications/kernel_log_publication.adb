------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body Kernel_Log_Publication with SPARK_Mode is

   function Make_Line
     (Time_Ms   : Unsigned_64;
      Text      : String;
      Truncated : Boolean) return Log_Record
   is
      Kept   : constant Natural := Natural'Min (Text'Length, Text_Bytes);
      Result : Log_Record :=
        (Sequence => Writing,
         Time_Ms  => Time_Ms,
         Source   => Kernel_Source,
         Name     => [others => ASCII.NUL],
         Kind     => Origin'Enum_Rep (Kernel_Line),
         Severity => Level'Enum_Rep (Line_Level (Text)),
         Flags    => (if Truncated or else Text'Length > Text_Bytes then Truncated_Flag else 0),
         Readers  => Audience'Enum_Rep (Everyone),
         Length   => Unsigned_16 (Kept),
         Spare    => 0,
         Fault    => (others => <>),
         Text     => [others => ASCII.NUL]);
   begin
      for I in 1 .. Kept loop
         Result.Text (I) := Cleaned (Text (Text'First + (I - 1)));
         pragma Loop_Invariant
           (for all J in 1 .. I => Printable (Result.Text (J)));
         pragma Loop_Invariant
           (Result.Sequence = Writing and then Result.Length = Unsigned_16 (Kept)
            and then Result.Source = Kernel_Source
            and then Result.Kind = Origin'Enum_Rep (Kernel_Line));
      end loop;
      return Result;
   end Make_Line;

   function Make_Stop
     (Time_Ms : Unsigned_64;
      Source  : Unsigned_64;
      Name    : Name_Text;
      Fault   : Fault_Reports.Report;
      Readers : Audience) return Log_Record is
     ((Sequence => Writing,
       Time_Ms  => Time_Ms,
       Source   => Source,
       Name     => Name,
       Kind     => Origin'Enum_Rep (Process_Stopped),
       Severity => Level'Enum_Rep (Error),
       Flags    => 0,
       Readers  => Audience'Enum_Rep (Readers),
       Length   => 0,
       Spare    => 0,
       Fault    => (if Readers = Everyone then Fault_Reports.Summary_Of (Fault) else Fault),
       Text     => [others => ASCII.NUL]));

   function Text_Of (Item : Log_Record) return String is
      Used   : constant Natural := Used_Length (Item);
      Result : String (1 .. Used) := [others => ' '];
   begin
      for I in Result'Range loop
         Result (I) := Cleaned (Item.Text (I));
         pragma Loop_Invariant (for all J in 1 .. I => Printable (Result (J)));
      end loop;
      return Result;
   end Text_Of;

   function Name_Length (Item : Log_Record) return Natural is
      Last : Natural := 0;
   begin
      for I in Item.Name'Range loop
         exit when Item.Name (I) = ASCII.NUL;
         if Item.Name (I) /= ' ' then
            Last := I;
         end if;
         pragma Loop_Invariant (Last <= I);
      end loop;
      return Last;
   end Name_Length;

   function Name_Of (Item : Log_Record) return String is
      Last   : constant Natural := Name_Length (Item);
      Result : String (1 .. Last) := [others => ' '];
   begin
      for I in Result'Range loop
         Result (I) := Cleaned (Item.Name (I));
         pragma Loop_Invariant (for all J in 1 .. I => Printable (Result (J)));
      end loop;
      return Result;
   end Name_Of;
end Kernel_Log_Publication;
