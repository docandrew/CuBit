------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Desktop's launch notice state (see the spec).
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Launch_Failure_Text;

package body Desktop_Launch_Notice with SPARK_Mode is

   --  Name, cut to Maximum_Name characters.
   procedure Copy_Name
     (Name : String; Into : out Name_Text; Length : out Name_Length)
   is
   begin
      Into := [others => ' '];
      Length := 0;
      for C of Name loop
         exit when Length = Maximum_Name;
         Length := Length + 1;
         Into (Length) := C;
      end loop;
   end Copy_Name;

   procedure Show_Refusal
     (N : out Notice; Name : String;
      Failure : CuBit.Launch_Arguments.Launch_Failure)
   is
   begin
      N := (Open => True, Why => Refused, Failure => Failure, others => <>);
      Copy_Name (Name, N.Name, N.Length);
   end Show_Refusal;

   procedure Dismiss (N : in out Notice) is
   begin
      N.Open := False;
   end Dismiss;

   function Title (N : Notice) return String is
     (N.Name (1 .. N.Length) &
      (case N.Why is
         when Refused     => " could not start",
         when Ended_Early => " stopped while starting"));

   function Detail (N : Notice) return String is
     (case N.Why is
        when Refused =>
          "Reason: " & CuBit.Launch_Failure_Text.Explanation (N.Failure) & ".",
        when Ended_Early =>
          (case N.Ending.Kind is
             when CuBit.Child_Exits.Stopped =>
               "It was stopped (crashed or killed) before it opened a window.",
             when CuBit.Child_Exits.Exited =>
               "It exited with code" & N.Ending.Code'Image &
               " before it opened a window."));

   procedure Watch
     (T : in out Watch_Table; Process : CuBit.Process_IDs.Process_ID;
      Name : String)
   is
      Slot : Watch_Index := T.Next;
   begin
      for K in Watch_Index loop
         if not T.Slots (K).Used then
            Slot := K;
            exit;
         end if;
      end loop;
      T.Slots (Slot).Used := True;
      T.Slots (Slot).Process := Process;
      Copy_Name (Name, T.Slots (Slot).Name, T.Slots (Slot).Length);
      T.Next := (if T.Next = Watch_Index'Last then Watch_Index'First else T.Next + 1);
   end Watch;

   procedure Window_Opened
     (T : in out Watch_Table; Process : CuBit.Process_IDs.Process_ID) is
   begin
      for Item of T.Slots loop
         if Item.Used and then Item.Process = Process then
            Item := (others => <>);
         end if;
      end loop;
   end Window_Opened;

   procedure Ended
     (T : in out Watch_Table; N : in out Notice;
      Ending : CuBit.Child_Exits.Report; Shown : out Boolean)
   is
   begin
      Shown := False;
      for Item of T.Slots loop
         if Item.Used and then Item.Process = Ending.Process then
            if Failed (Ending) then
               N := (Open => True, Name => Item.Name, Length => Item.Length,
                     Why => Ended_Early, Ending => Ending, others => <>);
               Shown := True;
            end if;
            Item := (others => <>);
         end if;
      end loop;
   end Ended;

end Desktop_Launch_Notice;
