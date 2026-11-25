------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Desktop's launch notice state (see the spec).
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Launch_Failure_Text;
with Desktop_Launch_Request;

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
      N := (Why => Refused, Failure => Failure, others => <>);
      Copy_Name (Name, N.Name, N.Length);
   end Show_Refusal;

   procedure Show_Problem
     (N : out Notice; Name : String; Why : Problem; Other : String := "")
   is
   begin
      N := (Why => Why, others => <>);
      Copy_Name (Name, N.Name, N.Length);
      Copy_Name (Other, N.Other, N.Other_Length);
   end Show_Problem;

   --  Seconds Desktop waits for procmgr's answer, in words.
   Limit_Seconds : constant String :=
     Natural'Image (Desktop_Launch_Request.Answer_Limit_Ms / 1_000);

   function Title (N : Notice) return String is
     (Name_Of (N) &
      (case N.Why is
         when Refused | Not_Sent    => " could not start",
         when Ended_Early           => " stopped while starting",
         when Still_Starting        => " did not start",
         when Unanswered | Unreadable => " may not have started"));

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
               " before it opened a window."),
        when Still_Starting =>
          (if N.Other_Length = 0 then "Another app is" else N.Other (1 .. N.Other_Length) & " is") &
          " still starting. Try again in a moment.",
        when Not_Sent =>
          "Reason: the Desktop could not send the launch request to procmgr.",
        when Unanswered =>
          "procmgr did not answer within" & Limit_Seconds &
          " seconds. The Desktop stopped waiting; you can launch apps again.",
        when Unreadable =>
          "The Desktop could not read procmgr's answer. You can launch apps again.");

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
               N := (Name => Item.Name, Length => Item.Length,
                     Why => Ended_Early, Ending => Ending, others => <>);
               Shown := True;
            end if;
            Item := (others => <>);
         end if;
      end loop;
   end Ended;

end Desktop_Launch_Notice;
