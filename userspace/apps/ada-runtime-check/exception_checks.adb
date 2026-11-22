pragma Ada_2022;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Finalization;
with Check_Results; use Check_Results;
with Events; use Events;

package body Exception_Checks is

   Custom_Error : exception;

   --  A value the compiler cannot see through, so checks stay run-time.
   Ten : Integer := 10 with Volatile;

   --  Raise and handle across subprograms --------------------------------

   Depth_Reached : Natural := 0;

   procedure Level_3 is
   begin
      Depth_Reached := 3;
      raise Custom_Error with "raised three calls down";
   end Level_3;

   procedure Level_2 is
   begin
      Depth_Reached := 2;
      Level_3;
      Depth_Reached := 20;  --  never: the exception passes through
   end Level_2;

   procedure Level_1 is
   begin
      Depth_Reached := 1;
      Level_2;
      Depth_Reached := 10;
   end Level_1;
   pragma No_Inline (Level_1);
   pragma No_Inline (Level_2);
   pragma No_Inline (Level_3);

   procedure Across_Subprograms is
   begin
      Level_1;
      Check (False, "exception raised three calls down reaches the caller's handler");
   exception
      when E : Custom_Error =>
         Check (Depth_Reached = 3 and then Exception_Message (E) = "raised three calls down",
                "exception raised three calls down reaches the caller's handler");
         Check (Exception_Name (E) = "EXCEPTION_CHECKS.CUSTOM_ERROR",
                "Exception_Name (" & Exception_Name (E) & ")");
         Check (Exception_Identity (E) = Custom_Error'Identity, "Exception_Identity");
   end Across_Subprograms;

   --  A run-time check -----------------------------------------------------

   type Row is array (1 .. 10) of Integer;

   function Element (R : Row; Index : Integer) return Integer is (R (Index));
   pragma No_Inline (Element);

   procedure Run_Time_Check is
      R : constant Row := [others => 7];
      Got : Integer := 0;
   begin
      Got := Element (R, Ten + 1);
      Check (False, "index check raises Constraint_Error (no value " & Got'Image & ")");
   exception
      when E : Constraint_Error =>
         Check (Got = 0, "index check raises Constraint_Error, caught by the caller");
         Note ("Constraint_Error message: " & Exception_Message (E));
         Check (Exception_Message (E)'Length > 0, "the check's message names where it failed");
   end Run_Time_Check;

   procedure Overflow_Check is
      Big : Integer := Integer'Last - Ten + 10 with Volatile;
   begin
      Big := Big + Ten;
      Check (False, "overflow check raises Constraint_Error");
   exception
      when Constraint_Error =>
         Check (Big = Integer'Last, "overflow check raises Constraint_Error");
   end Overflow_Check;

   --  Finalization during propagation ------------------------------------

   procedure Raise_With_Controlled is
      A : Tracked := (Ada.Finalization.Controlled with Id => 1);
      B : Tracked := (Ada.Finalization.Controlled with Id => 2);
      pragma Unreferenced (A, B);
   begin
      raise Custom_Error with "leaving with controlled objects";
   end Raise_With_Controlled;
   pragma No_Inline (Raise_With_Controlled);

   procedure Middle_With_Controlled is
      R : Resource;
      pragma Unreferenced (R);
   begin
      Raise_With_Controlled;
   end Middle_With_Controlled;
   pragma No_Inline (Middle_With_Controlled);

   procedure Finalization_During_Propagation is
   begin
      Clear;
      Middle_With_Controlled;
      Check (False, "controlled objects finalized while the exception propagates");
   exception
      when Custom_Error =>
         --  R initialized (I0); A and B built from aggregates; on the way
         --  out B, A (the inner frame, reverse order), then R.
         Check (Trace = "I0F2F1F0",
                "controlled objects finalized while the exception propagates (" & Trace & ")");
   end Finalization_During_Propagation;

   --  Re-raise and saved occurrences -------------------------------------

   procedure Reraise_Inner is
   begin
      raise Custom_Error with "first";
   exception
      when Custom_Error =>
         raise;
   end Reraise_Inner;
   pragma No_Inline (Reraise_Inner);

   procedure Reraise is
      Saved : Exception_Occurrence;
   begin
      begin
         Reraise_Inner;
      exception
         when E : others =>
            Save_Occurrence (Saved, E);
      end;
      Check (Exception_Identity (Saved) = Custom_Error'Identity
             and then Exception_Message (Saved) = "first",
             "raise; keeps the occurrence; Save_Occurrence copies it");
      begin
         Reraise_Occurrence (Saved);
         Check (False, "Reraise_Occurrence raises the saved occurrence");
      exception
         when E : Custom_Error =>
            Check (Exception_Message (E) = "first", "Reraise_Occurrence raises the saved occurrence");
      end;
   end Reraise;

   procedure Others_And_Nesting is
      Outer_Seen, Inner_Seen : Boolean := False;
   begin
      begin
         begin
            raise Program_Error;
         exception
            when Constraint_Error =>
               Inner_Seen := True;  --  not this one
         end;
      exception
         when others =>
            Outer_Seen := True;
      end;
      Check (Outer_Seen and then not Inner_Seen, "a handler for another exception is passed over; others takes it");
   end Others_And_Nesting;

   --  An exception from Finalize ------------------------------------------

   type Failing is new Ada.Finalization.Controlled with record
      Fail : Boolean := False;
   end record;
   overriding procedure Finalize (F : in out Failing);

   Failing_Finalized : Natural := 0;

   overriding procedure Finalize (F : in out Failing) is
   begin
      Failing_Finalized := Failing_Finalized + 1;
      if F.Fail then
         raise Custom_Error with "from Finalize";
      end if;
   end Finalize;

   procedure Failing_Collection is
   begin
      Failing_Finalized := 0;
      declare
         type Failing_Access is access Failing;
         First : constant Failing_Access := new Failing'(Ada.Finalization.Controlled with Fail => False);
         Second : constant Failing_Access := new Failing'(Ada.Finalization.Controlled with Fail => True);
         Third : constant Failing_Access := new Failing'(Ada.Finalization.Controlled with Fail => False);
         pragma Unreferenced (First, Second, Third);
      begin
         null;
      end;
      Check (False, "Finalize raising: the collection finishes, then Program_Error");
   exception
      when E : Program_Error =>
         Note ("Program_Error message: " & Exception_Message (E));
         Check (Failing_Finalized = 3, "Finalize raising: the collection finishes, then Program_Error");
   end Failing_Collection;

   procedure Run is
   begin
      Across_Subprograms;
      Run_Time_Check;
      Overflow_Check;
      Finalization_During_Propagation;
      Reraise;
      Others_And_Nesting;
      Failing_Collection;
   end Run;

end Exception_Checks;
