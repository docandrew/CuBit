------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Which register windows a driver launched for an admitted ACPI device
--  gets (docs/device-matching.md "ACPI launch").
--
--  @description
--  An ACPI-matched driver's device-memory request names a span by its
--  index: the Nth memory span devmgr admitted for the bound device, in
--  _CRS order, just as a PCI driver's index names a BAR. Addresses come
--  only from devmgr's admission record (Admitted); the manifest says which
--  span and at most how many bytes. Plan decides the whole launch at once:
--  every device-memory request is met, or none is (Refused, with the
--  first failing request), so a driver never runs with part of what its
--  manifest declares.
--
--  Window K is the K-th device-memory request of the manifest: the start
--  of its span and at most the request's max_bytes of it, read-only when
--  the request asks so. A read-write request for a span _CRS declares
--  read-only is refused.
--
--  An ACPI device's one interrupt request is its parent line: a platform
--  line at resource 0 (the first interrupt descriptor of _CRS, the one
--  the GPIO bootstrap observation carries), one vector. Plan says the
--  manifest asks it (Parent_Line); whether the device has a routable line
--  is the caller's to decide, and an absent line leaves the binding empty.
--
--  Pure and SPARK; the caller derives and gives the capabilities.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CCL.Resource_Sections;

package Devmgr_ACPI_Windows with Pure, SPARK_Mode is
   package RS renames CCL.Resource_Sections;
   use type RS.Basis_Kind;
   use type RS.Resource_Kind;
   use type RS.Rights_Kind;
   use type RS.Interrupt_Mode;

   Max_Spans : constant := RS.MAX_ACPI_SPANS;
   subtype Span_Count is Natural range 0 .. Max_Spans;
   subtype Span_Index is Positive range 1 .. Max_Spans;

   --  One admitted span: whole pages [Base, Base + Length).
   type Span is record
      Base, Length : Unsigned_64 := 0;
      Writable     : Boolean := False;
   end record;
   type Span_Array is array (Span_Index) of Span;

   --  The spans devmgr admitted for one device, in _CRS order.
   type Admitted is record
      Count : Span_Count := 0;
      Items : Span_Array := [others => (others => <>)];
   end record;

   function Valid_Span (S : Span) return Boolean is
     (S.Length > 0 and then S.Base <= Unsigned_64'Last - S.Length);
   function Valid_Admitted (A : Admitted) return Boolean is
     (for all K in 1 .. A.Count => Valid_Span (A.Items (K)));

   type Window is record
      First, Bytes : Unsigned_64 := 0;
      Read_Only    : Boolean := True;
      From_Span    : Span_Index := Span_Index'First;
   end record;
   subtype Window_Count is Natural range 0 .. Max_Spans;
   type Window_Array is array (Span_Index) of Window;

   type Outcome is
     (Granted,             --  every device-memory request is met
      Not_ACPI_Resources,  --  the resources are relative to a PCI function or platform device
      Not_Admitted,        --  a request names a span past the admitted count
      Span_Read_Only,      --  a read-write request for a read-only span
      Too_Many_Windows,    --  more device-memory requests than admitted spans can name
      Not_Parent_Line,     --  an interrupt request other than one parent line
      Not_Device_Memory);  --  a device resource other than device memory or the parent line

   --  The one interrupt request an ACPI-matched driver may make.
   Parent_Line_Index   : constant := RS.ACPI_PARENT_LINE_INDEX;
   Parent_Line_Vectors : constant := 1;
   function Is_Parent_Line (R : RS.Resource) return Boolean is
     (R.Kind = RS.Interrupt
      and then R.Extra = Unsigned_64 (RS.Interrupt_Mode'Enum_Rep (RS.Platform_Line))
      and then R.Index = Parent_Line_Index
      and then R.Amount = Parent_Line_Vectors);

   type Plan_Result is record
      Status  : Outcome := Not_ACPI_Resources;
      --  The first failing .cubit.resources entry (1-based), or 0.
      Request : RS.Entry_Count := 0;
      Total   : Window_Count := 0;
      Windows : Window_Array := [others => (others => <>)];
      --  The manifest asks the device's parent line (Is_Parent_Line).
      Parent_Line : Boolean := False;
   end record;

   --  Window W inside span S, at its start, at most Limit bytes.
   function Fits (W : Window; S : Span; Limit : Unsigned_64) return Boolean is
     (W.First = S.Base and then W.Bytes in 1 .. S.Length and then W.Bytes <= Limit
      and then (W.Read_Only or else S.Writable));

   function Plan (Device : Admitted; Resources : RS.Section_Plan) return Plan_Result
   with Pre  => Valid_Admitted (Device),
        Post =>
          (if Plan'Result.Status = Granted then
             Plan'Result.Request = 0
             and then (for all K in 1 .. Plan'Result.Total =>
                         Plan'Result.Windows (K).From_Span <= Device.Count
                         and then Fits (Plan'Result.Windows (K),
                                        Device.Items (Plan'Result.Windows (K).From_Span),
                                        RS.MAX_DEVICE_MEMORY_BYTES))
             and then Plan'Result.Parent_Line =
                        (for some I in 1 .. Resources.Count => Is_Parent_Line (Resources.Entries (I)))
           else Plan'Result.Total = 0 and then not Plan'Result.Parent_Line
                and then (if Plan'Result.Status /= Not_ACPI_Resources
                          then Plan'Result.Request in 1 .. Resources.Count));

   function Outcome_Name (O : Outcome) return String is
     (case O is
        when Granted            => "granted",
        when Not_ACPI_Resources => "its resources are not relative to an ACPI device",
        when Not_Admitted       => "a device-memory request names a span that was not admitted",
        when Span_Read_Only     => "a read-write request names a span _CRS declares read-only",
        when Too_Many_Windows   => "more device-memory requests than admitted spans",
        when Not_Parent_Line    =>
           "an ACPI device's one interrupt request is its parent line (platform line 0, one vector)",
        when Not_Device_Memory  => "an ACPI device is given device memory and its parent line only");

end Devmgr_ACPI_Windows;
