package body ACPI_Native_HID_Detection is procedure Run(Server:in out ACPI_Requests.State;Result:out Report;Limits:Policy:=(others=><>)) is pragma Unreferenced(Server,Result,Limits);begin raise Program_Error with "unexpected discovery invocation";end Run;   procedure Run_With_Policy (Server : in out ACPI_Requests.State;
      Result : out Report; Limits : Policy := (others => <>)) is
      pragma Unreferenced (Server, Result, Limits, Check_Candidate);
   begin
      raise Program_Error with "unexpected filtered discovery invocation in report model";
   end Run_With_Policy;
   function Name (Status : Outcome) return String is
      pragma Unreferenced (Status);
   begin raise Program_Error with "unexpected discovery name invocation"; return ""; end Name;
   function Step_Name (Step : Progress_Step) return String is
      pragma Unreferenced (Step);
   begin raise Program_Error with "unexpected discovery step invocation"; return ""; end Step_Name;
   function Error_Name (Kind : Error_Kind) return String is
      pragma Unreferenced (Kind);
   begin raise Program_Error with "unexpected discovery error invocation"; return ""; end Error_Name;
end ACPI_Native_HID_Detection;