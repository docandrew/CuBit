package ACPI_Native_Startup_Timing is
 package P is type Kind is (AML_Sleep, AML_Stall); end P;
 type Token is new Natural;
 procedure Enter (Item : P.Kind; Handle : out Token);
 procedure Leave (Handle : in out Token);
end ACPI_Native_Startup_Timing;
