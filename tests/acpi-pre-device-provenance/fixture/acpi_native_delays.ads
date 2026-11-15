-- Hosted test model only; never a native runtime/provider implementation.
with Interfaces; package ACPI_Native_Delays is
 procedure Test_Start; procedure Assert_Closed;
 procedure Pause_Startup(Accepted : out Boolean); procedure Resume_Startup(Accepted : out Boolean);
 procedure Inspect_Paused_Startup(Deadline : out Interfaces.Unsigned_64; Accepted : out Boolean);
end ACPI_Native_Delays;