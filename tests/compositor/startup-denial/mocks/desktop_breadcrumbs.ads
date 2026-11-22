package Desktop_Breadcrumbs is
 type Stage is (Entered, Init_Before, Init_After, Health_Before, Health_After, Targets_Before, Targets_After, Pipeline_Before, Pipeline_After, Upload_Before, Upload_After, Readback_Before, Readback_After, Selected, Begin_Before, Begin_After, Deferred, Pending, Unsafe, Complete, Submit, Released);
 procedure Mark(Point : Stage); end Desktop_Breadcrumbs;
