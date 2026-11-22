with Desktop_Breadcrumbs;
package Desktop_Startup_Progress is
 subtype Stage is Desktop_Breadcrumbs.Stage;
 procedure Enter(Point : Stage); function Current return Stage;
end Desktop_Startup_Progress;
