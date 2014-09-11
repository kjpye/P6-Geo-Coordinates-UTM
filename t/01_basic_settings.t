use v6;
use Test;

plan 3;

BEGIN { @*INC.push('../lib'); }
use Geo::Coordinates::UTM;

ok True, "Module loaded";

my (Str $zone, Real $east, Real $north);

ok ($zone,$east,$north)=latlon_to_utm('WGS-84',57.833055556,-2.788951667), "latlon_to_utm available";

ok utm_to_latlon('WGS-84',$zone,$east,$north), "utm_to_latlon available";
